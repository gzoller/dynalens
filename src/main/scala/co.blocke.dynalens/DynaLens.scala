/*
 * Copyright (c) 2025 Greg Zoller
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package co.blocke.dynalens

import zio.*

import scala.quoted.*
import co.blocke.scala_reflection.*
import co.blocke.scala_reflection.reflect.*
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.TypedName
import Path.*


case class DynaLens[T](
                        _update: (String, Any, T) => ZIO[Any, DynaLensError, T],
                        _get: (String, T) => ZIO[Any, DynaLensError, Any],
                        _registry: Map[String, DynaLens[?]],
                        _typeName: String,
                        _typeInfo: Map[String,Any],
                        _elemIsOptional: Map[String, Boolean]   // per-field: Seq element is Option[_]?
):
  type ThisT = T

  // Run a compiled lens script
  inline def run(
      script: BlockStmt,
      target: T,
      registry: _BiMapRegistry = EmptyBiMapRegistry
  ): ZIO[Any, DynaLensError, (T, DynaContext)] =
    actualRun(script, target).provide(BiMapRegistry.layer(registry))

  private inline def actualRun(
      script: BlockStmt,
      target: T
  ): ZIO[_BiMapRegistry, DynaLensError, (T, DynaContext)] =
    val ctx: DynaContext = DynaContext(target, Some(this))
    for {
      resultCtx <- script.resolve(ctx)
      (resultObj, _) = resultCtx("top")
    } yield (resultObj.asInstanceOf[T], resultCtx)

  def runNoZIO(script: BlockStmt, target: T, registry: _BiMapRegistry = EmptyBiMapRegistry): Either[DynaLensError, (T, DynaContext)] =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe
        .run(
          run(script, target, registry).either
        )
        .getOrThrow()
    }

  def get(path: String, obj: T): ZIO[Any, DynaLensError, Any] =
    _getValue( parsePath(path), obj)

  private def _getValue(pathElements: List[PathElement], obj: T): ZIO[Any, DynaLensError, Any] = {

    def step(current: Any, currentLens: DynaLens[?], path: List[PathElement]): ZIO[Any, DynaLensError, Any] =
      path match {
        case Nil =>
          ZIO.succeed(current)

        case Field(f, isOptional) :: rest =>
          currentLens
            ._get(f, current.asInstanceOf[currentLens.ThisT])
            .flatMap {
              case None if isOptional =>
                // short-circuit if the field is optional and None
                ZIO.succeed(None)

              case None =>
                // fail if field missing and not optional
                ZIO.fail(DynaLensError(s"Field '$f' is missing or null"))

              case Some(value) =>
                currentLens._registry.get(f) match {
                  case Some(nextLens) =>
                    step(value, nextLens, rest)
                  case None =>
                    if rest.isEmpty then
                      if isOptional then ZIO.succeed(Some(value))
                        else ZIO.succeed(value)
                    else step(value, currentLens, rest)
                }

              case nonOptValue =>
                // In case the lens returns a raw value (not wrapped in Option)
                currentLens._registry.get(f) match {
                  case Some(nextLens) =>
                    step(nonOptValue, nextLens, rest)
                  case None =>
                    if rest.isEmpty then
                      if isOptional then ZIO.succeed(Some(nonOptValue))
                      else ZIO.succeed(nonOptValue)
                    else step(nonOptValue, currentLens, rest)
                }
            }

        case IndexedField(f, i, isOptional) :: rest =>
          currentLens
            ._get(f, current.asInstanceOf[currentLens.ThisT])
            .flatMap {
              case null | None =>
                if isOptional then
                  if i.isEmpty && rest == Nil then
                    ZIO.succeed(Nil) // missing optional list → Nil
                  else
                    ZIO.fail(DynaLensError(s"Cannot index into missing optional list field '$f'"))
                else
                  ZIO.fail(DynaLensError(s"Field '$f' is not a Seq"))

              case seq =>
                val listZio: ZIO[Any, DynaLensError, Seq[Any]] = seq match {
                  case s: Seq[?] => ZIO.succeed(s)
                  case Some(s: Seq[?]) if isOptional => ZIO.succeed(s)
                  case other =>
                    ZIO.fail(DynaLensError(s"Expected Seq at field '$f', but got: ${other.getClass.getSimpleName}"))
                }

                listZio.flatMap { list =>
                  if i.isEmpty && rest == Nil then
                    ZIO.succeed(list) // full list return
                  else
                    list.lift(i.get) match {
                      case Some(elem) =>
                        currentLens._registry.get(f) match {
                          case Some(elemLens) => step(elem, elemLens, rest)
                          case None =>
                            if rest.isEmpty then ZIO.succeed(elem)
                            else ZIO.fail(DynaLensError(s"No registry for '$f' to recurse into index"))
                        }
                      case None =>
                        ZIO.fail(DynaLensError(s"Index ${i.get} out of bounds for field '$f'"))
                    }
                }
            }
      }

    step(obj, this, pathElements)
  }

  def update(path: String, value: Any, obj: T): ZIO[Any, DynaLensError, T] =
    _updateValue(parsePath(path), value, obj)

  private def _updateValue(pathElements: List[PathElement], value: Any, obj: T): ZIO[Any, DynaLensError, T] = {

    def wrapIfNeeded(opt: Boolean, v: Any): Any =
      if opt then
        v match {
          case _: Option[?] => v
          case _ => Some(v)
        }
      else v

    def step(current: Any, currentLens: DynaLens[?], path: List[PathElement]): ZIO[Any, DynaLensError, Any] = path match {
      case Nil =>
        ZIO.fail(DynaLensError("Cannot update empty path"))

      case Field(f, isOptional) :: Nil =>
        val finalValue = wrapIfNeeded(isOptional, value)
        currentLens._update(f, finalValue, current.asInstanceOf[currentLens.ThisT])

      case Field(f, isOptional) :: rest =>
        for {
          nested <- currentLens._get(f, current.asInstanceOf[currentLens.ThisT])
          nextLens <- currentLens._registry.get(f) match
            case Some(a) => ZIO.succeed(a)
            case None => ZIO.fail(DynaLensError(s"No nested lens for field '$f'"))
          updatedNested <- step(nested, nextLens, rest)
          wrapped = wrapIfNeeded(isOptional, updatedNested)
          updated <- currentLens._update(f, wrapped, current.asInstanceOf[currentLens.ThisT])
        } yield updated

      case IndexedField(f, maybeI, isOptional) :: rest =>
        for {
          rawList <- currentLens._get(f, current.asInstanceOf[currentLens.ThisT])
          list <- rawList match
            case None | null =>
              if isOptional then ZIO.succeed(Nil)
              else ZIO.fail(DynaLensError(s"Field '$f' is not a Seq"))
            case l: Seq[?] => ZIO.succeed(l)
            case Some(wrapped: Seq[?]) => ZIO.succeed(wrapped)
            case x =>
              ZIO.fail(DynaLensError(s"Field '$f' is not a Seq"))

          updatedList <-
            maybeI match
              case Some(i) =>
                rest match
                  case Nil =>
                    // Case: foo[2] = value
                    list.lift(i) match
                      case Some(_) =>
                        val updatedValue =
                          currentLens._elemIsOptional.get(f).map(elemIsOpt => if elemIsOpt then Some(value) else value).getOrElse(value)
                        ZIO.succeed(list.updated(i, updatedValue))
                      case None =>
                        ZIO.fail(DynaLensError(s"Index $i out of bounds for field '$f'"))

                  case _ =>
                    for {
                      elem <- list.lift(i) match
                        case Some(e) => ZIO.succeed(e)
                        case None => ZIO.fail(DynaLensError(s"Index $i out of bounds for field '$f'"))
                      nestedLens <- currentLens._registry.get(f) match
                        case Some(lens) => ZIO.succeed(lens)
                        case None => ZIO.fail(DynaLensError(s"No nested lens for collection field '$f'"))
                      updatedElem <- step(elem, nestedLens, rest)
                    } yield list.updated(i, updatedElem)
              case None =>
                rest match
                  case Nil =>
                    // Case: foo[] = value — replace whole list
                    val finalValue =
                      if isOptional then
                        value match
                          case Nil => None   // convert Nil->None for list
                          case _ => value
                      else
                        value
                    ZIO.succeed(finalValue)

                  case _ =>
                    // Case: foo[].bar = ... — map over list
                    for {
                      nestedLens <- currentLens._registry.get(f) match
                        case Some(lens) => ZIO.succeed(lens)
                        case None => ZIO.fail(DynaLensError(s"No nested lens for collection field '$f'"))
                      updatedItems <- ZIO.foreach(list)(elem => step(elem, nestedLens, rest))
                    } yield updatedItems

          updated <- currentLens._update(f, updatedList, current.asInstanceOf[currentLens.ThisT])
        } yield updated
    }

    step(obj, this, pathElements).asInstanceOf[ZIO[Any, DynaLensError, T]]
  }

  private def walkPath(
                        path: List[PathElement],
                        current: Any,
                        dynalens: DynaLens[?]
                      ): ZIO[Any, DynaLensError, DynaContext] = {
    val ctx = DynaContext(current, Some(dynalens))

    def step(
              path: List[PathElement],
              value: Any,
              lens: DynaLens[?]
            ): ZIO[Any, DynaLensError, Unit] = path match {
      case Field(f, isOptional) :: rest =>
        for {
          v <- lens._get(f, value.asInstanceOf[lens.ThisT])
          nextLensOpt = lens._registry.get(f)
          _ <- nextLensOpt match
            case Some(nextLens) =>
              v match {
                case Some(real) =>
                  // Optional[Complex] — unwrap and insert Complex lens into ctx
                  ctx += (f -> (Some(real), Some(nextLens)))
                  step(rest, real, nextLens)
                case None =>
                  if isOptional then ZIO.unit
                  else ZIO.fail(DynaLensError(s"Field '$f' is missing or null"))
                case other =>
                  ctx += (f -> (Some(other), Some(nextLens)))
                  step(rest, other, nextLens)
              }

            case None =>
              step(rest, value, lens)
        } yield ()

      case IndexedField(f, _, isOptional) :: rest =>
        for {
          v <- lens._get(f, value.asInstanceOf[lens.ThisT])
          _ <- v match {
            case None | null =>
              if isOptional then ZIO.unit
              else ZIO.fail(DynaLensError(s"Expected non-optional list at '$f'"))

            case Some(seq: Seq[?]) =>
              // Multiple children — loop
              val listZIO = ZIO.foreach(seq) { elem =>
                lens._registry.get(f) match
                  case Some(loopLens) =>
                    step(rest, elem, loopLens)
                  case None =>
                    ZIO.unit
              }
              ctx += (f -> (None, lens._registry.get(f)))
              listZIO.unit

            case seq: Seq[?] =>
              val listZIO = ZIO.foreach(seq) { elem =>
                lens._registry.get(f) match
                  case Some(loopLens) =>
                    step(rest, elem, loopLens)
                  case None =>
                    ZIO.unit
              }
              ctx += (f -> (None, lens._registry.get(f)))
              listZIO.unit

            case other =>
              ZIO.fail(DynaLensError(s"Expected sequence at '$f', got ${other.getClass.getSimpleName}"))
          }
        } yield ()

      case Nil =>
        ZIO.unit
    }

    step(path, current, dynalens).as(ctx)
  }

  // Split path at Iterables to create sub-paths
  private def splitIntoLevels(path: List[PathElement]): List[List[PathElement]] = {
    val (levels, current) = path.foldLeft(List.empty[List[PathElement]] -> List.empty[PathElement]) {
      case ((acc, current), pe @ IndexedField(_, None, _)) =>
        (acc :+ (current :+ pe)) -> Nil
      case ((acc, current), pe) =>
        acc -> (current :+ pe)
    }

    (levels :+ current).filter(_.nonEmpty)
  }

  def map[R](
      path: String,
      fn: Fn[R],
      obj: T,
      outerCtx: DynaContext = DynaContext.empty // <-- added outer context
  ): ZIO[_BiMapRegistry, DynaLensError, T] =

    def processPaths(
        paths: List[List[PathElement]],
        refObj: Any,
        dynalens: Option[DynaLens[?]],
        ctx: DynaContext
    ): ZIO[_BiMapRegistry, DynaLensError, Any] =
      dynalens
        .map(lens =>
          paths match {
            /*
            case pathParts :: Nil =>
              val partialPath = Path.partialPath(pathParts)
              for {
                in <- lens._getValue(pathParts, refObj.asInstanceOf[lens.ThisT])
                maybeLens = pathParts.last match {
                  case IndexedField(p, _, _) => lens._registry.get(p).orElse(None)
                  case Field(p, _)           => None
                }
                _ = ctx.put("this", (in, maybeLens)) // assign loop param variable
                enrichedCtx = outerCtx ++ ctx.toMap // <-- merge loop context with outer context
                out <- fn.resolve(enrichedCtx)
                updated <- lens.update(partialPath, out, refObj.asInstanceOf[lens.ThisT])
              } yield updated
             */
            case pathParts :: Nil =>
              // --- inside processPaths, base case: case pathParts :: Nil => ---
              val partialPath = Path.partialPath(pathParts)
              for {
                in <- lens._getValue(pathParts, refObj.asInstanceOf[lens.ThisT])

                maybeLeafLens =
                  pathParts.last match {
                    case IndexedField(p, _, _) => lens._registry.get(p).orElse(None)
                    case Field(_, _)           => None
                  }

                _ = println(s"[map.base] path=$path  partial=$partialPath  leaf(in)=${Option(in).map(_.getClass.getSimpleName).getOrElse("null")}  value=$in")

                // bind this = leaf value
                leafCtx = ctx.updatedWith("this", (in, maybeLeafLens))

                // show what we're actually putting into ctx
                _ = {
                  val hasThis = leafCtx.contains("this")
                  val lensTag = maybeLeafLens.map(_.getClass.getSimpleName).getOrElse("None")
                  val keys    = leafCtx.keys.mkString(",")
                  println(s"[map.base] bind this => value=$in lens=$lensTag  ctxKeys=[$keys]  hasThis=$hasThis")
                }

                enrichedCtx = outerCtx ++ leafCtx.toMap

                // show merged outer + leaf ctx
                _ = {
                  val keys = enrichedCtx.keys.mkString(",")
                  println(s"[map.base] enrichedCtx keys=[$keys]  hasThis=${enrichedCtx.contains("this")}")
                }

                out <- {
                  println(s"[map.base] resolving RHS fn=${fn.getClass.getSimpleName} with this=${in}")
                  fn.resolve(enrichedCtx)
                }

                _ = println(s"[map.base] RHS result=$out (${Option(out).map(_.getClass.getSimpleName).getOrElse("null")})")

                updated <- lens.update(partialPath, out, refObj.asInstanceOf[lens.ThisT])
              } yield updated

            case pathParts :: rest =>
              val partialPath = Path.partialPath(pathParts)
              val loopKey = pathParts.last.name
              for {
                listVal <- lens._getValue(pathParts, refObj.asInstanceOf[lens.ThisT])
                iterable <- ZIO.fromEither(listVal match {
                  case i: Iterable[?]       => Right(i)
                  case Some(i: Iterable[?]) => Right(i)
                  case None                 => Right(Nil)
                  case other =>
                    Left(DynaLensError(s"Expected iterable at path '$partialPath', but found: ${other.getClass.getName}"))
                })

                maybeLensForList <- ctx.get(loopKey) match {
                  case Some((_, existingLens)) => ZIO.succeed(existingLens)
                  case None if iterable.nonEmpty =>
                    ZIO.fail(DynaLensError(s"No existing lens found in ctx for key: $loopKey"))
                  case None => ZIO.succeed(None)
                }
                updatedIterable <- ZIO.foreach(iterable) { item =>
                  ctx.update(loopKey, (item, maybeLensForList)) // update ctx with current item
                  processPaths(rest, item, maybeLensForList, ctx) // recurse
                }
                updatedRefObj <- lens._updateValue(pathParts, updatedIterable, refObj.asInstanceOf[lens.ThisT])
              } yield updatedRefObj

            case Nil =>
              ZIO.fail(DynaLensError("Should Never Happen(tm)"))
          }
        )
        .orNull

    val parsed = parsePath(path)
    for {
      ctx <- walkPath(parsed, obj, this)
      splitPaths = splitIntoLevels(parsed)
      updated <- processPaths(splitPaths, obj, Some(this), ctx)
    } yield updated.asInstanceOf[T]


object DynaLens:

  inline def dynalens[T]: DynaLens[T] = ${ generateDynaLensImpl[T] }

  private def generateDynaLensImpl[T: Type](using Quotes): Expr[DynaLens[T]] =
    import quotes.reflect.*

    ReflectOnType[T](quotes)(TypeRepr.of[T])(using scala.collection.mutable.Map.empty[TypedName, Boolean]) match {
      case s: ScalaClassRef[T] if s.isCaseClass =>
        val getLambdaExpr = generateGetLambda[T](quotes, s.fields)
        val updateLambdaExpr = generateUpdateLambda[T](quotes, s.fields)

        // Build recursive registry of DynaLens for any fields of this class which are also case classes
        val pairs: List[Expr[(String, DynaLens[?])]] = s.fields.flatMap { field =>
          field.fieldRef match {
            case c: ScalaClassRef[?] if c.isCaseClass =>
              c.refType match
                case '[t] =>
                  val lensExpr = generateDynaLensImpl[t] // recursive call
                  val keyExpr = Expr(field.name)
                  Some('{ $keyExpr -> $lensExpr })
            case c: SeqRef[?] =>
              c.elementRef match
                case d: ScalaClassRef[?] if d.isCaseClass =>
                  d.refType match
                    case '[t] =>
                      val lensExpr = generateDynaLensImpl[t]
                      val keyExpr = Expr(field.name)
                      Some('{ $keyExpr -> $lensExpr })
                case _ => None
            // For Option[Seq] if option element type is a class make sure we put lens in registry    
            case c: OptionRef[?] if c.optionParamType.isInstanceOf[SeqRef[?]] =>
              c.optionParamType match
                case s: SeqRef[?] =>
                  s.elementRef match
                    case d: ScalaClassRef[?] if d.isCaseClass =>
                      d.refType match
                        case '[t] =>
                          val lensExpr = generateDynaLensImpl[t]
                          val keyExpr = Expr(field.name)
                          Some('{ $keyExpr -> $lensExpr })
                    case _ => None
                case _ => None
            case _ => None
          }
        }
        val registryExpr: Expr[Map[String, DynaLens[?]]] = {
          val listExpr: Expr[List[(String, DynaLens[?])]] = Expr.ofList(pairs)
          '{ Map.from[String, DynaLens[?]]($listExpr) }
        }

        val elemOptPairs =
          s.fields.map { f =>
            val isElemOpt = f.fieldRef match
              case s: SeqRef[?] => s.elementRef.isInstanceOf[OptionRef[?]]
              case _ => false
            f.name -> isElemOpt
          }
        val elemIsOptionalExpr: Expr[Map[String, Boolean]] =
          liftMapBoolean( elemOptPairs.toMap )

        val typeNameExpr = Expr(s.typedName.toString)
        val typeInfoExpr = liftTypeInfo( buildPathTree(s) )

        '{ DynaLens[T]($updateLambdaExpr, $getLambdaExpr, $registryExpr, $typeNameExpr, $typeInfoExpr, $elemIsOptionalExpr) }

      case x => throw new Exception(s"Sorry, dynalens only supports Scala case classes but received ${x.name}")
    }

  private def generateGetLambda[T: Type](quotes: Quotes, classFields: List[FieldInfoRef]): Expr[(String, T) => ZIO[Any, DynaLensError, Any]] =
    import quotes.reflect.*
    given Quotes = quotes

    val tpe = TypeRepr.of[T]

    Lambda(
      Symbol.spliceOwner,
      MethodType(List("field", "target"))(
        _ => List(TypeRepr.of[String], tpe),
        _ => TypeRepr.of[ZIO[Any, DynaLensError, Any]]
      ),
      (_, params) => {
        val fieldParam = params(0).asInstanceOf[Term]
        val targetParam = params(1).asInstanceOf[Term]

        val matchExpr = Match(
          fieldParam,
          classFields.map { f =>
            val fieldName = f.name
            val fieldAccess = Select.unique(targetParam, fieldName)
            CaseDef(
              Literal(StringConstant(fieldName)),
              None,
              '{ ZIO.succeed(${ fieldAccess.asExpr }) }.asTerm
            )
          } :+ CaseDef(
            Wildcard(),
            None,
            '{ ZIO.fail(DynaLensError("Field not found: " + ${ fieldParam.asExprOf[String] })) }.asTerm
          )
        )

        matchExpr
      }
    ).asExprOf[(String, T) => ZIO[Any, DynaLensError, Any]]

  private def generateUpdateLambda[T: Type](quotes: Quotes, classFields: List[FieldInfoRef]): Expr[(String, Any, T) => ZIO[Any, DynaLensError, T]] =
    import quotes.reflect.*
    given Quotes = quotes

    val tpe = TypeRepr.of[T]
    val sym = tpe.typeSymbol
    val fields = sym.caseFields

    // Define parameter symbols: (fieldName: String, value: Any, target: T)
    val methodSym = Symbol.spliceOwner
    val paramNames = List("field", "value", "target")
    val paramTypes = List(TypeRepr.of[String], TypeRepr.of[Any], tpe)

    val methodType = MethodType(paramNames)(_ => paramTypes, _ => TypeRepr.of[ZIO[Any, DynaLensError, T]])

    Lambda(
      methodSym,
      methodType,
      (_, params) => {
        val fieldParam = params(0).asInstanceOf[Term] // field: String
        val valueParam = params(1).asInstanceOf[Term] // value: Any
        val targetParam = params(2).asInstanceOf[Term] // target: T

        // Build cases
        val cases = classFields.map { field =>
          val name = field.name
          val fieldType = field.fieldRef.refType

          val updatedValue: Term =
            field.fieldRef match
              case opt: ScalaOptionRef[?] =>
                val innerRTypeRef = opt.optionParamType
                innerRTypeRef.refType match
                  case '[innerT] =>
                    val optionType = opt.refType
                    val valueExpr = valueParam.asExprOf[Any]

                    val wrappedExpr: Expr[Option[innerT]] = '{
                      val v = $valueExpr
                      v match {
                        case o: Option[?] => o.asInstanceOf[Option[innerT]]
                        case null         => None
                        case other        => Some(other.asInstanceOf[innerT])
                      }
                    }

                    wrappedExpr.asExprOf(using optionType).asTerm

              case _ =>
                TypeApply(
                  Select.unique(valueParam, "asInstanceOf"),
                  List(TypeTree.of(using fieldType))
                ).asExpr.asTerm

          val copyArgs = fields.map { f =>
            if f.name == name then NamedArg(f.name, updatedValue)
            else NamedArg(f.name, Select.unique(targetParam, f.name))
          }

          val updatedExpr = Apply(Select.unique(targetParam, "copy"), copyArgs)

          CaseDef(
            Literal(StringConstant(name)),
            None,
            '{ ZIO.succeed(${ updatedExpr.asExprOf[T] }) }.asTerm
          )
        }
        val fallback = CaseDef(Wildcard(), None, '{ ZIO.fail(DynaLensError("Field not found: " + ${ fieldParam.asExprOf[String] })) }.asTerm)
        Match(fieldParam, cases :+ fallback)
      }
    ).asExprOf[(String, Any, T) => ZIO[Any, DynaLensError, T]]


  private def buildPathTree(r: RTypeRef[?]): Map[String, Any] = r match {
    case c: ScalaClassRef[?] =>
      c.fields.map { f =>
        val fieldType = f.fieldRef
        val key = f.name

        fieldType match {
          case o: OptionRef[?] =>
            o.optionParamType match {
              case scr: ScalaClassRef[?] =>
                val inner = buildPathTree(scr) + ("__type" -> "{}?")
                key -> inner
              case s: SeqRef[?] =>
                val inner = buildPathTree(s.elementRef) + ("__type" -> "[]?")
                key -> inner
              case _ =>
                key -> "?"
            }

          case s: SeqRef[?] =>
            s.elementRef match {
              case c2: ScalaClassRef[?] =>
                val inner = buildPathTree(c2) + ("__type" -> "[]")
                key -> inner
              case _ =>
                key -> "[]"
            }

          case c3: ScalaClassRef[?] =>
            val inner = buildPathTree(c3) + ("__type" -> "{}")
            key -> inner

          case _ =>
            key -> "" // primitive
        }
      }.toMap

    case _ =>
      Map.empty
  }

  private def liftTypeInfo(map: Map[String, Any])(using Quotes): Expr[Map[String, Any]] = {
    val liftedPairs: List[Expr[(String, Any)]] = map.toList.map {
      case (k, v: String) =>
        '{ Tuple2(${ Expr(k) }, ${ Expr(v) }) }
      case (k, v: Map[String @unchecked, Any @unchecked]) =>
        val nested: Expr[Map[String, Any]] = liftTypeInfo(v)
        '{ Tuple2(${ Expr(k) }, $nested) }
      case (k, _) =>
        quotes.reflect.report.error(s"Unsupported type for key: $k"); '{ ??? }
    }

    val liftedListExpr: Expr[List[(String, Any)]] = Expr.ofList(liftedPairs)
    '{ Map[String, Any]().++($liftedListExpr) }
  }

  def liftMapBoolean(map: Map[String, Boolean])(using Quotes): Expr[Map[String, Boolean]] = {
    val pairs: List[Expr[(String, Boolean)]] =
      map.toList.map { case (k, v) => '{ (${ Expr(k) }, ${ Expr(v) }) } }
    '{ Map[String, Boolean](${ Varargs(pairs) } *) }
  }

