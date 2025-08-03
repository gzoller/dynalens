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
import co.blocke.scala_reflection.reflect.ReflectOnType
import co.blocke.scala_reflection.reflect.rtypeRefs.{FieldInfoRef, OptionRef, ScalaClassRef, ScalaOptionRef, SeqRef}
import co.blocke.scala_reflection.TypedName
import Path.*

import scala.annotation.tailrec

case class DynaLensError(msg: String)

case class DynaLens[T](
    _update: (String, Any, T) => ZIO[Any, DynaLensError, T],
    _get: (String, T) => ZIO[Any, DynaLensError, Any],
    _registry: Map[String, DynaLens[?]],
    _typeName: String
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

  def get(path: String, obj: T): ZIO[Any, DynaLensError, Any] = {
    val parsed = parsePath(path)

    def step(current: Any, currentLens: DynaLens[?], path: List[PathElement]): ZIO[Any, DynaLensError, Any] =
      path match {
        case Nil =>
          ZIO.succeed(current)

        case Field(f) :: rest =>
          currentLens
            ._get(f, current.asInstanceOf[currentLens.ThisT])
            .flatMap { value =>
              currentLens._registry.get(f) match {
                case Some(nextLens) =>
                  step(value, nextLens, rest)
                case None =>
                  if rest.isEmpty then ZIO.succeed(value)
                  else step(value, currentLens, rest)
              }
            }

        case IndexedField(f, i) :: rest =>
          currentLens
            ._get(f, current.asInstanceOf[currentLens.ThisT])
            .flatMap {
              case list: Seq[Any] =>
                if i < 0 && rest == Nil then // return whole Seq
                  ZIO.succeed(list)
                else
                  list.lift(i) match {
                    case Some(elem) =>
                      currentLens._registry.get(f) match {
                        case Some(elemLens) =>
                          step(elem, elemLens, rest)
                        case None =>
                          if rest.isEmpty then ZIO.succeed(elem)
                          else ZIO.fail(DynaLensError(s"No registry for '$f' to recurse into index"))
                      }
                    case None =>
                      ZIO.fail(DynaLensError(s"Index $i out of bounds"))
                  }

              case other =>
                other match {
                  case Some(elem) =>
                    currentLens._registry.get(f) match {
                      case Some(elemLens) =>
                        step(elem, elemLens, rest)
                      case None =>
                        if rest.isEmpty then ZIO.succeed(elem)
                        else ZIO.fail(DynaLensError(s"No registry for '$f' to recurse into index"))
                    }
                  case None => ZIO.fail(DynaLensError(s"Boom 2"))
                  case _ =>
                    ZIO.fail(DynaLensError(s"Expected sequence at '$f', but got ${other.getClass.getSimpleName}"))
                }
            }
      }

    step(obj, this, parsed)
  }

  def update(path: String, value: Any, obj: T): ZIO[Any, DynaLensError, T] = {
    val parsed = parsePath(path)

    def step(current: Any, currentLens: DynaLens[?], path: List[PathElement]): ZIO[Any, DynaLensError, Any] = path match {
      case Nil =>
        ZIO.fail(DynaLensError("Cannot update empty path"))

      case Field(f) :: Nil =>
        currentLens
          ._update(f, value, current.asInstanceOf[currentLens.ThisT])

      case Field(f) :: rest =>
        for {
          nested <- currentLens._get(f, current.asInstanceOf[currentLens.ThisT])
          nextLens <- currentLens._registry.get(f) match
            case Some(a) => ZIO.succeed(a)
            case None    => ZIO.fail(DynaLensError(s"No nested lens for field '$f'"))
          updatedNested <- step(nested, nextLens, rest)
          updated <- currentLens._update(f, updatedNested, current.asInstanceOf[currentLens.ThisT])
        } yield updated

      case IndexedField(f, i) :: rest =>
        for {
          rawList <- currentLens._get(f, current.asInstanceOf[currentLens.ThisT])
          list <- ZIO
            .attempt(rawList.asInstanceOf[Seq[Any]])
            .mapError(_ => DynaLensError(s"Field '$f' is not a Seq"))
          elem <- list.lift(i) match
            case Some(e) => ZIO.succeed(e)
            case None    => ZIO.fail(DynaLensError(s"Index $i out of bounds for field '$f'"))
          nextLens <- currentLens._registry.get(f) match
            case Some(a) => ZIO.succeed(a)
            case None    => ZIO.fail(DynaLensError(s"No nested lens for collection field '$f'"))
          updatedElem <- step(elem, nextLens, rest)
          updatedList = list.updated(i, updatedElem)
          updated <- currentLens._update(f, updatedList, current.asInstanceOf[currentLens.ThisT])
        } yield updated
    }

    step(obj, this, parsed).asInstanceOf[ZIO[Any, DynaLensError, T]]
  }

  private def walkPath(
      path: List[PathElement],
      current: Any,
      dynalens: DynaLens[?]
  ): ZIO[Any, DynaLensError, DynaContext] = {

    val ctx: DynaContext = DynaContext(current, Some(dynalens))

    @tailrec
    def step(
        path: List[PathElement],
        currentLens: DynaLens[?]
    ): ZIO[Any, DynaLensError, Unit] = path match {
      case Field(f) :: rest =>
        currentLens._registry.get(f) match
          case Some(nextLens) => step(rest, nextLens)
          case None           => step(rest, currentLens)

      case IndexedField(f, _) :: rest =>
        currentLens._registry.get(f) match {
          case Some(loopLens) =>
            // Insert into top-level map
            ctx += (f -> (null, Some(loopLens)))
            step(rest, loopLens)
          case None => // simple field--just return current ctx
            ZIO.unit
        }

      case Nil =>
        ZIO.unit
    }

    step(path, dynalens).as(ctx)
  }

  // Split path at Iterables to create sub-paths
  private def splitIntoLevels(path: List[PathElement]): List[List[PathElement]] = {
    val (levels, current) = path.foldLeft(List.empty[List[PathElement]] -> List.empty[PathElement]) {
      case ((acc, current), pe @ IndexedField(_, -1)) =>
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
            case pathParts :: Nil =>
              val partialPath = Path.partialPath(pathParts)
              for {
                in <- lens.get(partialPath, refObj.asInstanceOf[lens.ThisT])
                maybeLens = pathParts.last match {
                  case IndexedField(p, _) => lens._registry.get(p).orElse(None)
                  case Field(p) => None
                }
                _ = ctx.put("this", (in, maybeLens)) // assign loop param variable
                enrichedCtx = outerCtx ++ ctx.toMap // <-- merge loop context with outer context
                out <- fn.resolve(enrichedCtx)
                updated <- lens.update(partialPath, out, refObj.asInstanceOf[lens.ThisT])
              } yield updated

            case pathParts :: rest =>
              val partialPath = Path.partialPath(pathParts)
              val loopKey = pathParts.last.name
              for {
                listVal <- lens.get(partialPath, refObj.asInstanceOf[lens.ThisT])
                iterable <- ZIO.fromEither(listVal match {
                  case i: Iterable[?] => Right(i)
                  case Some(i: Iterable[?]) => Right(i)
                  case None => Right(Nil)
                  case other =>
                    Left(DynaLensError(s"Expected iterable at path '$partialPath', but found: ${other.getClass.getName}"))
                })
                maybeLensForList <- ctx.get(loopKey) match {
                  case Some((_, existingLens)) => ZIO.succeed(existingLens)
                  case None =>
                    ZIO.fail(DynaLensError(s"No existing lens found in ctx for key: $loopKey"))
                }
                updatedIterable <- ZIO.foreach(iterable) { item =>
                  ctx.update(loopKey, (item, maybeLensForList)) // update ctx with current item
                  processPaths(rest, item, maybeLensForList, ctx) // recurse
                }
                updatedRefObj <- lens.update(partialPath, updatedIterable, refObj.asInstanceOf[lens.ThisT])
              } yield updatedRefObj

            case Nil =>
              ZIO.fail(DynaLensError("Should Never Happen(tm)"))
          }
        ).orNull

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
        val typeNameExpr = Expr(s.typedName.toString)

        '{ DynaLens[T]($updateLambdaExpr, $getLambdaExpr, $registryExpr, $typeNameExpr) }

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
