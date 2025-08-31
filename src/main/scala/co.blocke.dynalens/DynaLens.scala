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
                  (i, rest) match {
                    case (None, Nil) =>
                      // full list return
                      ZIO.succeed(list)

                    case (None, _ :: _) =>
                      // Wildcard with a remaining path segment should never reach this low-level getter;
                      // callers must either (a) map over elements, or (b) resolve via a bound element lens.
                      ZIO.fail(DynaLensError(
                        s"Internal: wildcard index for '$f[]' with a remaining path ('${Path.partialPath(rest)}'); " +
                          s"this should be resolved via an element context, not the top lens."
                      ))

                    case (Some(idx), _) =>
                      list.lift(idx) match {
                        case Some(elem) =>
                          currentLens._registry.get(f) match {
                            case Some(elemLens) => step(elem, elemLens, rest)
                            case None =>
                              if (rest.isEmpty) ZIO.succeed(elem)
                              else ZIO.fail(DynaLensError(s"No registry for '$f' to recurse into index"))
                          }
                        case None =>
                          ZIO.fail(DynaLensError(s"Index $idx out of bounds for field '$f'"))
                      }
                  }
                }
            }
      }

    step(obj, this, pathElements)
  }

  sealed trait UpdateMode
  private object UpdateMode {
    case object Assign extends UpdateMode // '='
    case object MapOver extends UpdateMode // '=>'
  }

  final case class Rhs(eval: DynaContext => ZIO[_BiMapRegistry, DynaLensError, Any])

  object Rhs {
    def const(v: Any): Rhs = Rhs(_ => ZIO.succeed(v))
  }

  def update(path: String, value: Any, obj: T): ZIO[_BiMapRegistry, DynaLensError, T] =
    _updateValue(parsePath(path), value, obj)

  private def _updateValue(pathElements: List[PathElement], value: Any, obj: T): ZIO[_BiMapRegistry, DynaLensError, T] = {

    def wrapIfNeeded(opt: Boolean, v: Any): Any =
      if opt then
        v match {
          case _: Option[?] => v
          case _ => Some(v)
        }
      else v

    // withElemCtx should build a DynaContext anchored on the element
    // def withElemCtx(elem: Any, base: DynaContext): DynaContext = ...

    def step(
              current: Any,
              currentLens: DynaLens[?],
              path: List[PathElement],
              mode: UpdateMode,
              rhsValue: Rhs,
              baseCtx: DynaContext
            ): ZIO[_BiMapRegistry, DynaLensError, Any] = path match {
      case Nil =>
        ZIO.fail(DynaLensError("Cannot update empty path"))

      // Field leaf: foo = <rhs>
      case Field(f, isOptional) :: Nil =>
        rhsValue.eval(baseCtx).flatMap { raw =>
          val finalValue = wrapIfNeeded(isOptional, raw)
          currentLens._update(f, finalValue, current.asInstanceOf[currentLens.ThisT])
        }

      // Field descent: foo.bar = ...
      case Field(f, isOptional) :: rest =>
        for {
          nested <- currentLens._get(f, current.asInstanceOf[currentLens.ThisT])
          nextLens <- currentLens._registry.get(f) match
            case Some(a) => ZIO.succeed(a)
            case None => ZIO.fail(DynaLensError(s"No nested lens for field '$f'"))
          updatedNested <- step(nested, nextLens, rest, mode, rhsValue, baseCtx)
          wrapped = wrapIfNeeded(isOptional, updatedNested)
          updated <- currentLens._update(f, wrapped, current.asInstanceOf[currentLens.ThisT])
        } yield updated

      // Indexed (list) … (your normalized, MapOver/Assign split, with per-element ctx)
      case IndexedField(f, idxOpt, fieldIsOpt) :: rest =>
        currentLens
          ._get(f, current.asInstanceOf[currentLens.ThisT])
          .flatMap { raw =>
            // Normalize to List[Any], gracing None/Some(None) -> Nil
            val listOpt: Option[List[Any]] = raw match {
              case null => Some(Nil)
              case None => Some(Nil)
              case Some(None) => Some(Nil)
              case Some(s: Seq[?]) => Some(s.toList)
              case s: Seq[?] => Some(s.toList)
              case Some(it: Iterable[?]) => Some(it.toList)
              case it: Iterable[?] => Some(it.toList)
              case _ => None
            }

            listOpt match {
              case None =>
                ZIO.fail(DynaLensError(s"Field '$f' is not a Seq"))

              case Some(list) =>
                idxOpt match {

                  // ------------------ wildcard [] ------------------
                  case None =>
                    rest match {
                      // foo[] = rhs  (whole-list replacement)
                      case Nil =>
                        rhsValue.eval(baseCtx).flatMap { rv =>
                          val elemIsOpt = currentLens._elemIsOptional.getOrElse(f, false)

                          // If elements are optional, wrap each element unless already Option
                          val rvElemAdjusted: Any = rv match {
                            case s: Seq[?] if elemIsOpt =>
                              s.map(wrapIfNeeded(true, _)).toList
                            case it: Iterable[?] if elemIsOpt =>
                              it.map(wrapIfNeeded(true, _)).toList
                            case other => other
                          }

                          // If the field itself is optional (Option[List[_]]), apply Nil->None and box lists
                          val finalValue: Any =
                            if (fieldIsOpt) rvElemAdjusted match {
                              case s: Seq[?] if s.isEmpty => None
                              case s: Seq[?] => Some(s.toList)
                              case it: Iterable[?] => Some(it.toList)
                              case opt: Option[?] => opt
                              case other => Some(other)
                            } else rvElemAdjusted

                          currentLens
                            ._update(f, finalValue, current.asInstanceOf[currentLens.ThisT])
                            .map(_.asInstanceOf[Any])
                        }

                      // foo[].bar = rhs  (map over list)
                      case _ =>
                        mode match {
                          case UpdateMode.MapOver =>
                            currentLens._registry.get(f) match {
                              case Some(elemLens) =>
                                ZIO
                                  .foreach(list) { elem =>
                                    step(
                                      elem,
                                      elemLens,
                                      rest,
                                      mode,
                                      rhsValue,
                                      withElemCtx(elem, baseCtx)
                                    )
                                  }
                                  .flatMap { updatedItems =>
                                    currentLens
                                      ._update(f, updatedItems, current.asInstanceOf[currentLens.ThisT])
                                      .map(_.asInstanceOf[Any])
                                  }
                              case None =>
                                ZIO.fail(DynaLensError(s"No nested lens for collection field '$f'"))
                            }
                          case UpdateMode.Assign =>
                            ZIO.fail(DynaLensError(s"Cannot assign to wildcard '$f[]' — use '=>' for map or assign the whole field"))
                        }
                    }

                  // ------------------ fixed index [i] ------------------
                  case Some(i) =>
                    list.lift(i) match {
                      case None =>
                        ZIO.fail(DynaLensError(s"Index $i out of bounds for field '$f'"))

                      case Some(elem) =>
                        currentLens._registry.get(f) match {
                          case Some(elemLens) =>
                            if (rest.nonEmpty) {
                              // recurse inside element, then splice back
                              step(
                                elem,
                                elemLens,
                                rest,
                                mode,
                                rhsValue,
                                withElemCtx(elem, baseCtx)
                              ).flatMap { updatedElem =>
                                val newSeq = list.updated(i, updatedElem)
                                currentLens
                                  ._update(f, newSeq, current.asInstanceOf[currentLens.ThisT])
                                  .map(_.asInstanceOf[Any])
                              }
                            } else {
                              // leaf element replace: wrap if element type is optional
                              val elemCtx = withElemCtx(elem, baseCtx)
                              val elemIsOpt = currentLens._elemIsOptional.getOrElse(f, false)
                              rhsValue.eval(elemCtx).flatMap { rv =>
                                val rvFinal = wrapIfNeeded(elemIsOpt, rv)
                                val patched = list.updated(i, rvFinal)
                                currentLens
                                  ._update(f, patched, current.asInstanceOf[currentLens.ThisT])
                                  .map(_.asInstanceOf[Any])
                              }
                            }

                          case None =>
                            if (rest.isEmpty) {
                              // same leaf element replace, but no elemLens
                              val elemCtx = withElemCtx(elem, baseCtx)
                              val elemIsOpt = currentLens._elemIsOptional.getOrElse(f, false)
                              rhsValue.eval(elemCtx).flatMap { rv =>
                                val rvFinal = wrapIfNeeded(elemIsOpt, rv)
                                val patched = list.updated(i, rvFinal)
                                currentLens
                                  ._update(f, patched, current.asInstanceOf[currentLens.ThisT])
                                  .map(_.asInstanceOf[Any])
                              }
                            } else {
                              ZIO.fail(DynaLensError(s"No registry for '$f' to recurse into index"))
                            }
                        }
                    }
                }
            }
          }
    }

    val rhs: Rhs = Rhs.const(value) // <-- constant RHS for '='
    val baseCtx: DynaContext = DynaContext(obj, Some(this))

    step(
      obj,
      this,
      pathElements,
      UpdateMode.Assign, // '=' path
      rhs,
      baseCtx
    ).map(_.asInstanceOf[T])
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

  // Walk registry lenses along a path prefix to reach the parent lens
  private def lensForPathPrefix(root: DynaLens[?], parts: List[Path.PathElement]): Option[DynaLens[?]] =
    parts.foldLeft(Option(root)) {
      case (None, _) => None
      case (Some(cur), Path.Field(name, _)) => cur._registry.get(name)
      case (Some(cur), Path.IndexedField(name, _, _)) => cur._registry.get(name) // element lens of the collection
    }

  def map[R](
              path: String,
              fn: Fn[R],
              obj: T,
              outerCtx: DynaContext = DynaContext.empty
            ): ZIO[_BiMapRegistry, DynaLensError, T] = {

    def processPaths(
                      paths: List[List[Path.PathElement]],
                      refObj: Any,
                      dynalensOpt: Option[DynaLens[?]],
                      ctx: DynaContext,
                      outer: DynaContext,
                      bodyFn: Fn[Any]
                    ): ZIO[_BiMapRegistry, DynaLensError, Any] =
      dynalensOpt match {
        case None =>
          ZIO.fail(DynaLensError("Internal: missing lens for map()"))

        case Some(lens) =>
          for {
            res <- paths match {
              // ----- LEAF -----
              case pathParts :: Nil =>
                val partial = Path.partialPath(pathParts)
                for {
                  in <- lens._getValue(pathParts, refObj.asInstanceOf[lens.ThisT])

                  // if last segment was an indexed field, you already compute maybeLens
                  maybeLens = pathParts.last match {
                    case Path.IndexedField(fieldName, _, _) => lens._registry.get(fieldName)
                    case _                                  => None
                  }

                  outAny <- in match {
                    // OPTION-MAP (Some): bind `this` to the unwrapped value, eval RHS, re-wrap
                    case Some(v) =>
                      val enriched = outerCtx ++ ctx.set("this", (v, maybeLens)).toMap
                      fn.resolve(enriched).map(o => Some(o))

                    // OPTION-MAP (None): stays None
                    case None =>
                      ZIO.succeed(None)

                    // LIST-MAP or SCALAR (no option): your existing path
                    case nonOpt =>
                      val enriched = outerCtx ++ ctx.set("this", (nonOpt, maybeLens)).toMap
                      fn.resolve(enriched)
                  }

                  updated <- outAny match {
                    case _: Some[?] | None => // Option case
                      lens.update(partial, outAny, refObj.asInstanceOf[lens.ThisT])

                    case plain =>              // Non-option case
                      lens.update(partial, plain, refObj.asInstanceOf[lens.ThisT])
                  }
                } yield updated

              // ----- DESCENT -----
              case pathParts :: rest =>
                val partial  = Path.partialPath(pathParts)
                val loopKey  = pathParts.last.name

                for {
                  listVal <- lens._getValue(pathParts, refObj.asInstanceOf[lens.ThisT])

                  iterable <- ZIO.fromEither(listVal match {
                    case i: Iterable[?]       => Right(i.asInstanceOf[Iterable[Any]])
                    case Some(i: Iterable[?]) => Right(i.asInstanceOf[Iterable[Any]])
                    case None                 => Right(Nil)
                    case other => Left(DynaLensError(s"Expected iterable at path '$partial', but got: ${other.getClass.getName}"))
                  })

                  // NEW: find the lens at the parent of the collection, then get the element lens for `loopKey`
                  parentLens  = lensForPathPrefix(lens, pathParts.dropRight(1))
                  elemLens    = parentLens.flatMap(_._registry.get(loopKey))

                  _ <- if (rest.nonEmpty && elemLens.isEmpty)
                    ZIO.fail(DynaLensError(s"No lens registered for elements of '$loopKey' (needed to map nested path '$partial')"))
                  else ZIO.unit

                  updatedIterable <- withCollectionSymbol(ctx, loopKey, iterable) {
                    ZIO.foreach(iterable) { item =>
                      withLoopSymbol(ctx, loopKey, item, elemLens) {
                        processPaths(rest, item, elemLens, ctx, outer, bodyFn)
                      }
                    }
                  }

                  updatedRef <- lens._updateValue(pathParts, updatedIterable, refObj.asInstanceOf[lens.ThisT])
                } yield updatedRef

              case Nil =>
                ZIO.fail(DynaLensError("Should Never Happen(tm)"))
            }
          } yield res
      }

    val parsed = Path.parsePath(path)
    for {
      // establish a loop frame/bindings for the full path (top, etc.)
      ctx <- walkPath(parsed, obj, this)
      splitPaths = splitIntoLevels(parsed)
      updated <- processPaths(splitPaths, obj, Some(this), ctx, outerCtx, fn.asInstanceOf[Fn[Any]])
    } yield updated.asInstanceOf[T]
  }


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

  // Encodes the *value* shape that appears under a Map’s "__valType"
  private def buildMapValueSchema(r: RTypeRef[?]): Any = r match {
    case c: ScalaClassRef[?] =>
      // Case class: expand fields (no __type tag for classes)
      buildPathTree(c)

    case s: SeqRef[?] =>
      // List/Seq value
      s.elementRef match {
        case c2: ScalaClassRef[?] =>
          buildPathTree(c2) + ("__type" -> "[]")
        case _ =>
          "[]"
      }

    case m: MapRef[?] =>
      // Map value: record {} and recurse again for its value type
      Map(
        "__type" -> "{}",
        "__valType" -> buildMapValueSchema(m.elementRef2)
      )

    case o: OptionRef[?] =>
      // Optional value – encode with the right suffix
      o.optionParamType match {
        case c: ScalaClassRef[?] =>
          buildPathTree(c) + ("__type" -> "{}?") // optional *object-like* payload
        case s: SeqRef[?] =>
          buildPathTree(s.elementRef) + ("__type" -> "[]?")
        case m: MapRef[?] =>
          Map(
            "__type" -> "{}?",
            "__valType" -> buildMapValueSchema(m.elementRef2)
          )
        case _ =>
          "?" // optional scalar
      }

    case _ =>
      "" // plain scalar
  }

  /** Build the schema tree used for path rewriting/type checks. */
  // Build the schema tree used as typeInfo for the compiler/rewrite
  // Encodes containers with a typed child:
  //   - lists: "__type" -> "[]"/"[]?", "__elemType" -> <shape>
  //   - maps:  "__type" -> "{}"/"{}?", "__valType"  -> <shape>
  //   - classes: fields map + "__type" -> "{}"/"{}?"
  //   - scalars: ""
  private def buildPathTree(r: RTypeRef[?]): Map[String, Any] = r match {
    // ----- Case class / product type -----
    case c: ScalaClassRef[?] =>
      c.fields.map { f =>
        val key       = f.name
        val fieldType = f.fieldRef

        // shape builder for nested “value/element” positions
        def valueShapeOf(ref: RTypeRef[?]): Any = ref match {
          case scr: ScalaClassRef[?] =>
            // full nested class shape
            buildPathTree(scr) + ("__type" -> "{}")
          case s: SeqRef[?] =>
            Map(
              "__type"     -> "[]",
              "__elemType" -> valueShapeOf(s.elementRef) // recursive element shape
            )
          case m: MapRef[?] =>
            Map(
              "__type"   -> "{}",
              "__valType"-> valueShapeOf(m.elementRef2)     // recursive value shape
            )
          case _ =>
            "" // primitive / string / other scalar
        }

        val node: Any = fieldType match {
          // ----- Option[T] -----
          case o: OptionRef[?] =>
            o.optionParamType match {
              case scr: ScalaClassRef[?] =>
                // Optional case class → include its fields + {}?
                buildPathTree(scr) + ("__type" -> "{}?")
              case s: SeqRef[?] =>
                // Optional list → []? with typed elem
                Map(
                  "__type"     -> "[]?",
                  "__elemType" -> valueShapeOf(s.elementRef)
                )
              case m: MapRef[?] =>
                // Optional map → {}? with typed value
                Map(
                  "__type"   -> "{}?",
                  "__valType"-> valueShapeOf(m.elementRef2)
                )
              case _ =>
                "?" // optional scalar
            }

          // ----- Seq/List/Array[T] -----
          case s: SeqRef[?] =>
            Map(
              "__type"     -> "[]",
              "__elemType" -> valueShapeOf(s.elementRef)
            )

          // ----- Map[K,V] -----
          case m: MapRef[?] =>
            Map(
              "__type"   -> "{}",
              "__valType"-> valueShapeOf(m.elementRef2)
            )

          // ----- Nested case class (non-container) -----
          case scr: ScalaClassRef[?] =>
            buildPathTree(scr) + ("__type" -> "{}")

          // ----- Scalar -----
          case _ =>
            "" // primitive / string
        }

        key -> node
      }.toMap

    // Fallback: non-class root (unlikely in your use)
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

  private def liftMapBoolean(map: Map[String, Boolean])(using Quotes): Expr[Map[String, Boolean]] = {
    val pairs: List[Expr[(String, Boolean)]] =
      map.toList.map { case (k, v) => '{ (${ Expr(k) }, ${ Expr(v) }) } }
    '{ Map[String, Boolean](${ Varargs(pairs) } *) }
  }

