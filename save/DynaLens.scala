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
    _typeInfo: Map[String, Any],
    _schema: ClassType,
    _elemIsOptional: Map[String, Boolean] // per-field: Seq element is Option[_]?
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
    _getValue(parsePath(path), obj)

  private def _getValue(pathElements: List[PathElement], obj: T): ZIO[Any, DynaLensError, Any] = {

    def step(current: Any, currentLens: DynaLens[?], path: List[PathElement]): ZIO[Any, DynaLensError, Any] =
      path match {
        case Nil =>
          ZIO.succeed(current)

        case Field(f, isOptional) :: rest =>
          val base = current match {
            case Some(coll: Iterable[?]) if coll.nonEmpty => coll.head
            case coll: Iterable[?] if coll.nonEmpty       => coll.head
            case Some(v)                                 => v
            case other                                   => other
          }
          println(
            s"[step] ENTER: field='$f', " +
              s"current=${Option(current).map(_.getClass.getName).getOrElse("null")}, " +
              s"lensClass=${currentLens.getClass.getSimpleName}, " +
              s"rest=${rest.map(_.toString).mkString("→")}"
          )
          currentLens
            ._get(f, base.asInstanceOf[currentLens.ThisT])
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
                  if i.isEmpty && rest == Nil then ZIO.succeed(Nil) // missing optional list → Nil
                  else ZIO.fail(DynaLensError(s"Cannot index into missing optional list field '$f'"))
                else ZIO.fail(DynaLensError(s"Field '$f' is not a Seq"))

              case seq =>
                val listZio: ZIO[Any, DynaLensError, Seq[Any]] = seq match {
                  case s: Seq[?]                     => ZIO.succeed(s)
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
                      ZIO.fail(
                        DynaLensError(
                          s"Internal: wildcard index for '$f[]' with a remaining path ('${Path.partialPath(rest)}'); " +
                            s"this should be resolved via an element context, not the top lens."
                        )
                      )

                    case (Some(idx), _) =>
                      list.lift(idx) match {
                        case Some(elem) =>
                          currentLens._registry.get(f) match {
                            case Some(elemLens) => step(elem, elemLens, rest)
                            case None =>
                              if rest.isEmpty then ZIO.succeed(elem)
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

  private object Rhs {
    def const(v: Any): Rhs = Rhs(_ => ZIO.succeed(v))
  }

  def update(path: String, value: Any, obj: T): ZIO[_BiMapRegistry, DynaLensError, T] =
    _updateValue(parsePath(path), value, obj)

  private def _updateValue(pathElements: List[PathElement], value: Any, obj: T): ZIO[_BiMapRegistry, DynaLensError, T] = {
    println(s"[UVAL] lens=${_typeName} path=$pathElements " +
      s"value.class=${Option(value).map(_.getClass.getName).getOrElse("null")} " +
      s"obj.class=${Option(obj).map(_.getClass.getName).getOrElse("null")}")
    def wrapIfNeeded(opt: Boolean, v: Any): Any =
      if opt then
        v match {
          case _: Option[?] => v
          case _            => Some(v)
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
            case None    => ZIO.fail(DynaLensError(s"No nested lens for field '$f'"))
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
              case null                  => Some(Nil)
              case None                  => Some(Nil)
              case Some(None)            => Some(Nil)
              case Some(s: Seq[?])       => Some(s.toList)
              case s: Seq[?]             => Some(s.toList)
              case Some(it: Iterable[?]) => Some(it.toList)
              case it: Iterable[?]       => Some(it.toList)
              case _                     => None
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
                            if fieldIsOpt then
                              rvElemAdjusted match {
                                case s: Seq[?] if s.isEmpty => None
                                case s: Seq[?]              => Some(s.toList)
                                case it: Iterable[?]        => Some(it.toList)
                                case opt: Option[?]         => opt
                                case other                  => Some(other)
                              }
                            else rvElemAdjusted

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
                        raw match
                          case None => // None -- no-op
                            ZIO.succeed(current)
                          case _ =>  // Some(...) -> out of bounds
                            ZIO.fail(DynaLensError(s"Index $i out of bounds for field '$f'"))

                      case Some(elem) =>
                        currentLens._registry.get(f) match {
                          case Some(elemLens) =>
                            if rest.nonEmpty then {
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
                            if rest.isEmpty then {
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
        println(s"[step] value type = ${value.getClass.getName}, rest = $rest")
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
      case (None, _)                                  => None
      case (Some(cur), Path.Field(name, _))           => cur._registry.get(name)
      case (Some(cur), Path.IndexedField(name, _, _)) => cur._registry.get(name) // element lens of the collection
    }

  // stop one element *before* we’d have to enter a collection
  private def prefixUntilFirstCollection(p: List[Path.PathElement]): List[Path.PathElement] =
    p match {
      case Nil => Nil
      case _ =>
        // take until we *encounter* an IndexedField, but do not include it,
        // then drop the final element (so "interest.qty" => List(Field("interest")))
        p.takeWhile {
          case Path.IndexedField(_, _, _) => false
          case _ => true
        }.dropRight(1)
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
                val partial  = Path.partialPath(pathParts)
                val parent   = pathParts.dropRight(1)
                val lastSeg  = pathParts.last
                val lastName = lastSeg.name

                inline def tname(x: Any): String = Option(x).map(_.getClass.getName).getOrElse("null")
                println(s"[LEAF] pathParts=${pathParts.mkString("→")}, parent=${parent.mkString("→")}, last=${lastSeg}")

                if (parent.isEmpty) {
                  // ---------- One-segment leaf: e.g. "m" (whole-field mapping) ----------
                  for {
                    in <- lens._getValue(pathParts, refObj.asInstanceOf[lens.ThisT])
                    _  = println(s"[LEAF/one] in.class=${tname(in)} for field='$lastName' (whole-field mapping)")

                    out <- in match {
                      // ---------- Map field: m => { ... (this.key / this.value) ... } ----------
                      case mm: scala.collection.Map[?, ?] @unchecked =>
                        val m = mm.asInstanceOf[scala.collection.Map[Any, Any]]
                        println(s"[LEAF/one] Detected Map field '$lastName' with ${m.size} entries")

                        // Lens for MAP VALUE (if any): registry(lastName) should refer to the value/element lens
                        val valueLens = lens._registry.get(lastName)
                        val entries   = m.toList

                        ZIO.foreach(entries) { case (k, v) =>
                          // Bind this/key/value per entry
                          withKeyScoped(ctx, "key", (k, None)) {
                            withKeyScoped(ctx, "value", (v, valueLens)) {
                              withThisScoped(ctx, (k, v), valueLens) {
                                bodyFn.resolve(ctx).flatMap {
                                  case (nk: Any, nv: Any) => ZIO.succeed((nk, nv))
                                  case other =>
                                    ZIO.fail(
                                      DynaLensError(
                                        s"[LEAF/one] Map body must return (key, value) tuple, got: ${other.getClass.getSimpleName}"
                                      )
                                    )
                                }
                              }
                            }
                          }
                        }.flatMap { pairs =>
                          val rebuilt = pairs.toMap
                          println(s"[LEAF/one] rebuilt map size=${rebuilt.size}")
                          lens.update(partial, rebuilt, refObj.asInstanceOf[lens.ThisT])
                        }

                      // ---------- Option[Map] field ----------
                      // TODO: This seems to be a dupe of the previous case--not something special for Option[Map]
                      case Some(mm: scala.collection.Map[?, ?] @unchecked) =>
                        val m = mm.asInstanceOf[scala.collection.Map[Any, Any]]
                        println(s"[LEAF/one] Detected Option[Map] field '$lastName' with ${m.size} entries (Some)")
                        val valueLens = lens._registry.get(lastName)
                        val entries   = m.toList

                        ZIO.foreach(entries) { case (k, v) =>
                          withKeyScoped(ctx, "key", (k, None)) {
                            withKeyScoped(ctx, "value", (v, valueLens)) {
                              withThisScoped(ctx, (k, v), valueLens) {
                                bodyFn.resolve(ctx).flatMap {
                                  case (nk: Any, nv: Any) => ZIO.succeed((nk, nv))
                                  case other =>
                                    ZIO.fail(
                                      DynaLensError(
                                        s"[LEAF/one] Map body must return (key, value) tuple, got: ${other.getClass.getSimpleName}"
                                      )
                                    )
                                }
                              }
                            }
                          }
                        }.flatMap { pairs =>
                          val rebuilt = pairs.toMap
                          println(s"[LEAF/one] rebuilt Option[Map] size=${rebuilt.size}")
                          lens.update(partial, Some(rebuilt), refObj.asInstanceOf[lens.ThisT])
                        }

                      // ---------- Option[Map] == None ----------
                      case None =>
                        println(s"[LEAF/one] Option[Map] is None for field '$lastName' → write back None")
                        lens.update(partial, None, refObj.asInstanceOf[lens.ThisT])

                      // ---------- Non-Map field: keep existing whole-field behavior ----------
                      case other =>
                        val localCtx = outerCtx ++ ctx.set("this", (other, None)).toMap
                        for {
                          bodyOut <- bodyFn.resolve(localCtx)
                          _       = println(s"[LEAF/one] bodyOut.class=${tname(bodyOut)}")
                          valueToWrite <-
                            lastSeg match {
                              case Path.Field(_, isOpt) =>
                                val v =
                                  if (isOpt) bodyOut match {
                                    case o: Option[?] => o
                                    case null         => None
                                    case x            => Some(x)
                                  } else bodyOut
                                ZIO.succeed(v)
                              case _ =>
                                ZIO.fail(DynaLensError(s"Unexpected non-Field leaf for '$partial'"))
                            }
                          _ = println(s"[LEAF/one] writing back to '$partial' value.class=${tname(valueToWrite)}")
                          outRef <- lens.update(partial, valueToWrite, refObj.asInstanceOf[lens.ThisT])
                        } yield outRef
                    }
                  } yield out

                } else {
                  // ---------- Multi-segment leaf like "interest.qty" (per-element updates) ----------
                  for {
                    parentVal <- lens._getValue(parent, refObj.asInstanceOf[lens.ThisT])
                    _         = println(s"[LEAF/multi] parentVal.class=${tname(parentVal)}")

                    elems <- parentVal match {
                      case it: Iterable[?] =>
                        val xs = it.asInstanceOf[Iterable[Any]].toList
                        println(s"[LEAF/multi] elems (Iterable) size=${xs.size} head=${xs.headOption.map(tname)}")
                        ZIO.succeed(xs)
                      case Some(it: Iterable[?]) =>
                        val xs = it.asInstanceOf[Iterable[Any]].toList
                        println(s"[LEAF/multi] elems (Some(Iterable)) size=${xs.size} head=${xs.headOption.map(tname)}")
                        ZIO.succeed(xs)
                      case None =>
                        println(s"[LEAF/multi] elems = Nil (parent is None)")
                        ZIO.succeed(Nil)
                      case other =>
                        ZIO.fail(DynaLensError(s"Expected iterable parent for '$partial', got ${tname(other)}"))
                    }

                    parentLens  = lensForPathPrefix(lens, parent.dropRight(1))
                    elemLensOpt = parentLens.flatMap(_._registry.get(parent.last.name))
                    _           = println(s"[LEAF/multi] elemLensOpt.isDefined=${elemLensOpt.isDefined} parent='${parent.last.name}' field='$lastName'")
                    elemLens   <- ZIO.fromOption(elemLensOpt)
                      .orElseFail(DynaLensError(s"No element lens registered for '${parent.last.name}' when accessing '$lastName'"))

                    // body sees this = elems (Iterable), with element lens attached
                    localCtx = outerCtx ++ ctx.set("this", (elems, Some(elemLens))).toMap
                    bodyOut <- bodyFn.resolve(localCtx)
                    _       = println(s"[LEAF/multi] bodyOut.class=${tname(bodyOut)}")

                    newVals <- bodyOut match {
                      case it: Iterable[?] =>
                        val xs = it.asInstanceOf[Iterable[Any]].toList
                        if (xs.size != elems.size)
                          ZIO.fail(DynaLensError(s"Body returned ${xs.size} values for ${elems.size} elements at '$partial'"))
                        else
                          ZIO.succeed(xs)
                      case scalar =>
                        ZIO.succeed(List.fill(elems.size)(scalar))
                    }
                    _ = println(s"[LEAF/multi] prepared newVals.size=${newVals.size}")

                    updatedElems <- ZIO.foreach(elems.zip(newVals)) { case (elem, newVal) =>
                      println(s"[LEAF/multi]  - update elem.class=${tname(elem)} with newVal.class=${tname(newVal)} field='$lastName'")
                      elemLens.update(lastName, newVal, elem.asInstanceOf[elemLens.ThisT]).map(_.asInstanceOf[Any])
                    }

                    outRef <- {
                      val parentKey = Path.partialPath(parent)
                      parentVal match {
                        case Some(_: Iterable[?]) =>
                          println(s"[LEAF/multi] writing back Some(updatedElems) to parent='$parentKey'")
                          lens.update(parentKey, Some(updatedElems), refObj.asInstanceOf[lens.ThisT])
                        case _ =>
                          println(s"[LEAF/multi] writing back updatedElems to parent='$parentKey'")
                          lens.update(parentKey, updatedElems, refObj.asInstanceOf[lens.ThisT])
                      }
                    }
                  } yield outRef
                }
              /*
              // ----- LEAF -----
              case pathParts :: Nil =>
                val partial = Path.partialPath(pathParts)

                val parent = pathParts.dropRight(1)
                val lastSeg = pathParts.last
                println(s"[LEAF] pathParts=${pathParts.mkString("→")}, parent=${parent.mkString("→")}, last=${lastSeg}")

                for {
                  in <-
                    if parent.nonEmpty then
                      lens._getValue(parent, refObj.asInstanceOf[lens.ThisT]).flatMap {
                        case coll: Iterable[?] =>
                          // Get element lens for the collection
                          val parentLensOpt = lensForPathPrefix(lens, parent.dropRight(1))
                          val elemLensOpt   = parentLensOpt.flatMap(_._registry.get(parent.last.name))

                          elemLensOpt match {
                            case Some(el) =>
                              ZIO.foreach(coll.asInstanceOf[Iterable[Any]]) { elem =>
                                el._get(lastSeg.name, elem.asInstanceOf[el.ThisT])
                              }.map(_.collect { case Some(x) => x }.toList)

                            case None =>
                              ZIO.fail(DynaLensError(s"No element lens registered for '${parent.last.name}' when accessing '${lastSeg.name}'"))
                          }

                        case Some(coll: Iterable[?]) =>
                          val parentLensOpt = lensForPathPrefix(lens, parent.dropRight(1))
                          val elemLensOpt   = parentLensOpt.flatMap(_._registry.get(parent.last.name))
                          elemLensOpt match {
                            case Some(el) =>
                              ZIO.foreach(coll.asInstanceOf[Iterable[Any]]) { elem =>
                                el._get(lastSeg.name, elem.asInstanceOf[el.ThisT])
                              }.map(xs => Some(xs.collect { case Some(x) => x }.toList))
                            case None =>
                              ZIO.fail(DynaLensError(s"No element lens registered for '${parent.last.name}' when accessing '${lastSeg.name}'"))
                          }

                        case other =>
                          lens._getValue(pathParts, refObj.asInstanceOf[lens.ThisT])
                      }
                    else
                      lens._getValue(pathParts, refObj.asInstanceOf[lens.ThisT])
//                  in <- lens._getValue(pathParts, refObj.asInstanceOf[lens.ThisT])
                  _ <- ZIO.succeed(println(s"[LEAF] in.class=${Option(in).map(_.getClass.getName).getOrElse("null")}"))

                  // if last segment was an indexed field, you already compute maybeLens
                  maybeLens = pathParts.last match {
                    case Path.IndexedField(fieldName, _, _) => lens._registry.get(fieldName)
                    case _                                  => None
                  }

                  // If it's a (option-)map, transform entries and write back right here.
                  updatedRef <- in match {
                    // ----- Map leaf -----
                    case mm: scala.collection.Map[?, ?] @unchecked =>
                      val m = mm.asInstanceOf[scala.collection.Map[Any, Any]]

                      // lens for the MAP VALUE (if any)
                      val parentLens = lensForPathPrefix(lens, pathParts.dropRight(1))
                      val valueLens = parentLens.flatMap(_._registry.get(pathParts.last.name))

                      // transform (k,v) -> (nk,nv)
                      for {
                        pairs <- ZIO.foreach(m.toList) { case (k, v) =>
                          withKeyScoped(ctx, "key", (k, None)) {
                            withKeyScoped(ctx, "value", (v, valueLens)) {
                              withThisScoped(ctx, (k, v), valueLens) {
                                fn.resolve(ctx).flatMap {
                                  case (nk: Any, nv: Any) => ZIO.succeed((nk, nv))
                                  case other =>
                                    ZIO.fail(DynaLensError(s"Map body must return (key, value) tuple, got: ${other.getClass.getSimpleName}"))
                                }
                              }
                            }
                          }
                        }
                        rebuilt = pairs.toMap
                        out <- lens.update(partial, rebuilt, refObj.asInstanceOf[lens.ThisT])
                      } yield out

                    // ----- Option[Map] leaf -----
                    case Some(mm: scala.collection.Map[?, ?] @unchecked) =>
                      val m = mm.asInstanceOf[scala.collection.Map[Any, Any]]
                      val parentLens = lensForPathPrefix(lens, pathParts.dropRight(1))
                      val valueLens = parentLens.flatMap(_._registry.get(pathParts.last.name))

                      for {
                        pairs <- ZIO.foreach(m.toList) { case (k, v) =>
                          withKeyScoped(ctx, "key", (k, None)) {
                            withKeyScoped(ctx, "value", (v, valueLens)) {
                              withThisScoped(ctx, (k, v), valueLens) {
                                fn.resolve(ctx).flatMap {
                                  case (nk: Any, nv: Any) => ZIO.succeed((nk, nv))
                                  case other =>
                                    ZIO.fail(DynaLensError(s"Map body must return (key, value) tuple, got: ${other.getClass.getSimpleName}"))
                                }
                              }
                            }
                          }
                        }
                        rebuilt = pairs.toMap
                        out <- lens.update(partial, Some(rebuilt), refObj.asInstanceOf[lens.ThisT])
                      } yield out

                    // Option[Map] == None → write back None (or keep as-is if you prefer)
                    case None =>
                      lens.update(partial, None, refObj.asInstanceOf[lens.ThisT])

                    // ----- All other shapes: Option[non-map], list, scalar -----
                    case Some(v) =>
                      println(s"[LEAF] assigning this=${Option(v).map(_.getClass.getName).getOrElse("null")} for bodyFn=${fn.getClass.getSimpleName}")
                      val enriched = outerCtx ++ ctx.set("this", (v, maybeLens)).toMap
                      for {
                        o <- fn.resolve(enriched)
                        out <- lens.update(partial, Some(o), refObj.asInstanceOf[lens.ThisT])
                      } yield out

                    case nonOpt =>
                      println(s"[LEAF (nonOpt)] assigning this=${Option(nonOpt).map(_.getClass.getName).getOrElse("null")} for bodyFn=${fn.getClass.getSimpleName}")
                      val enriched = outerCtx ++ ctx.set("this", (nonOpt, maybeLens)).toMap
                      for {
                        o <- fn.resolve(enriched)
                        out <- lens.update(partial, o, refObj.asInstanceOf[lens.ThisT])
                      } yield out
                  }
                } yield updatedRef
               */

              // ----- DESCENT -----
              case pathParts :: rest =>
                val partial = Path.partialPath(pathParts)
                val collName = pathParts.last.name

                for {
                  collVal <- lens._getValue(pathParts, refObj.asInstanceOf[lens.ThisT])

                  // Detect map vs list, and normalize to entries/items we can foreach
                  isMap = collVal.isInstanceOf[scala.collection.immutable.Map[?, ?]] ||
                    collVal.isInstanceOf[scala.collection.mutable.Map[?, ?]] ||
                    collVal.isInstanceOf[Option[?]] && collVal.asInstanceOf[Option[?]].exists(_.isInstanceOf[scala.collection.Map[?, ?]])

                  // For maps, iterate entries; for lists, iterate items (existing)
                  iterableE <- ZIO.fromEither {
                    collVal match {
                      case m: scala.collection.Map[?, ?] @unchecked =>
                        Right(m.asInstanceOf[scala.collection.Map[Any, Any]].toList) // entries
                      case Some(m: scala.collection.Map[?, ?] @unchecked) =>
                        Right(m.asInstanceOf[scala.collection.Map[Any, Any]].toList)
                      case None =>
                        Right(Nil)
                      case it: Iterable[?] =>
                        Right(it.asInstanceOf[Iterable[Any]])
                      case Some(it: Iterable[?]) =>
                        Right(it.asInstanceOf[Iterable[Any]])
                      case other =>
                        Left(DynaLensError(s"Expected iterable at path '$partial', but got: ${other.getClass.getName}"))
                    }
                  }

                  // Discover the element/value lens if needed
                  parentLens = lensForPathPrefix(lens, pathParts.dropRight(1))
                  elemLens = parentLens.flatMap(_._registry.get(collName))

                  _ <-
                    if rest.nonEmpty && elemLens.isEmpty && !isMap then ZIO.fail(DynaLensError(s"No lens registered for elements of '$collName' (needed to map nested path '$partial')"))
                    else ZIO.unit

                  updatedIterable <- {
                    if isMap then {
                      // Iterate entries: (k, v)
                      withCollectionSymbol(ctx, collName, iterableE) {
                        ZIO.foreach(iterableE.asInstanceOf[List[(Any, Any)]]) { case (k, v) =>
                          withKeyScoped(ctx, "key", (k, None)) {
                            withKeyScoped(ctx, "value", (v, elemLens)) {
                              withThisScoped(ctx, (k, v), elemLens) {
                                if rest.nonEmpty then {
                                  // Recurse into the VALUE part for nested paths
                                  processPaths(rest, v, elemLens, ctx, outer, bodyFn).map(nv => (k, nv))
                                } else {
                                  // Leaf map: RHS must yield (newKey, newValue)
                                  bodyFn.resolve(ctx).flatMap {
                                    case (nk: Any, nv: Any) => ZIO.succeed((nk, nv))
                                    case other =>
                                      ZIO.fail(
                                        DynaLensError(
                                          s"Map body must return (key, value) tuple, got: ${other.getClass.getSimpleName}"
                                        )
                                      )
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    } else {
                      // Existing list/seq path unchanged
                      withCollectionSymbol(ctx, collName, iterableE) {
                        ZIO.foreach(iterableE) { item =>
                          // Bind "this" to the current element before descending
                          val localCtx = ctx.updatedWith("this", (item, elemLens))
                          withLoopSymbol(ctx, collName, item, elemLens) {
                            processPaths(rest, item, elemLens, localCtx, outer, bodyFn)
                          }
                        }
//                        ZIO.foreach(iterableE) { item =>
//                          withLoopSymbol(ctx, collName, item, elemLens) {
//                            processPaths(rest, item, elemLens, ctx, outer, bodyFn)
//                          }
//                        }
                      }
                    }
                  }

                  updatedRef <- {
                    if isMap then {
                      // Rebuild a real Map from updated entries and write it back
                      val rebuilt = updatedIterable.asInstanceOf[List[(Any, Any)]].toMap
                      lens._updateValue(pathParts, rebuilt, refObj.asInstanceOf[lens.ThisT])
                    } else {
                      // Existing list writeback
                      lens._updateValue(pathParts, updatedIterable, refObj.asInstanceOf[lens.ThisT])
                    }
                  }
                } yield updatedRef

              case Nil =>
                ZIO.fail(DynaLensError("Should Never Happen(tm)"))
            }
          } yield res
      }

    val parsed = Path.parsePath(path)
    println(s"[map] parsed path = ${parsed.mkString("→")}")

    val prefix = prefixUntilFirstCollection(parsed)
    println(s"[map] prefixUntilFirstCollection = ${prefix.mkString("→")}")

    for {
      seed <- walkPath(prefix, obj, this)
      _ = println(s"[map] seed keys: ${seed.keys.mkString(",")}")
      _ = seed.foreach { case (k, (v, _)) => println(s"[map]   seed[$k] = ${Option(v).map(_.getClass.getSimpleName).getOrElse("null")}") }

      splitPaths = splitIntoLevels(parsed)
      _ = println(s"[map] splitPaths = ${splitPaths.map(_.mkString("→")).mkString(" | ")}")

      updated <- processPaths(splitPaths, obj, Some(this), seed, outerCtx, fn.asInstanceOf[Fn[Any]])
    } yield updated.asInstanceOf[T]
//    val parsed = Path.parsePath(path)
//    for {
//      // establish a loop frame/bindings for the full path (top, etc.)
//      ctx <- walkPath(parsed, obj, this)
//      splitPaths = splitIntoLevels(parsed)
//      updated <- processPaths(splitPaths, obj, Some(this), ctx, outerCtx, fn.asInstanceOf[Fn[Any]])
//    } yield updated.asInstanceOf[T]
  }

object DynaLens:

  inline def dynalens[T]: DynaLens[T] = ${ generateDynaLensImpl[T] }

  private def generateDynaLensImpl[T: Type](using Quotes): Expr[DynaLens[T]] =
    val ctx = DynaLensBuildContext(summon[Quotes])
    buildDynaLens[T](ctx)

  private def buildDynaLens[T: Type](ctx: DynaLensBuildContext): Expr[DynaLens[T]] =
    import ctx.quotes.reflect.*
    given Quotes = ctx.quotes
    ReflectOnType[T](ctx.quotes)(TypeRepr.of[T])(using scala.collection.mutable.Map.empty[TypedName, Boolean]) match {
      case s: ScalaClassRef[T] if s.isCaseClass =>
        val getLambdaExpr = generateGetLambda[T](ctx, s.fields)
        val updateLambdaExpr = generateUpdateLambda[T](ctx, s.fields)

        // Build recursive registry of DynaLens for any fields of this class which are also case classes
        val pairs: List[Expr[(String, DynaLens[?])]] = s.fields.flatMap { field =>
          field.fieldRef match {
            case c: ScalaClassRef[?] if c.isCaseClass && !c.isAppliedType && c.typeParamValues.isEmpty =>
              field.fieldRef.refType match
                case '[t] =>
                  val lensExpr = buildDynaLens[t](ctx)
                  val keyExpr  = Expr(field.name)
                  Some('{ $keyExpr -> $lensExpr })
            case tr: TraitRef[?] if tr.fields.nonEmpty =>
              tr.refType match
                case '[t] =>
                  val keyExpr  = Expr(field.name)
                  val lensExpr = buildTraitLens[t](ctx, tr)
                  Some('{ $keyExpr -> $lensExpr })
            case c: SeqRef[?] =>
              c.elementRef match
                case d: ScalaClassRef[?] if d.isCaseClass =>
                  d.refType match
                    case '[t] =>
                      val lensExpr = buildDynaLens[t](ctx)
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
                          val lensExpr = buildDynaLens[t](ctx)
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
              case _            => false
            f.name -> isElemOpt
          }
        val elemIsOptionalExpr: Expr[Map[String, Boolean]] =
          liftMapBoolean(ctx, elemOptPairs.toMap)
        val typeNameExpr = Expr(s.typedName.toString)
        val typeInfoExpr = liftTypeInfo(ctx, buildPathTree(s))
        val schemaExpr = Expr(Schema.build(s))

        '{ DynaLens[T]($updateLambdaExpr, $getLambdaExpr, $registryExpr, $typeNameExpr, $typeInfoExpr, $schemaExpr, $elemIsOptionalExpr) }

      case x => throw new Exception(s"Sorry, dynalens only supports Scala case classes but received ${x.name}")
    }

  private def buildTraitLens[T: Type](ctx: DynaLensBuildContext, tr: TraitRef[?]): Expr[DynaLens[T]] = {
    given Quotes = ctx.quotes

    val get = generateTraitGetLambda[T](ctx, tr.fields, tr)
    val set = generateTraitUpdateLambda[T](ctx, tr.fields, tr)
    val name = Expr(tr.typedName.toString)

    val pairs: List[Expr[(String, DynaLens[?])]] = tr.fields.flatMap { field =>
      field.fieldRef match {
        case c: ScalaClassRef[?] if c.isCaseClass && !c.isAppliedType && c.typeParamValues.isEmpty =>
          field.fieldRef.refType match
            case '[t] =>
              val lensExpr = buildDynaLens[t](ctx)
              val keyExpr = Expr(field.name)
              Some('{ $keyExpr -> $lensExpr })
        case tr: TraitRef[?] if tr.fields.nonEmpty =>
          tr.refType match
            case '[t] =>
              val keyExpr = Expr(field.name)
              val lensExpr = buildTraitLens[t](ctx, tr)
              Some('{ $keyExpr -> $lensExpr })
        case c: SeqRef[?] =>
          c.elementRef match
            case d: ScalaClassRef[?] if d.isCaseClass =>
              d.refType match
                case '[t] =>
                  val lensExpr = buildDynaLens[t](ctx)
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
                      val lensExpr = buildDynaLens[t](ctx)
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
    val typeInfoExpr: Expr[Map[String, Any]] = liftTypeInfo(ctx, buildPathTree(tr))
    val schemaExpr = Expr(Schema.build(tr))
    val elemOptPairs =
      tr.fields.map { f =>
        val isElemOpt = f.fieldRef match
          case s: SeqRef[?] => s.elementRef.isInstanceOf[OptionRef[?]]
          case _ => false
        f.name -> isElemOpt
      }
    val elemIsOptionalExpr: Expr[Map[String, Boolean]] =
      liftMapBoolean(ctx, elemOptPairs.toMap)

    '{ DynaLens[T]($set, $get, $registryExpr, $name, $typeInfoExpr, $schemaExpr, $elemIsOptionalExpr) }
  }

  private def generateTraitUpdateLambda[T: Type](
                                                  ctx: DynaLensBuildContext,
                                                  traitFields: List[FieldInfoRef],
                                                  tr: TraitRef[?]
                                                ): Expr[(String, Any, T) => zio.ZIO[Any, DynaLensError, T]] = {
    import ctx.quotes.reflect.*
    given Quotes = ctx.quotes

    val tpe = TypeRepr.of[T]
    val owner = Symbol.spliceOwner

    val mt = MethodType(List("field", "value", "target"))(
      _ => List(TypeRepr.of[String], TypeRepr.of[Any], tpe),
      _ => TypeRepr.of[zio.ZIO[Any, DynaLensError, T]]
    )

    Lambda(owner, mt, { (_, args) =>
      val fieldParam = args(0).asInstanceOf[Term]
      val valueParam = args(1).asInstanceOf[Term]
      val targetParam = args(2).asInstanceOf[Term]

      val cases: List[CaseDef] = traitFields.map { tf =>
        // for the trait's own field name, switch on child type and copy that field
        val innerName = tf.name

        // one CaseDef per sealed child that has that field
        val childCases: List[CaseDef] =
          tr.sealedChildren.collect {
            case c: ScalaClassRef[?] if c.isCaseClass && c.fields.exists(_.name == innerName) =>
              (c.refType, tf.fieldRef.refType) match
                case ('[childT], '[innerT]) =>
                  val childSym = Symbol.newVal(owner, "child", TypeRepr.of[childT], Flags.EmptyFlags, Symbol.noSymbol)
                  val updated = TypeApply(Select.unique(valueParam, "asInstanceOf"), List(TypeTree.of[innerT])).asExpr.asTerm
                  val copyArgs = c.fields.map { f =>
                    if f.name == innerName then NamedArg(f.name, updated)
                    else NamedArg(f.name, Select.unique(Ref(childSym), f.name))
                  }
                  CaseDef(
                    Typed(Bind(childSym, Wildcard()).asInstanceOf[Term], TypeTree.of[childT]),
                    None,
                    Apply(Select.unique(Ref(childSym), "copy"), copyArgs)
                  )
                case _ =>
                  // should never happen at runtime, but makes match exhaustive
                  throw new MatchError((c.refType, tf.fieldRef.refType))
          }.toList

        val trNameExpr = Expr(tr.typedName.toString)
        val innerNameExpr = Expr(innerName)

        // inner match returns T (actually the trait type, but we upcast to T via the outer copy site)
        val body: Term =
          Match(
            targetParam, // we’re updating the trait value itself
            childCases :+
              CaseDef(
                Wildcard(), None,
                '{ throw DynaLensError("No subtype of " + $trNameExpr + " defines field '" + $innerNameExpr + "'") }.asTerm
              )
          )

        // wrap in ZIO.succeed and key on the TRAIT FIELD NAME
        CaseDef(
          Literal(StringConstant(innerName)),
          None,
          '{ zio.ZIO.succeed(${ body.asExprOf[T] }) }.asTerm
        )
      }

      val fallback =
        CaseDef(Wildcard(), None,
          '{ zio.ZIO.fail(DynaLensError("Field not found (1): " + ${ fieldParam.asExprOf[String] })) }.asTerm
        )

      Match(fieldParam, cases :+ fallback)
    }).asExprOf[(String, Any, T) => zio.ZIO[Any, DynaLensError, T]]
  }

  private def generateTraitGetLambda[T: Type](
                                               ctx: DynaLensBuildContext,
                                               traitFields: List[FieldInfoRef],
                                               tr: TraitRef[?]
                                             ): Expr[(String, T) => zio.ZIO[Any, DynaLensError, Any]] = {
    import ctx.quotes.reflect.*
    given Quotes = ctx.quotes

    val tpe = TypeRepr.of[T]
    val owner = Symbol.spliceOwner

    val mt = MethodType(List("field", "target"))(
      _ => List(TypeRepr.of[String], tpe),
      _ => TypeRepr.of[zio.ZIO[Any, DynaLensError, Any]]
    )

    Lambda(owner, mt, { (_, args) =>
      val fieldParam = args(0).asInstanceOf[Term]
      val targetParam = args(1).asInstanceOf[Term]

      val cases = traitFields.map { tf =>
        val innerName = tf.name

        val childGets: List[CaseDef] =
          tr.sealedChildren.collect {
            case c: ScalaClassRef[?] if c.isCaseClass && c.fields.exists(_.name == innerName) =>
              c.refType match
                case '[childT] =>
                  val childSym = Symbol.newVal(owner, "child", TypeRepr.of[childT], Flags.EmptyFlags, Symbol.noSymbol)
                  CaseDef(
                    Typed(Bind(childSym, Wildcard()).asInstanceOf[Term], TypeTree.of[childT]),
                    None,
                    Select.unique(Ref(childSym), innerName)
                  )
          }.toList

        val trNameExpr = Expr(tr.typedName.toString)
        val innerNameExpr = Expr(innerName)

        val body: Term =
          Match(
            targetParam,
            childGets :+
              CaseDef(
                Wildcard(), None,
                '{ throw DynaLensError("No subtype of " + $trNameExpr + " defines field '" + $innerNameExpr + "'") }.asTerm
              )
          )

        CaseDef(
          Literal(StringConstant(innerName)),
          None,
          '{ zio.ZIO.succeed(${ body.asExpr }) }.asTerm
        )
      }

      val fallback =
        CaseDef(Wildcard(), None,
          '{ zio.ZIO.fail(DynaLensError("Field not found (2): " + ${ fieldParam.asExprOf[String] })) }.asTerm
        )

      Match(fieldParam, cases :+ fallback)
    }).asExprOf[(String, T) => zio.ZIO[Any, DynaLensError, Any]]
  }

  private def generateGetLambda[T: Type](ctx: DynaLensBuildContext, classFields: List[FieldInfoRef]): Expr[(String, T) => ZIO[Any, DynaLensError, Any]] =
    import ctx.quotes.reflect.*
    given Quotes = ctx.quotes

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
            '{ ZIO.fail(DynaLensError("Field not found (3): " + ${ fieldParam.asExprOf[String] })) }.asTerm
          )
        )

        matchExpr
      }
    ).asExprOf[(String, T) => ZIO[Any, DynaLensError, Any]]

  private def generateUpdateLambda[T: Type](ctx: DynaLensBuildContext, classFields: List[FieldInfoRef]): Expr[(String, Any, T) => ZIO[Any, DynaLensError, T]] =
    import ctx.quotes.reflect.*
    given Quotes = ctx.quotes

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
        val cases: List[CaseDef] = classFields.flatMap { field =>
          field.fieldRef match
            case tr: TraitRef[?] if tr.sealedChildren.nonEmpty =>
              field.fieldRef.refType match
                case '[traitT] =>
                  val updatedValue: Term =
                    TypeApply(Select.unique(valueParam, "asInstanceOf"), List(TypeTree.of[traitT])).asExpr.asTerm
                  Some(standardCaseDef[T](ctx)(field, updatedValue, targetParam, fields))
                case _ => None

            case opt: ScalaOptionRef[?] =>
              val innerRTypeRef = opt.optionParamType
              innerRTypeRef.refType match
                case '[innerT] =>
                  val optionType = opt.refType
                  val valueExpr  = valueParam.asExprOf[Any]
                  val wrappedExpr: Expr[Option[innerT]] = '{
                    val v = $valueExpr
                    v match
                      case o: Option[?] => o.asInstanceOf[Option[innerT]]
                      case null         => None
                      case other        => Some(other.asInstanceOf[innerT])
                  }
                  val updatedValue = wrappedExpr.asExprOf(using optionType).asTerm
                  Some(standardCaseDef[T](ctx)(field, updatedValue, targetParam, fields))

            case _ =>
              val fieldType = field.fieldRef.refType
              val updatedValue = TypeApply(
                Select.unique(valueParam, "asInstanceOf"),
                List(TypeTree.of(using fieldType))
              ).asExpr.asTerm
              Some(standardCaseDef[T](ctx)(field, updatedValue, targetParam, fields))
        }

        val fallback = CaseDef(
          Wildcard(),
          None,
          '{
            ZIO.fail[DynaLensError](DynaLensError("Field not found (4): " + ${ fieldParam.asExprOf[String] }))
          }.asTerm
        )
        Match(fieldParam, cases :+ fallback)
      }
    ).asExprOf[(String, Any, T) => zio.ZIO[Any, DynaLensError, T]]


  private def standardCaseDef[T: scala.quoted.Type](
                                                     ctx: DynaLensBuildContext
                                                   )(
                                                     field: co.blocke.scala_reflection.reflect.rtypeRefs.FieldInfoRef,
                                                     updatedValue: ctx.quotes.reflect.Term,
                                                     targetParam: ctx.quotes.reflect.Term,
                                                     parentFields: List[ctx.quotes.reflect.Symbol]
                                                   ): ctx.quotes.reflect.CaseDef = {
    given scala.quoted.Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    val name = field.name

    val copyArgs =
      parentFields.map { f =>
        if f.name == name then NamedArg(f.name, updatedValue)
        else NamedArg(f.name, Select.unique(targetParam, f.name))
      }

    val updatedExpr = Apply(Select.unique(targetParam, "copy"), copyArgs)

    // we have [T: Type] so this compiles:
    CaseDef(
      Literal(StringConstant(name)),
      None,
      '{ zio.ZIO.succeed(${ updatedExpr.asExprOf[T] }) }.asTerm
    )
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
        val key = f.name
        val fieldType = f.fieldRef

        def valueShapeOf(ref: RTypeRef[?]): Any = ref match {
          case scr: ScalaClassRef[?] =>
            buildPathTree(scr) + ("__type" -> "{}")
          case tr: TraitRef[?] if tr.fields.nonEmpty =>
            // NEW: treat trait like a product of its declared fields
            buildPathTree(tr) + ("__type" -> "{}")
          case s: SeqRef[?] =>
            Map("__type" -> "[]", "__elemType" -> valueShapeOf(s.elementRef))
          case m: MapRef[?] =>
            Map("__type" -> "{}", "__valType" -> valueShapeOf(m.elementRef2))
          case _ => ""
        }

        val node: Any = fieldType match {
          case o: OptionRef[?] =>
            o.optionParamType match {
              case scr: ScalaClassRef[?] =>
                buildPathTree(scr) + ("__type" -> "{}?")
              case tr: TraitRef[?] if tr.fields.nonEmpty =>
                buildPathTree(tr) + ("__type" -> "{}?")
              case s: SeqRef[?] =>
                Map("__type" -> "[]?", "__elemType" -> valueShapeOf(s.elementRef))
              case m: MapRef[?] =>
                Map("__type" -> "{}?", "__valType" -> valueShapeOf(m.elementRef2))
              case _ => "?"
            }

          case s: SeqRef[?] =>
            Map("__type" -> "[]", "__elemType" -> valueShapeOf(s.elementRef))

          case m: MapRef[?] =>
            Map("__type" -> "{}", "__valType" -> valueShapeOf(m.elementRef2))

          case scr: ScalaClassRef[?] =>
            buildPathTree(scr)

          case tr: TraitRef[?] if tr.fields.nonEmpty =>
            // NEW: allow direct trait references as nested maps
            buildPathTree(tr)

          case _ => ""
        }

        key -> node
      }.toMap

    // NEW: allow TraitRef at the root (e.g. when top-level type is a trait)
    case tr: TraitRef[?] if tr.fields.nonEmpty =>
      tr.fields.map(f => f.name -> "").toMap

    case _ => Map.empty
  }

  private def liftTypeInfo(ctx: DynaLensBuildContext, map: Map[String, Any]): Expr[Map[String, Any]] = {
    given Quotes = ctx.quotes
    val liftedPairs: List[Expr[(String, Any)]] = map.toList.map {
      case (k, v: String) =>
        '{ Tuple2(${ Expr(k) }, ${ Expr(v) }) }
      case (k, v: Map[String @unchecked, Any @unchecked]) =>
        val nested: Expr[Map[String, Any]] = liftTypeInfo(ctx, v)
        '{ Tuple2(${ Expr(k) }, $nested) }
      case (k, _) =>
        quotes.reflect.report.error(s"Unsupported type for key: $k"); '{ ??? }
    }

    val liftedListExpr: Expr[List[(String, Any)]] = Expr.ofList(liftedPairs)
    '{ Map[String, Any]().++($liftedListExpr) }
  }

  private def liftMapBoolean(ctx: DynaLensBuildContext, map: Map[String, Boolean]): Expr[Map[String, Boolean]] =
    given Quotes = ctx.quotes
    val pairs: List[Expr[(String, Boolean)]] =
      map.toList.map { case (k, v) => '{ (${ Expr(k) }, ${ Expr(v) }) } }
    '{ Map[String, Boolean](${ Varargs(pairs) }*) }
