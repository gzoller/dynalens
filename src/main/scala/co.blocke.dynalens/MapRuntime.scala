package co.blocke.dynalens

import co.blocke.dynalens.*

import zio.*

/** Shared runtime for statement and expression forms of => (map).
 * This is the single source of truth for iteration, scoping and writeback shaping.
 */
object MapRuntime:

  /** Map across a path starting from `root`, using `lens` to get/update values.
   * Preserves optionality of the receiver (Some/None).
   * - For Map receivers: predicate must yield (key, value) tuples; result is a Map (or Some(Map)).
   * - For List/Iterable receivers: predicate can yield a scalar (broadcast) or an Iterable of equal size; result is a List/Seq (or Some(List)).
   */
  def mapOver[T](
                  path: String,
                  predicate: Fn[Any],
                  root: T,
                  lens: DynaLens[T],
                  posStr: String,
                  outerCtx: DynaContext = DynaContext.empty
                ): ZIO[_BiMapRegistry, DynaLensError, T] = {

    // ---------- helpers ----------
    inline def tname(x: Any): String = Option(x).map(_.getClass.getName).getOrElse("null")

    def isMapLike(v: Any): Boolean =
      v.isInstanceOf[scala.collection.immutable.Map[?, ?]] ||
        v.isInstanceOf[scala.collection.mutable.Map[?, ?]] ||
        v.isInstanceOf[Option[?]] && v.asInstanceOf[Option[?]].exists(_.isInstanceOf[scala.collection.Map[?, ?]])

    def entriesOfMapLike(v: Any): Either[DynaLensError, List[(Any, Any)]] =
      v match
        case m: scala.collection.Map[?, ?] @unchecked =>
          Right(m.asInstanceOf[scala.collection.Map[Any, Any]].toList)
        case Some(m: scala.collection.Map[?, ?] @unchecked) =>
          Right(m.asInstanceOf[scala.collection.Map[Any, Any]].toList)
        case None =>
          Right(Nil)
        case other =>
          Left(DynaLensError(posStr, s"Expected Map/Option[Map], got: ${tname(other)}"))

    def itemsOfIterLike(v: Any): Either[DynaLensError, List[Any]] =
      v match
        case it: Iterable[?] =>
          Right(it.asInstanceOf[Iterable[Any]].toList)
        case Some(it: Iterable[?]) =>
          Right(it.asInstanceOf[Iterable[Any]].toList)
        case None =>
          Right(Nil)
        case other =>
          Left(DynaLensError(posStr, s"Expected Iterable/Option[Iterable], got: ${tname(other)}"))

    /* Core: process a split path (levels) against refObj using lens/registry and predicate */
    def processPaths(
      paths: List[List[Path.PathElement]],
      refObj: Any,
      dynalensOpt: Option[DynaLens[?]],
      ctx: DynaContext,
      outer: DynaContext,
      predicateFn: Fn[Any]
    ): ZIO[_BiMapRegistry, DynaLensError, Any] =
      dynalensOpt match
        case None =>
          ZIO.fail(DynaLensError(posStr, "Internal: missing lens for map()"))

        case Some(l) =>
          paths match
            // =========================
            //           LEAF
            // =========================
            case pathParts :: Nil =>
              val partial = Path.partialPath(pathParts)
              val parent = pathParts.dropRight(1)
              val lastSeg = pathParts.last
              val lastName = lastSeg.name

              if parent.isEmpty then
                // -------- One-segment leaf: whole-field mapping --------
                for {
                  in <- l.getValue(pathParts, refObj.asInstanceOf[l.ThisT])

                  out <- if isMapLike(in) then
                    // ---- Map (or Option[Map]) whole-field ----
                    val wasOptional = in.isInstanceOf[Option[?]]
                    for {
                      entries <- ZIO.fromEither(entriesOfMapLike(in))
                      valueLens = l._registry.get(lastName)
                      results <- ZIO.foreach(entries) { case (k, v) =>
                        withKeyScoped(ctx, "key", (k, None)) {
                          withKeyScoped(ctx, "value", (v, valueLens)) {
                            withThisScoped(ctx, (k, v), valueLens) {
                              predicateFn.resolve(ctx)
                            }
                          }
                        }
                      }
                      tupleResults = results.collect { case (nk: Any, nv: Any) => (nk, nv) }
                      outRefEffect =
                        if (tupleResults.size == results.size) {
                          val rebuilt = tupleResults.toMap
                          val writeVal: Any = if wasOptional then Some(rebuilt) else rebuilt
                          l.update(partial, writeVal, refObj.asInstanceOf[l.ThisT])
                        } else if (tupleResults.isEmpty) {
                          val writeVal: Any = if wasOptional then Some(results) else results
                          l.update(partial, writeVal, refObj.asInstanceOf[l.ThisT])
                        } else {
                          ZIO.fail(DynaLensError(posStr, "Map predicate mixed tuple and non-tuple results; either return (k,v) for all entries or a scalar for all entries"))
                        }
                      outRef <- outRefEffect
                    } yield outRef

                  else
                    // ---- Non-map whole-field: scalar / option-aware writeback ----
                    val localCtx = outerCtx ++ ctx.set("this", (in, None)).toMap
                    for {
                      predicateOut <- predicateFn.resolve(localCtx)
                      valueToWrite <-
                        lastSeg match
                          case Path.Field(_) =>
                            val v = predicateOut match
                              case o: Option[?] => o
                              case null         => None
                              case x            => Some(x)
                            ZIO.succeed(v)
                          case _ =>
                            ZIO.fail(DynaLensError("", s"Unexpected non-Field leaf for '$partial'"))
                      outRef <- l.update(partial, valueToWrite, refObj.asInstanceOf[l.ThisT])
                    } yield outRef
                } yield out

              else
                // -------- Multi-segment leaf: per-element updates (e.g. "interest.qty") --------
                for {
                  parentVal <- l.getValue(parent, refObj.asInstanceOf[l.ThisT])

                  elems <- parentVal match
                    case it: Iterable[?] => ZIO.succeed(it.asInstanceOf[Iterable[Any]].toList)
                    case Some(it: Iterable[?]) => ZIO.succeed(it.asInstanceOf[Iterable[Any]].toList)
                    case None => ZIO.succeed(Nil)
                    case other => ZIO.fail(DynaLensError("", s"Expected iterable parent for '$partial', got ${tname(other)}"))

                  parentLens = l.lensForPathPrefix(l, parent.dropRight(1))
                  elemLensOpt = parentLens.flatMap(_._registry.get(parent.last.name))
                  elemLens <- ZIO.fromOption(elemLensOpt)
                    .orElseFail(DynaLensError("", s"No element lens registered for '${parent.last.name}' when accessing '$lastName'"))

                  localCtx = outerCtx ++ ctx.set("this", (elems, Some(elemLens))).toMap
                  predicateOut <- predicateFn.resolve(localCtx)

                  newVals <- predicateOut match
                    case it: Iterable[?] =>
                      val xs = it.asInstanceOf[Iterable[Any]].toList
                      if xs.size != elems.size then
                        ZIO.fail(DynaLensError("", s"Body returned ${xs.size} values for ${elems.size} elements at '$partial'"))
                      else ZIO.succeed(xs)
                    case scalar =>
                      ZIO.succeed(List.fill(elems.size)(scalar))

                  updatedElems <- ZIO.foreach(elems.zip(newVals)) { case (elem, newVal) =>
                    elemLens.update(lastName, newVal, elem.asInstanceOf[elemLens.ThisT]).map(_.asInstanceOf[Any])
                  }

                  outRef <- {
                    val parentKey = Path.partialPath(parent)
                    parentVal match
                      case Some(_: Iterable[?]) => l.update(parentKey, Some(updatedElems), refObj.asInstanceOf[l.ThisT])
                      case _ => l.update(parentKey, updatedElems, refObj.asInstanceOf[l.ThisT])
                  }
                } yield outRef

            // =========================
            //          DESCENT
            // =========================
            case pathParts :: rest =>
              val partial  = Path.partialPath(pathParts)
              val collName = pathParts.last.name

              for {
                collVal <- l.getValue(pathParts, refObj.asInstanceOf[l.ThisT])

                isMap = isMapLike(collVal)

                iterableEither =
                  if isMap then entriesOfMapLike(collVal).map(_.asInstanceOf[List[Any]])
                  else itemsOfIterLike(collVal).map(_.asInstanceOf[List[Any]])

                iterable <- ZIO.fromEither(iterableEither)

                parentLens = l.lensForPathPrefix(l, pathParts.dropRight(1))
                elemLens = parentLens.flatMap(_._registry.get(collName))

                _ <-
                  if rest.nonEmpty && elemLens.isEmpty && !isMap then
                    ZIO.fail(DynaLensError("", s"No lens registered for elements of '$collName' (needed to map nested path '$partial')"))
                  else ZIO.unit

                updatedIterable <-
                  if isMap then
                    // iterate entries (k, v)
                    withCollectionSymbol(ctx, collName, iterable) {
                      ZIO.foreach(iterable.asInstanceOf[List[(Any, Any)]]) { case (k, v) =>
                        withKeyScoped(ctx, "key", (k, None)) {
                          withKeyScoped(ctx, "value", (v, elemLens)) {
                            withThisScoped(ctx, (k, v), elemLens) {
                              if rest.nonEmpty then
                                processPaths(rest, v, elemLens, ctx, outer, predicateFn).map(nv => (k, nv))
                              else
                                predicateFn.resolve(ctx)
                            }
                          }
                        }
                      }
                    }
                  else
                    // iterate items
                    withCollectionSymbol(ctx, collName, iterable) {
                      ZIO.foreach(iterable) { item =>
                        val localCtx = ctx.updatedWith("this", (item, elemLens))
                        withLoopSymbol(ctx, collName, item, elemLens) {
                          processPaths(rest, item, elemLens, localCtx, outer, predicateFn)
                        }
                      }
                    }

                updatedRef <-
                  if isMap then
                    val tupleResults = updatedIterable.collect { case (nk: Any, nv: Any) => (nk, nv) }
                    if (tupleResults.size == updatedIterable.size) {
                      val rebuilt = tupleResults.toMap
                      l.updateValue(pathParts, rebuilt, refObj.asInstanceOf[l.ThisT])
                    } else if (tupleResults.isEmpty) {
                      l.updateValue(pathParts, updatedIterable, refObj.asInstanceOf[l.ThisT])
                    } else {
                      ZIO.fail(DynaLensError(posStr, "Map predicate mixed tuple and non-tuple results; either return (k,v) for all entries or a scalar for all entries"))
                    }
                  else
                    l.updateValue(pathParts, updatedIterable, refObj.asInstanceOf[l.ThisT])

              } yield updatedRef

            case Nil =>
              ZIO.fail(DynaLensError("", "Should Never Happen(tm)"))


    // ---------- entry ----------
    val parsed  = Path.parsePath(path)
    val prefix  = prefixUntilFirstCollection(parsed)

    for {
      seed       <- lens.walkPath(prefix, root)
      splitPaths  = splitIntoLevels(parsed)
      updated    <- processPaths(splitPaths, root, Some(lens), seed, outerCtx, predicate)
    } yield updated.asInstanceOf[T]
  }

  // --- helper methods for path splitting ---
  private def prefixUntilFirstCollection(path: List[Path.PathElement]): List[Path.PathElement] =
    path.takeWhile {
      case _: Path.IndexedField => false
      case _                    => true
    }

  /** Split a full path into iteration "levels" — each sublist ends right before or at a collection boundary. */
  private def splitIntoLevels(path: List[Path.PathElement]): List[List[Path.PathElement]] =
    if path.isEmpty then Nil
    else
      val buf = scala.collection.mutable.ListBuffer[List[Path.PathElement]]()
      val current = scala.collection.mutable.ListBuffer[Path.PathElement]()

      path.foreach {
        case f @ Path.IndexedField(_, _) =>
          // Include the collection field as end of current segment
          current += f
          buf += current.toList
          current.clear()
        case p =>
          current += p
      }

      if current.nonEmpty then buf += current.toList
      buf.toList
