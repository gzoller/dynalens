package co.blocke.dynalens
package fn

import zio.*


// (For ElseFn)
sealed trait ElseFallback
case object FailSentinel extends ElseFallback


case class UUIDFn(posStr: String) extends Fn[Any]:
  override val methodName = "uuid"
  override val recv: Fn[Any] = RootFn
  override val args: List[Fn[Any]] = Nil
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (String, Lens)] =
    ZIO.succeed((java.util.UUID.randomUUID().toString, ScalarLens("uuid", false, None)))


case class ElseFn(recv: Fn[Any], default: Fn[Any], posStr: String) extends MethodFn[Any]:
  override val methodName = "else"
  override def args: List[Fn[Any]] = List(default)

  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(recv = kids(0).asInstanceOf[Fn[Any]], default = kids(1).asInstanceOf[Fn[Any]], posStr = posStr)

  // Update ElseFn.resolve to handle FailSentinel fallback:
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    for
      (optVal, rLens) <- recv.resolve(ctx)
      res <- optVal match
        case o: Option[?] =>
          o match
            case Some(v) => ZIO.succeed((v, rLens))

            case None =>
              default.resolve(ctx).flatMap {
                case (FailSentinel, _) =>
                  ZIO.fail(
                    DynaLensError(
                      posStr,
                      s"Required value missing before else(fail)"
                    )
                  )
                case other =>
                  ZIO.succeed(other)
              }

        case null =>
          default.resolve(ctx).flatMap {
            case (FailSentinel, _) =>
              ZIO.fail(
                DynaLensError(
                  posStr,
                  s"Receiver null before else(fail)"
                )
              )
            case other =>
              ZIO.succeed(other)
          }

        case other =>
          ZIO.fail(
            DynaLensError(
              posStr,
              s"else() requires Option receiver, got ${other.getClass.getSimpleName}"
            )
          )
    yield res


case class BlockFn[R](statements: Seq[Statement], finalFn: Fn[R], posStr: String) extends Fn[R]:
  override val methodName: String = "{block}"

  // A block doesn't have a meaningful runtime receiver; it's a sequence of statements
  // culminating in a final expression. Treat the root as the receiver.
  override def recv: Fn[Any] = RootFn

  // Only the final function is a child argument in the expression tree.
  override def args: List[Fn[Any]] = List(finalFn.asInstanceOf[Fn[Any]])

  override def children: List[Fn[?]] = List(finalFn)

  // Rebuild with a (single) final function child.
  override def rebuild(kids: List[Fn[?]]): Fn[R] =
    kids match
      case f :: Nil => copy(finalFn = f.asInstanceOf[Fn[R]])
      case _        => this

  // Block optionality mirrors the final expression's optionality.
  override val isOptional: Boolean = finalFn.isOptional

  // Execute statements in order, threading the DynaContext; return the value of the final Fn.
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (R, Lens)] =
    val staged: ZIO[RuntimeEnv, DynaLensError, DynaContext] =
      statements.foldLeft(ZIO.succeed(ctx): ZIO[RuntimeEnv, DynaLensError, DynaContext]) {
        (acc, stmt) => acc.flatMap(ctx2 => stmt.resolve(ctx2))
      }
    staged.flatMap(finalFn.resolve)


case class CaseWhenFn(
                       receiver: Fn[Any],
                       cases: Vector[(Any, Fn[Any])],
                       default: Option[Fn[Any]],
                       permissive: Boolean = false,
                       posStr: String
                     ) extends Fn[Any]:

  override val recv: Fn[Any] = receiver
  override def args: List[Fn[Any]] =
    receiver :: (cases.map(_._2).toList ++ default.toList)

  override def children: List[Fn[?]] = args

  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    val rcv = kids.head.asInstanceOf[Fn[Any]]
    val rhsCount = cases.length
    val newCases =
      cases.indices.map(i => (cases(i)._1, kids(i + 1).asInstanceOf[Fn[Any]])).toVector
    val newDefault =
      if default.isDefined then Some(kids(rhsCount + 1).asInstanceOf[Fn[Any]]) else None
    copy(receiver = rcv, cases = newCases, default = newDefault)

  override val isOptional: Boolean =
    receiver.isOptional || cases.exists(_._2.isOptional) || default.exists(_.isOptional)

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    for
      (v, _) <- receiver.resolve(ctx)
      result <- cases.collectFirst { case (p, rhs) if v == p => rhs.resolve(ctx) } match
        case Some(matched) => matched
        case None =>
          default match
            case Some(defFn) => defFn.resolve(ctx)
            case None if permissive => receiver.resolve(ctx)
            case None =>
              ZIO.fail(DynaLensError(posStr, s"No case matched for value: $v"))
    yield result


/** Absolute base receiver for standalone or constant expressions. */
case object RootFn extends Fn[Any]:
  // Since RootFn is the origin of all trees, its receiver is itself.
  override val recv: Fn[Any] = this
  override val args: List[Fn[Any]] = Nil
  override val methodName: String = "<root>"
  override val posStr: String = "<root>"
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    ZIO.succeed(((), ScalarLens("<root>", false, None)))


case object NoneFn extends Fn[Any]:
  override val recv: Fn[Any] = RootFn
  override val args: List[Fn[Any]] = Nil
  override val methodName: String = "<none>"
  val posStr = ""
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    ZIO.succeed((None, ScalarLens("<none>", true, None)))

case object NullFn extends Fn[Any]:
  override val recv: Fn[Any] = RootFn
  override val args: List[Fn[Any]] = Nil
  override val methodName: String = "<null>"
  val posStr = ""
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    ZIO.succeed((null, ScalarLens("<null>", true, None)))

case class BooleanConstantFn(value: Boolean) extends Fn[Boolean] with BooleanFn:
  override val recv: Fn[Any] = RootFn
  override val args: List[Fn[Any]] = Nil
  override val methodName: String = "<bool>"
  val posStr = ""
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] = this
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    ZIO.succeed((value, ScalarLens("<bool>", false, None)))

case class ConstantFn[R](out: R) extends Fn[R]:
  override val recv: Fn[Any] = RootFn
  override val args: List[Fn[Any]] = Nil
  override val methodName: String = "<const>"
  val posStr = ""
  override def rebuild(kids: List[Fn[?]]): Fn[R] = this
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (R, Lens)] =
    ZIO.succeed((out, ScalarLens("<const>", false, None)))


case class Tuple2Fn(recv: Fn[Any], args: List[Fn[Any]], posStr: String)
  extends BinaryFn[Any]:

  override val methodName: String = "<tuple2>"
  override val isOptional: Boolean =
    recv.isOptional || args.exists(_.isOptional)

  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    kids match
      case r :: a :: Nil =>
        copy(recv = r.asInstanceOf[Fn[Any]], args = List(a.asInstanceOf[Fn[Any]]))
      case _ => this

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, ((Any, Any), Lens)] =
    for
      (a, aLens) <- recv.resolve(ctx)
      (b, _)     <- args.head.resolve(ctx)
    yield ((a, b), aLens)


case class LoopFn(inner: Fn[Any], posStr: String)
  extends Fn[List[Any]]:

  override val recv: Fn[Any] = RootFn               // implicit receiver: "this"
  override val args: List[Fn[Any]] = List(inner)    // body of the loop
  override val methodName: String = "<loop>"

  override val isOptional: Boolean = inner.isOptional

  override def children: List[Fn[?]] = List(inner)
  override def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    kids match
      case h :: Nil => copy(inner = h.asInstanceOf[Fn[Any]])
      case _        => this

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (List[Any], Lens)] =
    ctx.get("this") match
      case Some((raw, lens)) =>
        for
          seq <- ZIO.fromEither(FnUtils.asSeq(raw, "loop", posStr))
            .mapError(_ => DynaLensError(posStr, s"LoopFn expected an Iterable for 'this', got ${raw.getClass.getSimpleName}"))
          results <- ZIO.foreach(seq) { item =>
            ctx.withThisScoped(item, lens) { scoped =>
              inner.resolve(scoped).map(_._1)
            }
          }
        yield (results.toList, lens)

      case None =>
        ZIO.fail(DynaLensError(posStr, "LoopFn requires 'this' bound to a collection"))


// TODO: Deprecate this--shouldn't be needed with lens chaining
case class PolyFn(recv: Fn[Any], args: List[Fn[Any]], posStr: String) extends Fn[Any]:
  override val methodName: String = "<poly>"
  override val isOptional: Boolean =
    recv.isOptional || args.exists(_.isOptional)
  override def children: List[Fn[?]] =
    recv :: args

  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    kids match
      case r :: tail =>
        copy(recv = r.asInstanceOf[Fn[Any]], args = tail.asInstanceOf[List[Fn[Any]]])
      case _ =>
        this

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    ctx.get("this") match
      case Some((origVal, lens)) =>
        for
          _ <- ZIO.foreachDiscard(args) { fn =>
            fn.resolve(ctx).map { case (resVal, _) => ctx.bind("this", resVal, lens) }
          }
          finalVal <- ctx.get("this") match {
            case Some((v, _)) => ZIO.succeed(v)
            case None => ZIO.fail(DynaLensError(posStr, "'this' unexpectedly missing from context"))
          }
        yield (finalVal, lens)
      case None =>
        ZIO.fail(DynaLensError(posStr, "'this' not found in context"))


case class GetFn(
                  path: String,
                  override val isOptional: Boolean,
                  recv: Fn[Any],     // always defined, never None
                  posStr: String
                ) extends Fn[Any]:

  import Path.* // for parsePath, PathElement, Field, IndexedField, partialPath

  override val args: List[Fn[Any]] = Nil
  override val methodName: String = "<get>"

  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    kids match
      case r :: Nil => copy(recv = r.asInstanceOf[Fn[Any]])
      case _        => this

  // ------------------------------------------------------------------
  // Core resolve: lens-first, no reflection fallback
  // ------------------------------------------------------------------
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    val parts = parsePath(path)

    // Decide anchor: symbol -> this -> top, including index anchors
    val anchored: Option[(Any, Lens, List[PathElement])] = parts match
      // Symbol anchor + index
      case PathElement(Some(firstName), Some(idxStr)) :: rest =>
        ctx.get(firstName) match
          case Some((v, l)) =>
            val idxElem = PathElement(None, Some(idxStr))
            Some((v, l, idxElem :: rest))
          case None =>
            ctx.get("this") match
              case Some((v, l)) => Some((v, l, parts))
              case None =>
                ctx.get("top").map { case (v, l) => (v, l, parts) }

      // Symbol anchor (no index)
      case PathElement(Some(firstName), None) :: rest =>
        ctx.get(firstName) match
          case Some((v, l)) => Some((v, l, rest))
          case None =>
            ctx.get("this") match
              case Some((v, l)) => Some((v, l, parts))
              case None =>
                ctx.get("top").map { case (v, l) => (v, l, parts) }

      // Index-only first segment: try this/top
      case PathElement(None, Some(_)) :: _ =>
        ctx.get("this")
          .map { case (v, l) => (v, l, parts) }
          .orElse(ctx.get("top").map { case (v, l) => (v, l, parts) })

      // No usable anchor, fall back to this/top
      case _ =>
        ctx.get("this")
          .map { case (v, l) => (v, l, parts) }
          .orElse(ctx.get("top").map { case (v, l) => (v, l, parts) })

    // DEBUG[GetFn:anchor]
    //   fullPath      = """" + path + """"
    //   parsedParts   = """" + parts.toString + """"
    //   anchored?     = """" + anchored.isDefined + """"
    //   baseLens      = """" + anchored.map(_._2.getClass.getSimpleName).getOrElse("<none>") + """"
    //   remainingPath = """" + anchored.map(_._3).getOrElse(Nil).toString + """"
    anchored match
      case None =>
        if isOptional then ZIO.succeed((None, ScalarLens("<get>", true, None)))
        else ZIO.fail(DynaLensError(posStr, s"No anchor found for path '$path' (missing symbol/this/top)"))

      case Some((baseVal, baseLens, tail)) =>
        // Validate anchor lens presence: a symbol/anchor must provide a lens.
        if baseLens == null then
          if isOptional then
            ZIO.succeed((None, ScalarLens("<get>", true, None)))
          else
            ZIO.fail(DynaLensError(posStr, s"Anchor for '$path' is missing a lens"))
        else if baseVal == null then
          ZIO.fail(DynaLensError(posStr, s"Receiver for '$path' is null"))
        else baseVal match
          case None =>
            if isOptional then ZIO.succeed((None, ScalarLens("<get>", true, None)))
            else ZIO.fail(DynaLensError(posStr, s"Receiver for '$path' is None"))
          case Some(inner) =>
            // Unwrap Option receiver and continue
            resolveWithLens(inner, baseLens, tail)
          case nonOpt =>
            resolveWithLens(nonOpt, baseLens, tail)

  // Derive the terminal lens by walking the entire remaining path
  private def deriveTerminalLens(start: Lens, tail: List[PathElement]): Lens =
    tail match
      case Nil => start
      case PathElement(Some(field), _) :: rest =>
        val next: Lens = start match
          case cl: ClassLens =>
            cl.fields.getOrElse(field, ScalarLens(field, isOptional = false, cl.parent))
          case ll: ListLens =>
            ll.elementLens match
              case cl: ClassLens => cl.fields.getOrElse(field, ScalarLens(field, isOptional = false, cl.parent))
              case _              => ScalarLens(field, isOptional = false, ll.elementLens.parent)
          case ml: MapLens =>
            ml.valueLens match
              case cl: ClassLens => cl.fields.getOrElse(field, ScalarLens(field, isOptional = false, cl.parent))
              case _              => ScalarLens(field, isOptional = false, ml.valueLens.parent)
          case other =>
            ScalarLens(field, isOptional = false, other.parent)
        deriveTerminalLens(next, rest)

      case PathElement(None, Some(_)) :: rest =>
        val next: Lens = start match
          case ll: ListLens => ll.elementLens
          case ml: MapLens  => ml.valueLens
          case _            => ScalarLens("<get>", isOptional = false, None)
        deriveTerminalLens(next, rest)

      // Impossible from parsePath — but present for exhaustivity safety
      case PathElement(None, None) :: _ =>
        throw new IllegalStateException(
          s"Invalid path element encountered in GetFn: PathElement(None,None)"
        )

  // Drive resolution using the provided lens; no reflection fallback.
  private def resolveWithLens(base: Any, lens: Lens, tail: List[PathElement])
  : ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    val resultZ: ZIO[Any, DynaLensError, Any] =
      if tail.isEmpty then ZIO.succeed(base)
      else lens.get(tail, base)

    val widened: ZIO[RuntimeEnv, DynaLensError, Any] = resultZ.mapError(e => e)

    // Normalize null / nested Option cases into a clean Option
    def normalize(v: Any): Any = v match
      case null            => None
      case None            => None
      case Some(null)      => None
      case Some(None)      => None
      case Some(v2)        => Some(v2)
      case other           => Some(other)

    if isOptional then
      widened.fold(
        _ =>
          (None, ScalarLens("<get>", true, None): Lens), // Optional swallow
        { raw =>
          val normVal = normalize(raw)
          val outLens = deriveTerminalLens(lens, tail)
          // DEBUG: show lens chaining for optional branch
          // println(s"[GetFn DEBUG] optional=true baseLens=${lens.getClass.getSimpleName} tail=${tail} -> outLens=${outLens.getClass.getSimpleName}(${outLens.name})")
          (normVal, outLens)
        }
      )
    else
      widened.flatMap {
        case null =>
          ZIO.fail(DynaLensError(posStr, s"Null encountered for required access"))
        case None =>
          ZIO.fail(DynaLensError(posStr, s"Required map key missing"))
        case Some(null) =>
          ZIO.fail(DynaLensError(posStr, s"Required map value is null"))
        case Some(None) =>
          ZIO.fail(DynaLensError(posStr, s"Required map key missing"))
        case Some(v) =>
          val outLens = deriveTerminalLens(lens, tail)
          // DEBUG: show lens chaining for required branch
          // println(s"[GetFn DEBUG] optional=false baseLens=${lens.getClass.getSimpleName} tail=${tail} -> outLens=${outLens.getClass.getSimpleName}(${outLens.name})")
          ZIO.succeed((v, outLens))
        case other =>
          val outLens = deriveTerminalLens(lens, tail)
          // DEBUG: show lens chaining for required branch (non-option)
          // println(s"[GetFn DEBUG] optional=false baseLens=${lens.getClass.getSimpleName} tail=${tail} -> outLens=${outLens.getClass.getSimpleName}(${outLens.name})")
          ZIO.succeed((other, outLens))
      }


object NoOpFn extends Fn[Any]:
  override val methodName: String = "<noop>"
  override val recv: Fn[Any] = this
  override val args: List[Fn[Any]] = Nil
  override val posStr: String = "<noop>"

  override def children: List[Fn[?]] = Nil

  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this

  override def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    ZIO.fail(DynaLensError(posStr, "NoOpFn should never be resolved"))


// Special converter: Fn[Any]->BooleanFn
case class ToBooleanFn(inner: Fn[Any], posStr: String) extends BooleanFn:

  override val recv: Fn[Any] = inner
  override val args: List[Fn[Any]] = Nil
  override val methodName: String = "<toBoolean>"
  override val isOptional: Boolean = inner.isOptional

  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    kids match
      case f :: Nil => copy(inner = f.asInstanceOf[Fn[Any]])
      case _        => this

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (r, rLens) <- inner.resolve(ctx)
      res <- r match
        case b: Boolean =>
          ZIO.succeed(b)

        case Some(_: Boolean) =>
          ZIO.fail(DynaLensError(posStr,
            s"Optional Boolean cannot be used directly — add .else()"))

        case None =>
          ZIO.fail(DynaLensError(posStr,
            s"Boolean expression evaluated to None — add .else()"))

        case other =>
          ZIO.fail(DynaLensError(posStr,
            s"Expected Boolean at runtime, got ${other.getClass.getSimpleName} = $other"))
    yield (res, rLens)