package co.blocke.dynalens
package fn

import zio.*

import scala.annotation.tailrec
import util.NumPromote


// (For ElseFn)
sealed trait ElseFallback
case object FailSentinel extends ElseFallback


case class UUIDFn(posStr: String) extends Fn[Any]:
  override val methodName = "uuid"
  override val recv: Fn[Any] = RootFn
  override val args: List[Fn[Any]] = Nil
  val resultType: FieldType = ScalarType("uuid", "java.lang.String", false)
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (String, Lens)] =
    ZIO.succeed((java.util.UUID.randomUUID().toString, ScalarLens("uuid", false, None)))


case class ElseFn(recv: Fn[Any], default: Fn[Any], posStr: String) extends MethodFn[Any]:
  override val methodName = "else"
  override def args: List[Fn[Any]] = List(default)

  val resultType: FieldType = recv.resultType
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(recv = kids(0).asInstanceOf[Fn[Any]], default = kids(1).asInstanceOf[Fn[Any]], posStr = posStr)

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    for
      attempt <- recv.resolve(ctx).either
      res <- attempt match
        case Right((raw, rLens)) =>
          // Only allow else() if lens is optional
          if !rLens.isOptional then
            ZIO.fail(DynaLensError(posStr, "else() requires an Option"))
          else
            // Normalize raw into Option: null => None, Some(null) => None, non-option => Some(value)
            val normalized: Option[Any] = raw match
              case null         => None
              case Some(null)   => None
              case None         => None
              case opt: Option[?] => opt.asInstanceOf[Option[Any]] match
                case Some(null) => None
                case other      => other
              case other        => Some(other)
            normalized match
              case Some(v) if v != null =>
                ZIO.succeed((v, rLens))
              case _ =>
                default.resolve(ctx).flatMap {
                  case (FailSentinel, _) =>
                    ZIO.fail(DynaLensError(posStr, s"Required value missing before else(fail)"))
                  case (v2, _) =>
                    ZIO.succeed((v2, rLens))
                }
        case Left(_) =>
          // If lens is optional, resolve default; else error.
          val recoveredLensZ: ZIO[RuntimeEnv, DynaLensError, Lens] = recv match
            case g: GetFn =>
              g.copy(isOptional = true).resolve(ctx).map(_._2).orElse(ZIO.succeed(ScalarLens("<get>", true, None)))
            case _ =>
              ZIO.succeed(ScalarLens("<get>", true, None))

          recoveredLensZ.flatMap { rLens =>
            if !rLens.isOptional then
              ZIO.fail(DynaLensError(posStr, "else() requires an Option"))
            else
              default.resolve(ctx).flatMap {
                case (FailSentinel, _) =>
                  ZIO.fail(DynaLensError(posStr, s"Required value missing before else(fail)"))
                case (v, _) =>
                  ZIO.succeed((v, rLens))
              }
          }
    yield res


case class BlockFn[R](statements: Seq[Statement], finalFn: Fn[R], posStr: String) extends Fn[R]:
  override val methodName: String = "{block}"

  // A block doesn't have a meaningful runtime receiver; it's a sequence of statements
  // culminating in a final expression. Treat the root as the receiver.
  override def recv: Fn[Any] = RootFn

  // Only the final function is a child argument in the expression tree.
  override def args: List[Fn[Any]] = List(finalFn.asInstanceOf[Fn[Any]])

  val resultType: FieldType = finalFn.resultType
  override def children: List[Fn[?]] = List(finalFn)

  // Rebuild with a (single) final function child.
  override def rebuild(kids: List[Fn[?]]): Fn[R] =
    kids match
      case f :: Nil => copy(finalFn = f.asInstanceOf[Fn[R]])
      case _        => this

  // Block optionality mirrors the final expression's optionality.
  override val isOptional: Boolean = finalFn.isOptional

  // Execute statements in order, threading the DynaContext; return the value of the final Fn.
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (R, Lens)] = {
    val ctxWithTop: DynaContext =
      ctx.getTop match
        case Some(_) => ctx
        case None =>
          ctx.getThis match
            case Some((obj, lens)) => ctx.bind("top", obj, lens)
            case None =>
              // Fallback: use RootFn's unit object and lens as synthetic top
              ctx.bind("top", (), ScalarLens("<root>", false, None))

    val staged: ZIO[RuntimeEnv, DynaLensError, DynaContext] =
      statements.foldLeft(
        ZIO.succeed(ctxWithTop): ZIO[RuntimeEnv, DynaLensError, DynaContext]
      ) { (accZ, stmt) =>
        accZ.flatMap { accCtx =>
          stmt match
            case u: UpdateStmt[_] =>
              u match
                case UpdateStmt(symPath, valueFn, _, updateFieldType) if !symPath.contains(".") && accCtx.get(symPath).isDefined =>
                  valueFn.resolve(accCtx).map { case (newVal, _) =>
                    val coerced = NumPromote.toType(newVal, updateFieldType.typeName)
                    val (_, existingLens) = accCtx.get(symPath).getOrElse(coerced -> ScalarLens(symPath, false, None))
                    accCtx.bind(symPath, coerced, existingLens)
                  }
                case _ =>
                  u.resolve(accCtx)
            case other =>
              other.resolve(accCtx).as(accCtx)
        }
      }

    staged.flatMap { finalCtx =>
      finalFn.resolve(finalCtx).map { case (value, lens) =>
        val clean =
          lens match
            case s: ScalarLens => s.copy(parent = None)
            case c: ClassLens  => c.copy(parent = None)
            case l: ListLens   => l.copy(parent = None)
            case m: MapLens    => m.copy(parent = None)
            case e: EnumLens   => e.copy(parent = None)
        (value, clean)
      }
    }
  }


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

  val resultType: FieldType =
    (cases.headOption.map(_._2.resultType))
      .orElse(default.map(_.resultType))
      .getOrElse(ScalarType("", "java.lang.Object", true))
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
        case Some(matched) =>
          matched.flatMap { case (value, _) =>
            receiver.resolve(ctx).map { case (_, rLens) =>
              (value, rLens) // preserve the receiver lens
            }
          }
        case None =>
          default match
            case Some(defFn) =>
              defFn.resolve(ctx).flatMap { case (v, _) =>
                receiver.resolve(ctx).map { case (_, rLens) => (v, rLens) }
              }
            case None if permissive => receiver.resolve(ctx)
            case None =>
              ZIO.fail(DynaLensError(posStr, s"No case matched for value: $v"))
    yield result


/** Absolute base receiver for standalone or constant expressions. */
case object RootFn extends Fn[Any]:
  // Since RootFn is the origin of all trees, its receiver is itself.
  override val recv: Fn[Any] = this
  override val args: List[Fn[Any]] = Nil
  val resultType: FieldType = ScalarType("<root>", "scala.Unit", false)
  override val methodName: String = "<root>"
  override val posStr: String = "<root>"
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    ZIO.succeed(((), ScalarLens("<root>", false, None)))


case object NoneFn extends Fn[Any]:
  override val recv: Fn[Any] = RootFn
  override val args: List[Fn[Any]] = Nil
  val resultType: FieldType = ScalarType("<none>", "scala.Option[scala.Any]", true)
  override val methodName: String = "<none>"
  val posStr = ""
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    ZIO.succeed((None, ScalarLens("<none>", true, None)))

case object NullFn extends Fn[Any]:
  override val recv: Fn[Any] = RootFn
  override val args: List[Fn[Any]] = Nil
  val resultType: FieldType = ScalarType("<null>", "scala.Any", true)
  override val methodName: String = "<null>"
  val posStr = ""
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    ZIO.succeed((null, ScalarLens("<null>", true, None)))

case class BooleanConstantFn(value: Boolean) extends Fn[Boolean] with BooleanFn:
  override val recv: Fn[Any] = RootFn
  override val args: List[Fn[Any]] = Nil
  val resultType: FieldType = ScalarType("<bool>", "scala.Boolean", false)
  override val methodName: String = "<bool>"
  val posStr = ""
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] = this
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    ZIO.succeed((value, ScalarLens("<bool>", false, None)))

case class ConstantFn[R](out: R) extends Fn[R]:
  override val recv: Fn[Any] = RootFn
  override val args: List[Fn[Any]] = Nil
  val resultType: FieldType = ScalarType("<const>", out.getClass.getName, false)
  override val methodName: String = "<const>"
  val posStr = ""
  override def rebuild(kids: List[Fn[?]]): Fn[R] = this
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (R, Lens)] =
    ZIO.succeed((out, ScalarLens("<const>", false, None)))


case class Tuple2Fn(recv: Fn[Any], arg: Fn[Any], posStr: String)
  extends BinaryFn[Any]:

  override val methodName: String = "<tuple2>"
  override val isOptional: Boolean =
    recv.isOptional || args.exists(_.isOptional)
  val resultType: FieldType = ScalarType("<tuple2>", "(scala.Any, scala.Any)", false)

  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    kids match
      case r :: a :: Nil =>
        copy(recv = r.asInstanceOf[Fn[Any]], arg = a.asInstanceOf[Fn[Any]])
      case _ => this

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, ((Any, Any), Lens)] =
    for
      (a, aLens) <- recv.resolve(ctx)
      (b, _)     <- arg.resolve(ctx)
    yield ((a, b), aLens)


case class GetFn(
                  path: String,
                  override val isOptional: Boolean,
                  recv: Fn[Any],     // always defined, never None
                  posStr: String,
                  maybeType: Option[FieldType] = None
                ) extends Fn[Any]:

  import Path.* // for parsePath, PathElement, Field, IndexedField, partialPath

  override val args: List[Fn[Any]] = Nil
  val resultType: FieldType = maybeType.getOrElse(ScalarType(path, "scala.Any", isOptional))
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

    // Special case: direct symbols this_key / this_value
    if (path == "this_key" || path == "this_value") && !path.contains(".") && !path.contains("[") then
      ctx.get(path) match
        case Some((v, l)) =>
          return ZIO.succeed((v, l))
        case None =>
          return ZIO.fail(DynaLensError(posStr, s"No this binding found for '$path'"))

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

    anchored match
      case None =>
        if isOptional then ZIO.succeed((None, ScalarLens("<get>", true, None)))
        else ZIO.fail(DynaLensError(posStr, s"No anchor found for path '$path' (missing symbol/this/top)"))

      case Some((baseVal, baseLens, tail)) =>
        if baseLens == null then
          if isOptional then
            ZIO.succeed((None, ScalarLens("<get>", true, None)))
          else
            ZIO.fail(DynaLensError(posStr, s"Missing lens for required path '$path'"))
        else if baseVal == null then
          if isOptional then
            ZIO.succeed((None, baseLens))
          else
            ZIO.fail(DynaLensError(posStr, s"Receiver for '$path' is null"))
        else baseVal match
          case None =>
            if isOptional then
              ZIO.succeed((None, baseLens))
            else {
              val z = resolveWithLens(None, baseLens, tail)
              z.map { case (v, l) =>
                (v, l)
              }
            }
          case Some(inner) =>
            if inner == null && isOptional then
              ZIO.succeed((None, baseLens))
            else {
              val z = resolveWithLens(inner, baseLens, tail)
              z.map { case (v, l) =>
                (v, l)
              }
            }
          case nonOpt =>
            if tail.isEmpty then
              ZIO.succeed((nonOpt, baseLens))
            else {
              val z = resolveWithLens(nonOpt, baseLens, tail)
              z.map { case (v, l) =>
                (v, l)
              }
            }

  // Derive the terminal lens by walking the entire remaining path
  @tailrec
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
          val outLens = deriveTerminalLens(lens, tail)
          if tail.isEmpty then
            ZIO.succeed((None, outLens))
          else
            ZIO.fail(DynaLensError(posStr, s"Required map key missing"))
        case Some(null) =>
          ZIO.fail(DynaLensError(posStr, s"Required map value is null"))
        case Some(None) =>
          val outLens = deriveTerminalLens(lens, tail)
          if tail.isEmpty then
            ZIO.succeed((None, outLens))
          else
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
  val resultType: FieldType = ScalarType("<noop>", "scala.Unit", false)
  override val posStr: String = "<noop>"

  override def children: List[Fn[?]] = Nil

  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this

  override def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    ZIO.fail(DynaLensError(posStr, "NoOpFn should never be resolved"))


case class IndexFn(recv: Fn[Any], index: Fn[Any], pos: String) extends Fn[Any] {
  override def posStr: String = pos
  override def args: List[Fn[Any]] = List(recv, index)
  val resultType: FieldType = ScalarType("<index>", "scala.Any", true)
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    IndexFn(kids.head.asInstanceOf[Fn[Any]], kids(1).asInstanceOf[Fn[Any]], pos)

  override def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] = {
    val resultZ: ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] = recv match
      // --- Compile-time path concatenation for GetFn or nested IndexFn ---
      case g: GetFn =>
        for {
          recvVal <- g.resolve(ctx)
          idxRes  <- index.resolve(ctx)
          idx = idxRes._1
          res <- recvVal._1 match {
            case None =>
              ZIO.succeed((None, recvVal._2))

            case list: List[_] if idx.isInstanceOf[Int] =>
              ZIO.succeed((list.lift(idx.asInstanceOf[Int]), recvVal._2))

            case map: Map[_, _] =>
              ZIO.succeed((map.asInstanceOf[Map[Any, Any]].get(idx), recvVal._2))

            case Some(inner) =>
              inner match {
                case list: List[_] if idx.isInstanceOf[Int] =>
                  ZIO.succeed((list.lift(idx.asInstanceOf[Int]), recvVal._2))
                case map: Map[_, _] =>
                  ZIO.succeed((map.asInstanceOf[Map[Any, Any]].get(idx), recvVal._2))
                case _ =>
                  ZIO.fail(DynaLensError(pos, s"Cannot index Option containing ${inner.getClass.getSimpleName}"))
              }

            case v =>
              ZIO.fail(DynaLensError(pos, s"Cannot index value of type ${v.getClass.getSimpleName}"))
          }
        } yield res

      case i: IndexFn =>
        for {
          // First, resolve the inner IndexFn
          innerRes <- i.resolve(ctx)
          (innerVal, innerLens) = innerRes
          idxRes <- index.resolve(ctx)
          idx = idxRes._1
          res <- innerVal match {
            case list: List[_] if idx.isInstanceOf[Int] =>
              ZIO.succeed((list.lift(idx.asInstanceOf[Int]), innerLens))
            case map: Map[_, _] =>
              ZIO.succeed((map.asInstanceOf[Map[Any, Any]].get(idx), innerLens))
            case opt: Option[_] =>
              opt match {
                case None =>
                  ZIO.succeed((None, innerLens))
                case Some(None) =>
                  ZIO.succeed((None, innerLens))
                case Some(inner) =>
                  inner match {
                    case list: List[_] if idx.isInstanceOf[Int] =>
                      ZIO.succeed((list.lift(idx.asInstanceOf[Int]), innerLens))
                    case map: Map[_, _] =>
                      ZIO.succeed((map.asInstanceOf[Map[Any, Any]].get(idx), innerLens))
                    case _ =>
                      ZIO.fail(DynaLensError(pos, s"Cannot index Option containing ${inner.getClass.getSimpleName}"))
                  }
              }
            case v =>
              ZIO.fail(DynaLensError(pos, s"Cannot index value of type ${if (v == null) "null" else v.getClass.getSimpleName}"))
          }
        } yield res

      // --- Runtime evaluation cases ---
      case other =>
        for {
          recvVal  <- other.resolve(ctx)
          indexVal <- index.resolve(ctx)
          res <- (recvVal._1, indexVal._1) match {

            // ===== Option cases =====
            case (opt: Option[_], key) =>
              opt match
                case Some(inner) =>
                  (inner, key) match
                    case (list: List[_], i: Int) =>
                      ZIO.succeed((list.lift(i), recvVal._2))
                    case (map: Map[_, _], k) =>
                      ZIO.succeed((map.asInstanceOf[Map[Any,Any]].get(k), recvVal._2))
                    case _ =>
                      ZIO.fail(DynaLensError(pos,
                        s"Cannot index Option containing ${inner.getClass.getSimpleName}"
                      ))
                case None =>
                  ZIO.succeed((None, recvVal._2))

            // ===== Direct List / Map =====
            case (list: List[_], i: Int) =>
              ZIO.succeed((list.lift(i), recvVal._2)) // returns Option[Any]

            case (map: Map[_, _], k) =>
              ZIO.succeed((map.asInstanceOf[Map[Any,Any]].get(k), recvVal._2))

            // ===== Non-indexable receiver =====
            case (v, _) =>
              ZIO.fail(DynaLensError(pos,
                s"Cannot index value of type ${v.getClass.getSimpleName}"
              ))
          }
        } yield res
    resultZ.map { res =>
      res
    }
  }
}


case object IterThisFn extends Fn[Any]:
  override val recv: Fn[Any] = RootFn
  override val args: List[Fn[Any]] = Nil
  val resultType: FieldType = ScalarType("<iter-this>", "scala.Any", false)
  override val methodName: String = "<iter-this>"
  override val posStr: String = "<iter-this>"
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    ctx.get("this") match
      case Some((v, l)) =>
        ZIO.succeed((v, l))
      case None =>
        ZIO.fail(DynaLensError(posStr, s"No 'this' bound for iteration element"))