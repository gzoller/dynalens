package co.blocke.dynalens
package fn

import zio.*
import scala.annotation.tailrec


/** IF */
case class IfFn[T](condition: BooleanFn, ifTrue: Fn[T], ifFalse: Fn[T], posStr: String)
  extends Fn[T] {

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (T, Lens)] =
    for
      (condVal, lens) <- condition.resolve(ctx)
      res <- if condVal then ifTrue.resolve(ctx) else ifFalse.resolve(ctx)
    yield res

  override val recv: Fn[Any] = condition.asInstanceOf[Fn[Any]]
  override val args: List[Fn[Any]] = List(ifTrue.asInstanceOf[Fn[Any]], ifFalse.asInstanceOf[Fn[Any]])
  override def rebuild(kids: List[Fn[?]]): Fn[T] =
    copy(
      condition = kids.head.asInstanceOf[BooleanFn],
      ifTrue = kids(1).asInstanceOf[Fn[T]],
      ifFalse = kids(2).asInstanceOf[Fn[T]]
    )
}


/** AND */
case class AndFn(recv: Fn[Any], args: List[Fn[Any]], posStr: String)
  extends BinaryFn[Boolean]
    with BooleanFn:

  override val methodName: String = "&&"

  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    kids match
      // recv + args list of 1 element
      case r :: a :: Nil =>
        copy(recv = r.asInstanceOf[Fn[Any]], args = List(a.asInstanceOf[Fn[Any]]))
      case _ =>
        this

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (lv, lLens) <- recv.resolve(ctx)
      lvBool <- lv match
        case b: Boolean => ZIO.succeed(b)
        case other => ZIO.fail(DynaLensError(posStr, s"Left side not Boolean: ${other.getClass.getSimpleName}"))
      result <-
        if !lvBool then ZIO.succeed((false, lLens)) // short-circuit
        else
          for
            (rv, _) <- args.head.resolve(ctx)
            rvBool <- rv match
              case b: Boolean => ZIO.succeed(b)
              case other => ZIO.fail(DynaLensError(posStr, s"Right side not Boolean: ${other.getClass.getSimpleName}"))
          yield (lvBool && rvBool, lLens)
    yield result


/** OR */
case class OrFn(recv: Fn[Any], args: List[Fn[Any]], posStr: String)
  extends BinaryFn[Boolean]
    with BooleanFn:

  override val methodName: String = "||"

  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    kids match
      // recv + args list of 1 element
      case r :: a :: Nil =>
        copy(recv = r.asInstanceOf[Fn[Any]], args = List(a.asInstanceOf[Fn[Any]]))
      case _ =>
        this

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (lv, lLens) <- recv.resolve(ctx)
      lvBool <- lv match
        case b: Boolean => ZIO.succeed(b)
        case other => ZIO.fail(DynaLensError(posStr, s"Left side not Boolean: ${other.getClass.getSimpleName}"))
      result <-
        if lvBool then ZIO.succeed((true, lLens)) // short-circuit
        else
          for
            (rv, _) <- args.head.resolve(ctx)
            rvBool <- rv match
              case b: Boolean => ZIO.succeed(b)
              case other => ZIO.fail(DynaLensError(posStr, s"Right side not Boolean: ${other.getClass.getSimpleName}"))
          yield (lvBool || rvBool, lLens)
    yield result


/** NOT */
case class NotFn(recv: Fn[Any], posStr: String)
  extends UnaryFn[Boolean]
    with BooleanFn {

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (v, lens) <- recv.resolve(ctx)
      result <- v match {
        case b: Boolean     => ZIO.succeed(!b)
        case Some(b: Boolean) => ZIO.succeed(!b)
        case None           => ZIO.succeed(true)
        case other          => ZIO.fail(DynaLensError(posStr, s"! expects Boolean, got ${other.getClass.getSimpleName}"))
      }
    yield (result, lens)

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])
}


/** ISDEFINED */
case class IsDefinedFn(recv: Fn[Any], posStr: String)
  extends UnaryFn[Boolean]
    with BooleanFn {

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (v, lens) <- recv.resolve(ctx)
      result = v match {
        case null       => false
        case None       => false
        case Some(_)    => true
        case m: Map[?, ?]   => m.nonEmpty
        case i: Iterable[?] => i.nonEmpty
        case _          => true
      }
    yield (result, lens)

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])
}


/** STARTSWITH */
case class StartsWithFn(recv: Fn[Any], other: Fn[Any], posStr: String)
  extends BinaryFn[Boolean]
    with BooleanFn {

  override val args: List[Fn[Any]] = List(other)
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (sVal, lLens) <- recv.resolve(ctx)
      (oVal, _) <- other.resolve(ctx)
      result <- ZIO.attempt {
        (Option(sVal).map(_.toString).getOrElse(""))
          .startsWith(Option(oVal).map(_.toString).getOrElse(""))
      }.catchAll(e => ZIO.fail(DynaLensError(posStr, s"startsWith() failed: ${e.getMessage}")))
    yield (result, lLens)

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], other = kids(1).asInstanceOf[Fn[Any]])
}


/** ENDSWITH */
case class EndsWithFn(recv: Fn[Any], other: Fn[Any], posStr: String)
  extends BinaryFn[Boolean]
    with BooleanFn {

  override val args: List[Fn[Any]] = List(other)
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (sVal, lLens) <- recv.resolve(ctx)
      (oVal, _) <- other.resolve(ctx)
      result <- ZIO.attempt {
        (Option(sVal).map(_.toString).getOrElse(""))
          .endsWith(Option(oVal).map(_.toString).getOrElse(""))
      }.catchAll(e => ZIO.fail(DynaLensError(posStr, s"endsWith() failed: ${e.getMessage}")))
    yield (result, lLens)

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], other = kids(1).asInstanceOf[Fn[Any]])
}

/** CONTAINS */
case class ContainsFn(recv: Fn[Any], other: Fn[Any], posStr: String)
  extends Fn[Boolean]
    with BinaryFn[Boolean]
    with BooleanFn {

  import ContainsFn._

  override val args: List[Fn[Any]] = List(other)
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (hay, lens) <- recv.resolve(ctx)
      result <- containsDynamic(hay, other, ctx, posStr, lens)
    yield result

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], other = kids(1).asInstanceOf[Fn[Any]])
}

object ContainsFn {

  @tailrec
  private def containsDynamic(
                               hay: Any,
                               needle: Fn[Any],
                               ctx: DynaContext,
                               posStr: String,
                               lens: Lens
                             ): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    hay match {

      // ---- Option unwraps ----
      case null        => ZIO.succeed((false, lens))
      case None        => ZIO.succeed((false, lens))
      case Some(inner) => containsDynamic(inner, needle, ctx, posStr, lens)

      // ---- String: substring ----
      case cs: CharSequence =>
        for {
          (ndlAny, ndlLens) <- needle.resolve(ctx)
        } yield (cs.toString.contains(Option(ndlAny).fold("null")(_.toString)), lens)

      // ---- Map: key presence (needle evaluated once) ----
      case m: Map[?, ?] =>
        for {
          (ndlVal, ndlLens) <- needle.resolve(ctx)
        } yield (m.asInstanceOf[Map[Any, Any]].contains(ndlVal), lens)

      // ---- Iterable: supports predicate OR value check ----
      case it: Iterable[?] =>
        needle match {
          case pred: BooleanFn =>
            def loop(
              elems: List[Any],
              firstError: Option[DynaLensError],
              foundFalseSuccess: Boolean
            ): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] = elems match {
              case Nil =>
                firstError match {
                  case None => ZIO.succeed((false, lens)) // no errors, all false
                  case Some(err) =>
                    if (foundFalseSuccess) ZIO.succeed((false, lens))
                    else ZIO.fail(err)
                }
              case head :: tail =>
                val elemLens: Lens = lens match
                  case ll: ListLens => ll.elementLens
                  case _            => lens
                ctx.withThisScoped(head, elemLens) { scopedCtx =>
                  pred.resolve(scopedCtx).either.flatMap {
                    case Right((true, _)) =>
                      ZIO.succeed((true, lens)) // short-circuit on true
                    case Right((false, _)) =>
                      loop(tail, firstError, true)
                    case Left(err) =>
                      loop(tail, firstError.orElse(Some(err)), foundFalseSuccess)
                  }
                }
            }
            loop(it.toList, None, false)
          case _ =>
            for {
              (ndlVal, _) <- needle.resolve(ctx)
            } yield (it.exists(_ == ndlVal), lens)
        }

      // ---- Unsupported receiver types ----
      case other =>
        ZIO.fail(
          DynaLensError(
            posStr,
            s"contains() not supported for ${other.getClass.getSimpleName}; expected String, Iterable, Map, or Option thereof"
          )
        )
    }
}


/** EQUALSIGNORECASE */
case class EqualsIgnoreCaseFn(recv: Fn[Any], other: Fn[Any], posStr: String)
  extends BinaryFn[Boolean]
    with BooleanFn {

  override val args: List[Fn[Any]] = List(other)
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (rVal, lLens) <- recv.resolve(ctx)
      (oVal, _) <- other.resolve(ctx)
      result <- ZIO.attempt {
        (Option(rVal).map(_.toString).getOrElse(""))
          .equalsIgnoreCase(Option(oVal).map(_.toString).getOrElse(""))
      }.catchAll(e => ZIO.fail(DynaLensError(posStr, s"equalsIgnoreCase() failed: ${e.getMessage}")))
    yield (result, lLens)

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], other = kids(1).asInstanceOf[Fn[Any]])
}


/** MATCHESREGEX */
case class MatchesRegexFn(recv: Fn[Any], other: Fn[Any], posStr: String)
  extends BinaryFn[Boolean]
    with BooleanFn {

  override val args: List[Fn[Any]] = List(other)
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (sVal, lLens) <- recv.resolve(ctx)
      (rVal, _) <- other.resolve(ctx)
      result <- ZIO.attempt {
        val s = Option(sVal).map(_.toString).getOrElse("")
        val r = Option(rVal).map(_.toString).getOrElse("")
        s.matches(r)
      }.catchAll(e => ZIO.fail(DynaLensError(posStr, s"matchesRegex() failed: ${e.getMessage}")))
    yield (result, lLens)

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], other = kids(1).asInstanceOf[Fn[Any]])
}