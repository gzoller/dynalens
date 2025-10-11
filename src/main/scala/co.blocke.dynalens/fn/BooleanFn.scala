package co.blocke.dynalens
package fn

import zio.*
import scala.annotation.tailrec


/** IF */
case class IfFn[T](condition: BooleanFn, ifTrue: Fn[T], ifFalse: Fn[T], posStr: String)
  extends Fn[T] {

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, T] =
    for
      condVal <- condition.resolve(ctx)
      result  <- if condVal then ifTrue.resolve(ctx) else ifFalse.resolve(ctx)
    yield result

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

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for
      l <- recv.resolve(ctx)
      result <- l match
        case b: Boolean =>
          if !b then ZIO.succeed(false) // short-circuit
          else
            args.head.resolve(ctx).map {
              case r: Boolean => b && r
              case other => throw new RuntimeException(s"Right side not Boolean: ${other.getClass.getSimpleName}")
            }.catchAll(e => ZIO.fail(DynaLensError(posStr, s"&& failed: ${e.getMessage}")))
        case other =>
          ZIO.fail(DynaLensError(posStr, s"Left side not Boolean: ${other.getClass.getSimpleName}"))
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

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for
      l <- recv.resolve(ctx)
      result <- l match
        case b: Boolean =>
          if b then ZIO.succeed(true) // short-circuit
          else
            args.head.resolve(ctx).map {
              case r: Boolean => b || r
              case other => throw new RuntimeException(s"Right side not Boolean: ${other.getClass.getSimpleName}")
            }.catchAll(e => ZIO.fail(DynaLensError(posStr, s"|| failed: ${e.getMessage}")))
        case other =>
          ZIO.fail(DynaLensError(posStr, s"Left side not Boolean: ${other.getClass.getSimpleName}"))
    yield result
    

/** NOT */
case class NotFn(recv: Fn[Any], posStr: String)
  extends UnaryFn[Boolean]
    with BooleanFn {

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    recv.resolve(ctx).flatMap {
      case b: Boolean     => ZIO.succeed(!b)
      case Some(b: Boolean) => ZIO.succeed(!b)
      case None           => ZIO.succeed(true)
      case other          => ZIO.fail(DynaLensError(posStr, s"! expects Boolean, got ${other.getClass.getSimpleName}"))
    }

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])
}


/** ISDEFINED */
case class IsDefinedFn(recv: Fn[Any], posStr: String)
  extends UnaryFn[Boolean]
    with BooleanFn {

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    recv.resolve(ctx).map {
      case null       => false
      case None       => false
      case Some(v)    => true
      case i: Iterable[?] => i.nonEmpty
      case m: Map[?, ?]   => m.nonEmpty
      case _          => true
    }

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])
}


/** STARTSWITH */
case class StartsWithFn(recv: Fn[Any], other: Fn[Any], posStr: String)
  extends BinaryFn[Boolean]
    with BooleanFn {

  override val args: List[Fn[Any]] = List(other)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for
      sVal <- recv.resolve(ctx)
      oVal <- other.resolve(ctx)
      result <- ZIO.attempt {
        (Option(sVal).map(_.toString).getOrElse(""))
          .startsWith(Option(oVal).map(_.toString).getOrElse(""))
      }.catchAll(e => ZIO.fail(DynaLensError(posStr, s"startsWith() failed: ${e.getMessage}")))
    yield result

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], other = kids(1).asInstanceOf[Fn[Any]])
}


/** ENDSWITH */
case class EndsWithFn(recv: Fn[Any], other: Fn[Any], posStr: String)
  extends BinaryFn[Boolean]
    with BooleanFn {

  override val args: List[Fn[Any]] = List(other)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for
      sVal <- recv.resolve(ctx)
      oVal <- other.resolve(ctx)
      result <- ZIO.attempt {
        (Option(sVal).map(_.toString).getOrElse(""))
          .endsWith(Option(oVal).map(_.toString).getOrElse(""))
      }.catchAll(e => ZIO.fail(DynaLensError(posStr, s"endsWith() failed: ${e.getMessage}")))
    yield result

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
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    recv.resolve(ctx).flatMap(hay => containsDynamic(hay, other, ctx, posStr))

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], other = kids(1).asInstanceOf[Fn[Any]])
}

object ContainsFn {

  @tailrec
  private def containsDynamic(
                               hay: Any,
                               needle: Fn[Any],
                               ctx: DynaContext,
                               posStr: String
                             ): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    hay match {

      // ---- Option unwraps ----
      case null        => ZIO.succeed(false)
      case None        => ZIO.succeed(false)
      case Some(inner) => containsDynamic(inner, needle, ctx, posStr)

      // ---- String: substring ----
      case cs: CharSequence =>
        for {
          ndlAny <- needle.resolve(ctx)
        } yield cs.toString.contains(Option(ndlAny).fold("null")(_.toString))

      // ---- Map: key presence (needle evaluated once) ----
      case m: Map[?, ?] =>
        for {
          ndlVal <- needle.resolve(ctx)
        } yield m.asInstanceOf[Map[Any, Any]].contains(ndlVal)

      // ---- Iterable: supports predicate OR value check ----
      case it: Iterable[?] =>
        needle match {

          // Predicate case: evaluate per element with `this` bound
          case pred: BooleanFn =>
            ZIO
              .foreach(it.asInstanceOf[Iterable[Any]]) { elem =>
                withThisScoped(ctx, elem) {
                  pred.resolve(ctx)
                }.either
              }
              .map(_.exists {
                case Right(true) => true
                case _           => false
              })

          // Value case: compute target once, then == compare
          case _ =>
            for {
              ndlVal <- needle.resolve(ctx)
            } yield it.asInstanceOf[Iterable[Any]].exists(_ == ndlVal)
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
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for
      rVal <- recv.resolve(ctx)
      oVal <- other.resolve(ctx)
      result <- ZIO.attempt {
        (Option(rVal).map(_.toString).getOrElse(""))
          .equalsIgnoreCase(Option(oVal).map(_.toString).getOrElse(""))
      }.catchAll(e => ZIO.fail(DynaLensError(posStr, s"equalsIgnoreCase() failed: ${e.getMessage}")))
    yield result

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], other = kids(1).asInstanceOf[Fn[Any]])
}


/** MATCHESREGEX */
case class MatchesRegexFn(recv: Fn[Any], other: Fn[Any], posStr: String)
  extends BinaryFn[Boolean]
    with BooleanFn {

  override val args: List[Fn[Any]] = List(other)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for
      sVal <- recv.resolve(ctx)
      rVal <- other.resolve(ctx)
      result <- ZIO.attempt {
        val s = Option(sVal).map(_.toString).getOrElse("")
        val r = Option(rVal).map(_.toString).getOrElse("")
        s.matches(r)
      }.catchAll(e => ZIO.fail(DynaLensError(posStr, s"matchesRegex() failed: ${e.getMessage}")))
    yield result

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], other = kids(1).asInstanceOf[Fn[Any]])
}