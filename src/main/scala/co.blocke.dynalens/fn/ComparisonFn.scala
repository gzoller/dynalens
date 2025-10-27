package co.blocke.dynalens
package fn

import zio.*

object ComparisonSupport:

  private def asBigDecimal(n: Any): Option[BigDecimal] = n match
    case _: Byte | _: Short | _: Int | _: Long => Some(BigDecimal(n.toString))
    case _: Float | _: Double => Some(BigDecimal(n.toString))
    case bi: BigInt => Some(BigDecimal(bi))
    case bd: BigDecimal => Some(bd)
    case _ => None

  def compareNumbers(a: Any, b: Any): Either[String, Int] =
    if (a == null || b == null) Left(s"Cannot compare values: $a, $b")
    else if (a.isInstanceOf[Option[?]] || b.isInstanceOf[Option[?]])
      Left(s"Cannot compare values: $a, $b")
    else
      (asBigDecimal(a), asBigDecimal(b)) match
        case (Some(ba), Some(bb)) => Right(ba.compare(bb)) // -1,0,1
        case _ => Left(s"Cannot compare values: $a, $b")



private def comparableCompare(l: Any, r: Any)(cmp: Int => Boolean): Option[Boolean] =
  (l, r) match
    case (a: Number, b: Number) =>
      Some(cmp(java.lang.Double.compare(a.doubleValue(), b.doubleValue())))
    case (a: String, b: String) =>
      Some(cmp(a.compareTo(b)))
    case (a: Comparable[?], b: Comparable[?]) if a.getClass == b.getClass =>
      Some(cmp(a.asInstanceOf[Comparable[Any]].compareTo(b)))
    case _ => None

private def compareOptionals(lAny: Any, rAny: Any)(cmp: (Any, Any) => Option[Boolean]): ZIO[RuntimeEnv, DynaLensError, Boolean] =
  (lAny, rAny) match
    case (None, None) => ZIO.succeed(true)
    case (None, Some(_)) =>
      ZIO.fail(DynaLensError("", "Cannot compare None with Some value"))
    case (Some(_), None) =>
      ZIO.fail(DynaLensError("", "Cannot compare Some value with None"))
    case (Some(lVal), Some(rVal)) =>
      cmp(lVal, rVal) match
        case Some(b) => ZIO.succeed(b)
        case None => ZIO.fail(DynaLensError("", s"Cannot compare types: ${lVal.getClass}, ${rVal.getClass}"))
    case _ =>
      ZIO.fail(DynaLensError("", s"Cannot compare values: $lAny, $rAny"))

case class LessThanFn(recv: Fn[Any], arg: Fn[Any], posStr: String) extends BinaryFn[Boolean] with BooleanFn:

  override val methodName = "<"
  override val args: List[Fn[Any]] = List(arg)
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (lv, lLens) <- recv.resolve(ctx)
      (rv, _)     <- arg.resolve(ctx)
      cmp <- ComparisonSupport.compareNumbers(lv, rv) match
        case Right(c) => ZIO.succeed(c)
        case Left(msg) => ZIO.fail(DynaLensError(posStr, msg))
    yield (cmp < 0, lLens)


case class GreaterThanFn(recv: Fn[Any], arg: Fn[Any], posStr: String) extends BinaryFn[Boolean] with BooleanFn:

  override val methodName = ">"
  override val args: List[Fn[Any]] = List(arg)
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (lv, lLens) <- recv.resolve(ctx)
      (rv, _)     <- args.head.resolve(ctx)
      cmp <- ComparisonSupport.compareNumbers(lv, rv) match
        case Right(c) => ZIO.succeed(c)
        case Left(msg) => ZIO.fail(DynaLensError(posStr, msg))
    yield (cmp > 0, lLens)


case class LessThanOrEqualFn(recv: Fn[Any], arg: Fn[Any], posStr: String) extends BinaryFn[Boolean] with BooleanFn:

  override val methodName = "<="
  override val args: List[Fn[Any]] = List(arg)
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (lv, lLens) <- recv.resolve(ctx)
      (rv, _)     <- arg.resolve(ctx)
      cmp <- ComparisonSupport.compareNumbers(lv, rv) match
        case Right(c) => ZIO.succeed(c)
        case Left(msg) => ZIO.fail(DynaLensError(posStr, msg))
    yield (cmp <= 0, lLens)


case class GreaterThanOrEqualFn(recv: Fn[Any], arg: Fn[Any], posStr: String) extends BinaryFn[Boolean] with BooleanFn:

  override val methodName = ">="
  override val args: List[Fn[Any]] = List(arg)
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (lv, lLens) <- recv.resolve(ctx)
      (rv, _)     <- arg.resolve(ctx)
      cmp <- ComparisonSupport.compareNumbers(lv, rv) match
        case Right(c) => ZIO.succeed(c)
        case Left(msg) => ZIO.fail(DynaLensError(posStr, msg))
    yield (cmp >= 0, lLens)


case class EqualFn(recv: Fn[Any], arg: Fn[Any], posStr: String) extends BinaryFn[Boolean] with BooleanFn:

  override val methodName = "=="
  override val args: List[Fn[Any]] = List(arg)
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (lv, lLens) <- recv.resolve(ctx)
      (rv, _)     <- arg.resolve(ctx)
      cmp <- ComparisonSupport.compareNumbers(lv, rv) match
        case Right(c) => ZIO.succeed(c)
        case Left(msg) => ZIO.fail(DynaLensError(posStr, msg))
    yield (cmp == 0, lLens)


case class NotEqualFn(recv: Fn[Any], arg: Fn[Any], posStr: String) extends BinaryFn[Boolean] with BooleanFn:

  override val methodName = "!="
  override val args: List[Fn[Any]] = List(arg)
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (lv, lLens) <- recv.resolve(ctx)
      (rv, _)     <- arg.resolve(ctx)
      cmp <- ComparisonSupport.compareNumbers(lv, rv) match
        case Right(c) => ZIO.succeed(c)
        case Left(msg) => ZIO.fail(DynaLensError(posStr, msg))
    yield (cmp != 0, lLens)