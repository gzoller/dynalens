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

  def compareValues(a: Any, b: Any): Either[String, Int] =
    (a, b) match
      case (_: Option[?], _) | (_, _: Option[?]) =>
        Left(s"Cannot compare values: $a, $b")
      case (null, _) | (_, null) =>
        Left(s"Cannot compare values: $a, $b")
      case (as: String, bs: String) =>
        Right(as.compareTo(bs))
      case _ =>
        (asBigDecimal(a), asBigDecimal(b)) match
          case (Some(ba), Some(bb)) => Right(ba.compare(bb))
          case _ => Left(s"Cannot compare values: $a, $b")


case class LessThanFn(recv: Fn[Any], arg: Fn[Any], posStr: String) extends BinaryFn[Boolean] with BooleanFn:

  override val methodName = "<"
  override val args: List[Fn[Any]] = List(arg)
  val resultType: FieldType = ScalarType("", "scala.Boolean", false)
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (lv, lLens) <- recv.resolve(ctx)
      (rv, _)     <- arg.resolve(ctx)
      cmp <- ComparisonSupport.compareValues(lv, rv) match
        case Right(c) => ZIO.succeed(c)
        case Left(msg) => ZIO.fail(DynaLensError(posStr, msg))
    yield (cmp < 0, lLens)


case class GreaterThanFn(recv: Fn[Any], arg: Fn[Any], posStr: String) extends BinaryFn[Boolean] with BooleanFn:

  override val methodName = ">"
  override val args: List[Fn[Any]] = List(arg)
  val resultType: FieldType = ScalarType("", "scala.Boolean", false)
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (lv, lLens) <- recv.resolve(ctx)
      (rv, _)     <- arg.resolve(ctx)
      cmp <- ComparisonSupport.compareValues(lv, rv) match
        case Right(c) => ZIO.succeed(c)
        case Left(msg) => ZIO.fail(DynaLensError(posStr, msg))
    yield (cmp > 0, lLens)


case class LessThanOrEqualFn(recv: Fn[Any], arg: Fn[Any], posStr: String) extends BinaryFn[Boolean] with BooleanFn:

  override val methodName = "<="
  override val args: List[Fn[Any]] = List(arg)
  val resultType: FieldType = ScalarType("", "scala.Boolean", false)
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (lv, lLens) <- recv.resolve(ctx)
      (rv, _)     <- arg.resolve(ctx)
      cmp <- ComparisonSupport.compareValues(lv, rv) match
        case Right(c) => ZIO.succeed(c)
        case Left(msg) => ZIO.fail(DynaLensError(posStr, msg))
    yield (cmp <= 0, lLens)


case class GreaterThanOrEqualFn(recv: Fn[Any], arg: Fn[Any], posStr: String) extends BinaryFn[Boolean] with BooleanFn:

  override val methodName = ">="
  override val args: List[Fn[Any]] = List(arg)
  val resultType: FieldType = ScalarType("", "scala.Boolean", false)
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (lv, lLens) <- recv.resolve(ctx)
      (rv, _)     <- arg.resolve(ctx)
      cmp <- ComparisonSupport.compareValues(lv, rv) match
        case Right(c) => ZIO.succeed(c)
        case Left(msg) => ZIO.fail(DynaLensError(posStr, msg))
    yield (cmp >= 0, lLens)


case class EqualFn(recv: Fn[Any], arg: Fn[Any], posStr: String) extends BinaryFn[Boolean] with BooleanFn:

  override val methodName = "=="
  override val args: List[Fn[Any]] = List(arg)
  val resultType: FieldType = ScalarType("", "scala.Boolean", false)
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (lv, lLens) <- recv.resolve(ctx)
      (rv, _)     <- arg.resolve(ctx)
    yield (lv == rv, lLens)


case class NotEqualFn(recv: Fn[Any], arg: Fn[Any], posStr: String) extends BinaryFn[Boolean] with BooleanFn:

  override val methodName = "!="
  override val args: List[Fn[Any]] = List(arg)
  val resultType: FieldType = ScalarType("", "scala.Boolean", false)
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (lv, lLens) <- recv.resolve(ctx)
      (rv, _)     <- arg.resolve(ctx)
    yield (lv != rv, lLens)