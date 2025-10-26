package co.blocke.dynalens
package fn

import zio.*


private def comparableCompare(l: Any, r: Any)(cmp: Int => Boolean): Option[Boolean] =
  (l, r) match
    case (a: Number, b: Number) =>
      Some(cmp(java.lang.Double.compare(a.doubleValue(), b.doubleValue())))
    case (a: String, b: String) =>
      Some(cmp(a.compareTo(b)))
    case (a: Comparable[?], b: Comparable[?]) if a.getClass == b.getClass =>
      Some(cmp(a.asInstanceOf[Comparable[Any]].compareTo(b)))
    case _ => None


case class LessThanFn(recv: Fn[Any], arg: Fn[Any], posStr: String) extends BinaryFn[Boolean] with BooleanFn:

  override val methodName = "<"
  override val args: List[Fn[Any]] = List(arg)
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for
      l <- recv.resolve(ctx)
      r <- arg.resolve(ctx)
      res <- comparableCompare(l, r)(_ < 0) match
        case Some(b) => ZIO.succeed(b)
        case None => ZIO.fail(DynaLensError(posStr, s"Cannot compare types: ${l.getClass}, ${r.getClass}"))
    yield res


case class GreaterThanFn(recv: Fn[Any], arg: Fn[Any], posStr: String) extends BinaryFn[Boolean] with BooleanFn:

  override val methodName = ">"
  override val args: List[Fn[Any]] = List(arg)
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for
      l <- recv.resolve(ctx)
      r <- arg.resolve(ctx)
      res <- comparableCompare(l, r)(_ > 0) match
        case Some(b) => ZIO.succeed(b)
        case None => ZIO.fail(DynaLensError(posStr, s"Cannot compare types: ${l.getClass}, ${r.getClass}"))
    yield res


case class LessThanOrEqualFn(recv: Fn[Any], arg: Fn[Any], posStr: String) extends BinaryFn[Boolean] with BooleanFn:

  override val methodName = "<="
  override val args: List[Fn[Any]] = List(arg)
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for
      l <- recv.resolve(ctx)
      r <- arg.resolve(ctx)
      res <- comparableCompare(l, r)(_ <= 0) match
        case Some(b) => ZIO.succeed(b)
        case None => ZIO.fail(DynaLensError(posStr, s"Cannot compare types: ${l.getClass}, ${r.getClass}"))
    yield res


case class GreaterThanOrEqualFn(recv: Fn[Any], arg: Fn[Any], posStr: String) extends BinaryFn[Boolean] with BooleanFn:

  override val methodName = ">="
  override val args: List[Fn[Any]] = List(arg)
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for
      l <- recv.resolve(ctx)
      r <- arg.resolve(ctx)
      res <- comparableCompare(l, r)(_ >= 0) match
        case Some(b) => ZIO.succeed(b)
        case None => ZIO.fail(DynaLensError(posStr, s"Cannot compare types: ${l.getClass}, ${r.getClass}"))
    yield res


case class EqualFn(recv: Fn[Any], arg: Fn[Any], posStr: String) extends BinaryFn[Boolean] with BooleanFn:

  override val methodName = "=="
  override val args: List[Fn[Any]] = List(arg)
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for
      l <- recv.resolve(ctx)
      r <- arg.resolve(ctx)
      res <- comparableCompare(l, r)(_ == 0) match
        case Some(b) => ZIO.succeed(b)
        case None => ZIO.fail(DynaLensError(posStr, s"Cannot compare types: ${l.getClass}, ${r.getClass}"))
    yield res


case class NotEqualFn(recv: Fn[Any], arg: Fn[Any], posStr: String) extends BinaryFn[Boolean] with BooleanFn:

  override val methodName = "!="
  override val args: List[Fn[Any]] = List(arg)
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for
      l <- recv.resolve(ctx)
      r <- arg.resolve(ctx)
      res <- comparableCompare(l, r)(_ != 0) match
        case Some(b) => ZIO.succeed(b)
        case None => ZIO.fail(DynaLensError(posStr, s"Cannot compare types: ${l.getClass}, ${r.getClass}"))
    yield res