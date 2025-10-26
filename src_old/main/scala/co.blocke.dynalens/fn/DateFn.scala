package co.blocke.dynalens
package fn

import zio.*


case class FormatDateFn(recv: Fn[Any], pattern: Fn[Any], posStr: String) extends MethodFn[Any]:
  override val methodName = "formatDate"
  def args: List[Fn[Any]] = List(pattern)
  def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(recv = kids(0).asInstanceOf[Fn[Any]], pattern = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext) =
    for
      dateVal <- recv.resolve(ctx)
      patVal  <- pattern.resolve(ctx)
      out <- (dateVal, patVal) match
        case (null, _) =>
          ZIO.fail(DynaLensError(posStr, s"formatDate() receiver is null"))
        case (d: java.time.temporal.TemporalAccessor, p: String) =>
          val fmt = java.time.format.DateTimeFormatter.ofPattern(p)
          ZIO.succeed(fmt.format(d))
        case (_, p: String) =>
          ZIO.fail(DynaLensError(posStr, s"formatDate() requires date/time receiver, got ${dateVal.getClass.getSimpleName}"))
        case _ =>
          ZIO.fail(DynaLensError(posStr, s"formatDate() pattern must be String"))
    yield out


case class ParseDateFn(recv: Fn[Any], pattern: Fn[Any], posStr: String) extends MethodFn[Any]:
  override val methodName = "parseDate"
  def args: List[Fn[Any]] = List(pattern)
  def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(recv = kids(0).asInstanceOf[Fn[Any]], pattern = kids(1).asInstanceOf[Fn[Any]])
  def resolve(ctx: DynaContext) =
    for
      srcVal <- recv.resolve(ctx)
      patVal <- pattern.resolve(ctx)
      out <- (srcVal, patVal) match
        case (null, _) => ZIO.fail(DynaLensError(posStr, s"parseDate() source is null"))
        case (s: String, p: String) =>
          val fmt = java.time.format.DateTimeFormatter.ofPattern(p)
          ZIO.succeed(java.time.LocalDateTime.parse(s, fmt))
        case _ =>
          ZIO.fail(DynaLensError(posStr, s"parseDate() requires (String, String) but got (${srcVal.getClass.getSimpleName}, ${patVal.getClass.getSimpleName})"))
    yield out


case class NowFn(posStr: String) extends Fn[Any]:
  override val methodName = "now"
  override val recv: Fn[Any] = RootFn
  def args: List[Fn[Any]] = Nil
  def rebuild(kids: List[Fn[?]]): Fn[Any] = this
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ZIO.succeed(java.time.LocalDateTime.now())