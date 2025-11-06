package co.blocke.dynalens
package fn

import zio.*


case class FormatDateFn(recv: Fn[Any], pattern: Fn[Any], posStr: String) extends MethodFn[Any]:
  override val methodName = "formatDate"
  def args: List[Fn[Any]] = List(pattern)
  val resultType: FieldType = ScalarType("", "java.lang.String", false)
  def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(recv = kids(0).asInstanceOf[Fn[Any]], pattern = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    for
      (dateAny, dateLens) <- recv.resolve(ctx)
      (patAny, _)  <- pattern.resolve(ctx)
      out <- (dateAny, patAny) match
        case (null, _) =>
          ZIO.fail(DynaLensError(posStr, s"formatDate() receiver is null"))
        case (d: java.time.temporal.TemporalAccessor, p: String) =>
          val fmt = java.time.format.DateTimeFormatter.ofPattern(p)
          ZIO.succeed((fmt.format(d), dateLens))
        case (_, p: String) =>
          ZIO.fail(DynaLensError(posStr, s"formatDate() requires date/time receiver, got ${dateAny.getClass.getSimpleName}"))
        case _ =>
          ZIO.fail(DynaLensError(posStr, s"formatDate() pattern must be String"))
    yield out


case class ParseDateFn(recv: Fn[Any], pattern: Fn[Any], posStr: String) extends MethodFn[Any]:
  override val methodName = "parseDate"
  def args: List[Fn[Any]] = List(pattern)
  val resultType: FieldType = ScalarType("", "java.time.LocalDateTime", false)
  def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(recv = kids(0).asInstanceOf[Fn[Any]], pattern = kids(1).asInstanceOf[Fn[Any]])
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    for
      (srcAny, srcLens) <- recv.resolve(ctx)
      (patAny, _) <- pattern.resolve(ctx)
      out <- (srcAny, patAny) match
        case (null, _) => ZIO.fail(DynaLensError(posStr, s"parseDate() source is null"))
        case (s: String, p: String) =>
          val fmt = java.time.format.DateTimeFormatter.ofPattern(p)
          ZIO.succeed((java.time.LocalDateTime.parse(s, fmt), srcLens))
        case _ =>
          ZIO.fail(DynaLensError(posStr, s"parseDate() requires (String, String) but got (${srcAny.getClass.getSimpleName}, ${patAny.getClass.getSimpleName})"))
    yield out


case class NowFn(posStr: String) extends Fn[Any]:
  override val methodName = "now"
  override val recv: Fn[Any] = RootFn
  val resultType: FieldType = ScalarType("", "java.time.LocalDateTime", false)
  def args: List[Fn[Any]] = Nil
  def rebuild(kids: List[Fn[?]]): Fn[Any] = this
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    ZIO.succeed((java.time.LocalDateTime.now(), ScalarLens("now", false, None)))