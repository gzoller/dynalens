package co.blocke.dynalens
package fn

import zio.*


case class TrimFn(recv: Fn[Any], posStr: String)
  extends Fn[String] with UnaryFn[String]:
  override def rebuild(kids: List[Fn[?]]): Fn[String] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext) =
    for raw <- recv.resolve(ctx)
      yield raw match
        case null => throw DynaLensError(posStr, "trim() found null")
        case s: String => s.trim
        case other => throw DynaLensError(posStr, s"trim() expects String, got ${other.getClass.getSimpleName}")


case class ToLowerFn(recv: Fn[Any], posStr: String)
  extends Fn[String] with UnaryFn[String]:
  override def rebuild(kids: List[Fn[?]]): Fn[String] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext) =
    for raw <- recv.resolve(ctx)
      yield raw match
        case null => throw DynaLensError(posStr, "toLower() found null")
        case s: String => s.toLowerCase
        case other => throw DynaLensError(posStr, s"toLower() expects String, got ${other.getClass.getSimpleName}")


case class ToUpperFn(recv: Fn[Any], posStr: String)
  extends Fn[String] with UnaryFn[String]:
  override def rebuild(kids: List[Fn[?]]): Fn[String] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext) =
    for raw <- recv.resolve(ctx)
      yield raw match
        case null => throw DynaLensError(posStr, "toUpper() found null")
        case s: String => s.toUpperCase
        case other => throw DynaLensError(posStr, s"toUpper() expects String, got ${other.getClass.getSimpleName}")


case class InterpolateFn(
                          recv: Fn[Any],
                          varMap: Map[String, Fn[Any]],
                          posStr: String
                        ) extends UnaryFn[String]:
  override def rebuild(kids: List[Fn[?]]): Fn[String] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext) =
    for
      raw <- recv.resolve(ctx)
      pat <- raw match
        case null => ZIO.fail(DynaLensError(posStr, "template() pattern was null"))
        case s: String => ZIO.succeed(s)
        case other => ZIO.fail(DynaLensError(posStr, s"template() expects String, got ${other.getClass.getSimpleName}"))
      filled <- ZIO.foreach(TemplateUtils.extractVariables(pat).toList) { v =>
        varMap.get(v) match
          case Some(fn) => fn.resolve(ctx).map(v -> _)
          case None => ZIO.succeed(v -> null.asInstanceOf[Any])
      }.map(_.toMap)
    yield TemplateUtils.fill(pat, filled)


case class SubstringFn(
                        recv: Fn[Any],
                        start: Fn[Any],
                        end: Option[Fn[Any]],
                        posStr: String
                      ) extends MethodFn[String]:
  override def args: List[Fn[Any]] = end.map(e => List(start, e)).getOrElse(List(start))

  override def rebuild(kids: List[Fn[?]]): Fn[String] =
    kids match
      case r :: s :: e :: Nil => copy(recv = r.asInstanceOf[Fn[Any]], start = s.asInstanceOf[Fn[Any]], end = Some(e.asInstanceOf[Fn[Any]]))
      case r :: s :: Nil => copy(recv = r.asInstanceOf[Fn[Any]], start = s.asInstanceOf[Fn[Any]], end = None)
      case _ => this

  private def toIndex(n: Any): Option[Int] = n match
    case i: java.lang.Integer => Some(i.intValue)
    case l: java.lang.Long => Some(l.intValue)
    case f: java.lang.Float => Some(f.toInt)
    case d: java.lang.Double => Some(d.toInt)
    case s: java.lang.Short => Some(s.toInt)
    case b: java.lang.Byte => Some(b.toInt)
    case _ => None

  def resolve(ctx: DynaContext) =
    for
      rawS <- recv.resolve(ctx)
      s <- rawS match
        case null => ZIO.fail(DynaLensError(posStr, "substring() receiver was null"))
        case str: String => ZIO.succeed(str)
        case other => ZIO.fail(DynaLensError(posStr, s"substring() expects String, got ${other.getClass.getSimpleName}"))
      fromV <- start.resolve(ctx)
      from <- ZIO.fromOption(toIndex(fromV)).mapError(_ => DynaLensError(posStr, "substring() start must be numeric"))
      toOpt <- end match
        case None => ZIO.succeed(None)
        case Some(e) => e.resolve(ctx).flatMap(v => ZIO.fromOption(toIndex(v)).map(Some(_)).mapError(_ => DynaLensError(posStr, "substring() end must be numeric")))
      (lo, hi0) = (Math.max(0, from), toOpt.getOrElse(s.length))
      hi = Math.max(lo, Math.min(hi0, s.length))
    yield s.substring(lo, hi)


case class ReplaceFn(
                      recv: Fn[Any],
                      target: Fn[Any],
                      replacement: Fn[Any],
                      posStr: String
                    ) extends MethodFn[String]:
  override def args: List[Fn[Any]] = List(target, replacement)

  override def rebuild(kids: List[Fn[?]]): Fn[String] =
    kids match
      case r :: t :: rp :: Nil =>
        copy(recv = r.asInstanceOf[Fn[Any]], target = t.asInstanceOf[Fn[Any]], replacement = rp.asInstanceOf[Fn[Any]])
      case _ => this

  def resolve(ctx: DynaContext) =
    for
      s <- recv.resolve(ctx).flatMap {
        case null => ZIO.fail(DynaLensError(posStr, "replace() receiver was null"))
        case str: String => ZIO.succeed(str)
        case other => ZIO.fail(DynaLensError(posStr, s"replace() expects String, got ${other.getClass.getSimpleName}"))
      }
      t <- target.resolve(ctx).map {
        case null | None => ""
        case Some(x) => x.toString
        case x => x.toString
      }
      rep <- replacement.resolve(ctx).map {
        case null | None => ""
        case Some(x) => x.toString
        case x => x.toString
      }
    yield s.replace(t, rep)


/** ---------------------- cocatenate ---------------------- */
case class ConcatFn(recv: Fn[Any], args: List[Fn[Any]], posStr: String)
  extends Fn[String]
    with MethodFn[String]:

  override val methodName: String = "+"
  override def rebuild(kids: List[Fn[?]]): Fn[String] =
    kids match
      case head :: tail =>
        copy(
          recv = head.asInstanceOf[Fn[Any]],
          args = tail.asInstanceOf[List[Fn[Any]]]
        )
      case _ =>
        copy(args = Nil)

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    for
      recvVal  <- recv.resolve(ctx)
      argVals  <- ZIO.foreach(args)(_.resolve(ctx))
      result   <- ZIO.attempt {
        val sb = new StringBuilder
        def appendAny(value: Any): Unit =
          value match
            case null | None => ()
            case Some(xs: Seq[?])      => xs.foreach(x => if x != null then sb.append(x.toString))
            case Some(xs: Iterable[?]) => xs.foreach(x => if x != null then sb.append(x.toString))
            case xs: Seq[?]            => xs.foreach(x => if x != null then sb.append(x.toString))
            case xs: Iterable[?]       => xs.foreach(x => if x != null then sb.append(x.toString))
            case Some(v)               => sb.append(v.toString)
            case v                     => sb.append(v.toString)

        appendAny(recvVal)
        argVals.foreach(appendAny)
        sb.toString
      }.catchAll(e => ZIO.fail(DynaLensError(posStr, s"concat (+) failed at $posStr: ${e.getMessage}")))
    yield result