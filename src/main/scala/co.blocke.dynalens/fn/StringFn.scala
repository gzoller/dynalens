package co.blocke.dynalens
package fn

import zio.*
import util.TemplateUtils


case class TrimFn(recv: Fn[Any], posStr: String)
  extends Fn[String] with UnaryFn[String]:
  override def rebuild(kids: List[Fn[?]]): Fn[String] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], posStr = posStr)

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (String, Lens)] =
    for
      (raw, rawLens) <- recv.resolve(ctx)
      res <- raw match
        case null => ZIO.fail(DynaLensError(posStr, "trim() found null"))
        case s: String => ZIO.succeed((s.trim, rawLens))
        case other => ZIO.fail(DynaLensError(posStr, s"trim() expects String, got ${other.getClass.getSimpleName}"))
    yield res


case class ToLowerFn(recv: Fn[Any], posStr: String)
  extends Fn[String] with UnaryFn[String]:
  override def rebuild(kids: List[Fn[?]]): Fn[String] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], posStr = posStr)

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (String, Lens)] =
    for
      (raw, rawLens) <- recv.resolve(ctx)
      res <- raw match
        case null => ZIO.fail(DynaLensError(posStr, "toLower() found null"))
        case s: String => ZIO.succeed((s.toLowerCase, rawLens))
        case other => ZIO.fail(DynaLensError(posStr, s"toLower() expects String, got ${other.getClass.getSimpleName}"))
    yield res


case class ToUpperFn(recv: Fn[Any], posStr: String)
  extends Fn[String] with UnaryFn[String]:
  override def rebuild(kids: List[Fn[?]]): Fn[String] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], posStr = posStr)

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (String, Lens)] =
    for
      (raw, rawLens) <- recv.resolve(ctx)
      res <- raw match
        case null => ZIO.fail(DynaLensError(posStr, "toUpper() found null"))
        case s: String => ZIO.succeed((s.toUpperCase, rawLens))
        case other => ZIO.fail(DynaLensError(posStr, s"toUpper() expects String, got ${other.getClass.getSimpleName}"))
    yield res


case class InterpolateFn(
                          recv: Fn[Any],
                          varMap: Map[String, Fn[Any]],
                          posStr: String
                        ) extends UnaryFn[String]:
  override def rebuild(kids: List[Fn[?]]): Fn[String] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], posStr = posStr)

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (String, Lens)] =
    for
      (raw, rawLens) <- recv.resolve(ctx)
      pat <- raw match
        case null => ZIO.fail(DynaLensError(posStr, "template() pattern was null"))
        case s: String => ZIO.succeed(s)
        case other => ZIO.fail(DynaLensError(posStr, s"template() expects String, got ${other.getClass.getSimpleName}"))
      filled <- ZIO.foreach(TemplateUtils.extractVariables(pat).toList) { v =>
        varMap.get(v) match
          case Some(fn) => fn.resolve(ctx).map(values => v -> values._1)
          case None => ZIO.succeed(v -> null.asInstanceOf[Any])
      }.map(_.toMap)
    yield (TemplateUtils.fill(pat, filled), rawLens)


case class SubstringFn(
                        recv: Fn[Any],
                        start: Fn[Any],
                        end: Option[Fn[Any]],
                        posStr: String
                      ) extends MethodFn[String]:
  override def args: List[Fn[Any]] = end.map(e => List(start, e)).getOrElse(List(start))

  override def rebuild(kids: List[Fn[?]]): Fn[String] =
    kids match
      case r :: s :: e :: Nil => copy(recv = r.asInstanceOf[Fn[Any]], start = s.asInstanceOf[Fn[Any]], end = Some(e.asInstanceOf[Fn[Any]]), posStr = posStr)
      case r :: s :: Nil => copy(recv = r.asInstanceOf[Fn[Any]], start = s.asInstanceOf[Fn[Any]], end = None, posStr = posStr)
      case _ => this

  private def toIndex(n: Any): Option[Int] = n match
    case i: java.lang.Integer => Some(i.intValue)
    case l: java.lang.Long => Some(l.intValue)
    case f: java.lang.Float => Some(f.toInt)
    case d: java.lang.Double => Some(d.toInt)
    case s: java.lang.Short => Some(s.toInt)
    case b: java.lang.Byte => Some(b.toInt)
    case _ => None

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (String, Lens)] =
    for
      (rawS, rawLens) <- recv.resolve(ctx)
      s <- rawS match
        case null => ZIO.fail(DynaLensError(posStr, "substring() receiver was null"))
        case str: String => ZIO.succeed(str)
        case other => ZIO.fail(DynaLensError(posStr, s"substring() expects String, got ${other.getClass.getSimpleName}"))
      fromV <- start.resolve(ctx)
      from <- ZIO.fromOption(toIndex(fromV._1)).mapError(_ => DynaLensError(posStr, "substring() start must be numeric"))
      toOpt <- end match
        case None => ZIO.succeed(None)
        case Some(e) => e.resolve(ctx).flatMap(v => ZIO.fromOption(toIndex(v._1)).map(Some(_)).mapError(_ => DynaLensError(posStr, "substring() end must be numeric")))
      
      // If start index is beyond the string length, return empty string + original lens
      _ <-
        if from >= s.length then
          ZIO.succeed(())
        else
          ZIO.succeed(())
      
      (lo, hi0) =
        if from >= s.length then
          (s.length, s.length)
        else
          (Math.max(0, from), toOpt.getOrElse(s.length))
      
      hi = Math.max(lo, Math.min(hi0, s.length))
    yield (s.substring(lo, hi), rawLens)


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
        copy(recv = r.asInstanceOf[Fn[Any]], target = t.asInstanceOf[Fn[Any]], replacement = rp.asInstanceOf[Fn[Any]], posStr = posStr)
      case _ => this

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (String, Lens)] =
    for
      (s, rawLens) <- recv.resolve(ctx).flatMap {
        case (null, _) => ZIO.fail(DynaLensError(posStr, "replace() receiver was null"))
        case (str: String, lens) => ZIO.succeed((str, lens))
        case (other, _) => ZIO.fail(DynaLensError(posStr, s"replace() expects String, got ${other.getClass.getSimpleName}"))
      }
      t <- target.resolve(ctx).map {
        case (null | None, _) => ""
        case (Some(x), _) => x.toString
        case (x, _) => x.toString
      }
      rep <- replacement.resolve(ctx).map {
        case (null | None, _) => ""
        case (Some(x), _) => x.toString
        case (x, _) => x.toString
      }
    yield (s.replace(t, rep), rawLens)


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
          args = tail.asInstanceOf[List[Fn[Any]]],
          posStr = posStr
        )
      case _ =>
        copy(args = Nil, posStr = posStr)

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (String, Lens)] =
    for
      (recvVal, recvLens) <- recv.resolve(ctx)
      argValsWithLens <- ZIO.foreach(args)(_.resolve(ctx))
      argVals = argValsWithLens.map(_._1)

      result <- ZIO.attempt {
        val sb = new StringBuilder

        def appendAny(value: Any): Unit =
          value match
            case null | None => ()
            case Some(v) => appendAny(v) // ✅ flatten nested Options
            case xs: Iterable[?] =>
              xs.foreach(x => appendAny(x)) // ✅ flatten collections
            case xs: Seq[?] =>
              xs.foreach(x => appendAny(x)) // ✅ flatten sequences
            case v =>
              sb.append(v.toString)

        appendAny(recvVal)
        argVals.foreach(appendAny)
        sb.toString
      }.catchAll(e => ZIO.fail(DynaLensError(posStr, s"concat (+) failed at $posStr: ${e.getMessage}")))

    yield (result, recvLens)