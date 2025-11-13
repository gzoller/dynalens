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
  val resultType: FieldType = ifTrue.resultType
}


/** AND */
case class AndFn(recv: Fn[Any], arg: Fn[Any], posStr: String)
  extends BinaryFn[Boolean]
    with BooleanFn:

  override val methodName: String = "&&"
  val resultType: FieldType = ScalarType("", "scala.Boolean", false)

  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    kids match
      // recv + args list of 1 element
      case r :: a :: Nil =>
        copy(recv = r.asInstanceOf[Fn[Any]], arg = a.asInstanceOf[Fn[Any]])
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
            (rv, _) <- arg.resolve(ctx)
            rvBool <- rv match
              case b: Boolean => ZIO.succeed(b)
              case other => ZIO.fail(DynaLensError(posStr, s"Right side not Boolean: ${other.getClass.getSimpleName}"))
          yield (lvBool && rvBool, lLens)
    yield result


/** OR */
case class OrFn(recv: Fn[Any], arg: Fn[Any], posStr: String)
  extends BinaryFn[Boolean]
    with BooleanFn:

  override val methodName: String = "||"
  val resultType: FieldType = ScalarType("", "scala.Boolean", false)

  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    kids match
      // recv + args list of 1 element
      case r :: a :: Nil =>
        copy(recv = r.asInstanceOf[Fn[Any]], arg = a.asInstanceOf[Fn[Any]])
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
            (rv, _) <- arg.resolve(ctx)
            rvBool <- rv match
              case b: Boolean => ZIO.succeed(b)
              case other => ZIO.fail(DynaLensError(posStr, s"Right side not Boolean: ${other.getClass.getSimpleName}"))
          yield (lvBool || rvBool, lLens)
    yield result


/** NOT */
case class NotFn(recv: Fn[Any], posStr: String)
  extends UnaryFn[Boolean]
    with BooleanFn {
  val resultType: FieldType = ScalarType("", "scala.Boolean", false)

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
  val resultType: FieldType = ScalarType("", "scala.Boolean", false)

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =

    for
      (v, lens) <- recv.resolve(ctx)
      result = v match {
        case Nil         => false           // empty list treated like None
        case None        => false
        case null        => false
        case _           => true
      }
    yield (result, lens)

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])
}


/** STARTSWITH */
case class StartsWithFn(recv: Fn[Any], arg: Fn[Any], posStr: String)
  extends BinaryFn[Boolean]
    with BooleanFn {
  val resultType: FieldType = ScalarType("", "scala.Boolean", false)

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (sVal, lLens) <- recv.resolve(ctx)
      (oVal, _) <- arg.resolve(ctx)
      result <- ZIO.attempt {
        (Option(sVal).map(_.toString).getOrElse(""))
          .startsWith(Option(oVal).map(_.toString).getOrElse(""))
      }.catchAll(e => ZIO.fail(DynaLensError(posStr, s"startsWith() failed: ${e.getMessage}")))
    yield (result, lLens)

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])
}


/** ENDSWITH */
case class EndsWithFn(recv: Fn[Any], arg: Fn[Any], posStr: String)
  extends BinaryFn[Boolean]
    with BooleanFn {
  val resultType: FieldType = ScalarType("", "scala.Boolean", false)

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (sVal, lLens) <- recv.resolve(ctx)
      (oVal, _) <- arg.resolve(ctx)
      result <- ZIO.attempt {
        (Option(sVal).map(_.toString).getOrElse(""))
          .endsWith(Option(oVal).map(_.toString).getOrElse(""))
      }.catchAll(e => ZIO.fail(DynaLensError(posStr, s"endsWith() failed: ${e.getMessage}")))
    yield (result, lLens)

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])
}

/** CONTAINS */
case class ContainsFn(recv: Fn[Any], arg: Fn[Any], posStr: String)
  extends Fn[Boolean]
    with BinaryFn[Boolean]
    with BooleanFn {
  val resultType: FieldType = ScalarType("", "scala.Boolean", false)

  import ContainsFn._

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (hay, lens) <- recv.resolve(ctx)
      result <- containsDynamic(hay, arg, ctx, posStr, lens)
    yield result

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])
}

object ContainsFn {

  @tailrec
  private def containsDynamic(
                               hay: Any,
                               needle: Fn[Any],
                               ctx: DynaContext,
                               posStr: String,
                               lens: Lens
                             ): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] = {
    println(s"[TRACE containsDynamic] ENTER: hay=${Option(hay).map(_.getClass.getSimpleName).getOrElse("null")}, needle=${needle.getClass.getSimpleName}, posStr=$posStr")

    hay match {

      // ---- Option unwraps ----
      case null =>
        println("[TRACE containsDynamic] -> null/None, returning false")
        ZIO.succeed((false, lens))
      case None =>
        println("[TRACE containsDynamic] -> null/None, returning false")
        ZIO.succeed((false, lens))
      case Some(inner) =>
        println(s"[TRACE containsDynamic] -> unwrapping Some(${Option(inner).map(_.getClass.getSimpleName).getOrElse("null")})")
        containsDynamic(inner, needle, ctx, posStr, lens)

      // ---- String: substring ----
      case cs: CharSequence =>
        println(s"[TRACE containsDynamic] -> CharSequence hay='${cs.toString}'")
        for {
          (ndlAny, ndlLens) <- needle.resolve(ctx)
        } yield (cs.toString.contains(Option(ndlAny).fold("null")(_.toString)), lens)

      // ---- Map: key presence (needle evaluated once) ----
      case m: Map[?, ?] =>
        println(s"[TRACE containsDynamic] -> Map keys=${m.size}")
        for {
          (ndlVal, ndlLens) <- needle.resolve(ctx)
        } yield (m.asInstanceOf[Map[Any, Any]].contains(ndlVal), lens)

      // ---- Iterable: supports predicate OR value check ----
      case it: Iterable[?] =>
        println(s"[TRACE containsDynamic] -> Iterable size=${it.size}")
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
            } yield {
              val haystack = it.asInstanceOf[Iterable[Any]]
              val matchFound = haystack.exists { elem =>
                (elem, ndlVal) match
                  // handle Scala boxed/unboxed equality across Int/Long/Double
                  case (a: Number, b: Number) => a.doubleValue() == b.doubleValue()
                  // handle Option flattening symmetry
                  case (Some(a), b) => a == b
                  case (a, Some(b)) => a == b
                  // fallback to safe equals
                  case (a, b) => a == b
              }
              (matchFound, lens)
            }
        }

      // ---- Unsupported receiver types ----
      case other =>
        println(s"[TRACE containsDynamic] -> Unsupported type ${other.getClass.getSimpleName}")
        ZIO.fail(
          DynaLensError(
            posStr,
            s"contains() not supported for ${other.getClass.getSimpleName}; expected String, Iterable, Map, or Option thereof"
          )
        )
    }
  }
}


/** EQUALSIGNORECASE */
case class EqualsIgnoreCaseFn(recv: Fn[Any], arg: Fn[Any], posStr: String)
  extends BinaryFn[Boolean]
    with BooleanFn {
  val resultType: FieldType = ScalarType("", "scala.Boolean", false)

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (rVal, lLens) <- recv.resolve(ctx)
      (oVal, _) <- arg.resolve(ctx)
      result <- ZIO.attempt {
        (Option(rVal).map(_.toString).getOrElse(""))
          .equalsIgnoreCase(Option(oVal).map(_.toString).getOrElse(""))
      }.catchAll(e => ZIO.fail(DynaLensError(posStr, s"equalsIgnoreCase() failed: ${e.getMessage}")))
    yield (result, lLens)

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])
}


/** MATCHESREGEX */
case class MatchesRegexFn(recv: Fn[Any], arg: Fn[Any], posStr: String)
  extends BinaryFn[Boolean]
    with BooleanFn {

  val resultType: FieldType = ScalarType("", "scala.Boolean", false)
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (sVal, lLens) <- recv.resolve(ctx)
      (rVal, _) <- arg.resolve(ctx)
      result <- ZIO.attempt {
        val s = Option(sVal).map(_.toString).getOrElse("")
        val r = Option(rVal).map(_.toString).getOrElse("")
        s.matches(r)
      }.catchAll(e => ZIO.fail(DynaLensError(posStr, s"matchesRegex() failed: ${e.getMessage}")))
    yield (result, lLens)

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], arg = kids(1).asInstanceOf[Fn[Any]])
}


case class ToBooleanFn(inner: Fn[Any], posStr: String)
  extends UnaryFn[Boolean]
    with BooleanFn {

  val resultType: FieldType = ScalarType("", "scala.Boolean", false)

  override val recv: Fn[Any] = inner
  override val args: List[Fn[Any]] = Nil

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Boolean, Lens)] =
    for
      (v, lens) <- inner.resolve(ctx)
      typedResult <- v match
        case b: Boolean => ZIO.succeed(b)
        case other =>
          ZIO.fail(DynaLensError(posStr,
            s"Expected Boolean result at runtime, but got: ${other.getClass.getSimpleName} = $other"))
    yield (typedResult, lens)

  def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(inner = kids.head.asInstanceOf[Fn[Any]])
}