package co.blocke.dynalens
package fn

import zio.*

import co.blocke.dynalens.fn.FnUtils.asSeq


  // Helper to set parent on a lens, preserving type
  private def withParent(l: Lens, parent: Lens): Lens =
    l match
      case sl: ScalarLens => sl.copy(parent = Some(parent))
      case ll: ListLens   => ll.copy(parent = Some(parent))
      case ml: MapLens    => ml.copy(parent = Some(parent))
      case el: EnumLens   => el.copy(parent = Some(parent))
      case other          => other


object FnUtils {
  /** Convert anything that should represent a collection into a List[Any],
   * unwrapping Option/None and normalizing null to Nil.
   */
  def asSeq(value: Any, opName: String, posStr: String): Either[DynaLensError, List[Any]] = value match {
    case null | None =>
      Right(Nil)

    case s: Seq[?] =>
      Right(s.asInstanceOf[Seq[Any]].toList)

    case i: Iterable[?] =>
      Right(i.asInstanceOf[Iterable[Any]].toList)

    case Some(inner) =>
      asSeq(inner, opName, posStr) // recursive unwrap

    case other =>
      Left(DynaLensError(posStr, s"$opName() may only be applied to Iterable types, but got: ${other.getClass.getSimpleName}"))
  }
}


/*---------------------------------------------
  :: (cons)
---------------------------------------------*/
case class ConsFn(recv: Fn[Any], arg: Fn[Any], posStr: String)
  extends BinaryFn[List[Any]]:
  
  val resultType: FieldType = ListType("", ScalarType("", "scala.Any", false), "scala.List[Any]")
  override def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(
      recv = kids.head.asInstanceOf[Fn[Any]],
      arg = kids(1).asInstanceOf[Fn[Any]],
      posStr = posStr
    )

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (List[Any], Lens)] =
    for
      l <- recv.resolve(ctx)
      r <- arg.resolve(ctx)
      (lv, lLens) = l
      (rv, _) = r
      result <- ZIO.attempt {
        (lv, rv) match
          case (null, _) | (None, _) => Nil
          case (Some(v), lst: List[?]) => v :: lst.asInstanceOf[List[Any]]
          case (v, lst: List[?]) => v :: lst.asInstanceOf[List[Any]]
          case (v, Some(lst: List[?])) => v :: lst.asInstanceOf[List[Any]]
          case (v, x) => List(v, x)
      }.catchAll(e => ZIO.fail(DynaLensError(posStr, s":: failed: ${e.getMessage}")))
    yield (result, lLens)


/*---------------------------------------------
  keys()
---------------------------------------------*/
case class KeysFn(recv: Fn[Any], posStr: String)
  extends UnaryFn[List[Any]]:

  def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(
      recv = kids.head.asInstanceOf[Fn[Any]],
      posStr = posStr
    )

  val resultType: FieldType = ListType("", ScalarType("", "scala.Any", false), "scala.List[Any]")

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (List[Any], Lens)] =
    recv.resolve(ctx).flatMap {
      case (null | None, mLens) => ZIO.succeed((Nil, mLens))
      case (Some(m: Map[?, ?]), mLens) =>
        val keysList = m.keys.toList.asInstanceOf[List[Any]]
        val keyLens: Lens = mLens match
          case _ => ScalarLens("key", mLens.isOptional, Some(mLens))
        ZIO.succeed((keysList, keyLens))
      case (m: Map[?, ?], mLens) =>
        val keysList = m.keys.toList.asInstanceOf[List[Any]]
        val keyLens: Lens = mLens match
          case _ => ScalarLens("key", mLens.isOptional, Some(mLens))
        ZIO.succeed((keysList, keyLens))
      case (Some(inner), _) =>
        ZIO.fail(DynaLensError(posStr, s"keys() requires Map receiver, got ${inner.getClass.getSimpleName}"))
      case (other, _) =>
        ZIO.fail(DynaLensError(posStr, s"keys() requires Map receiver, got ${other.getClass.getSimpleName}"))
    }


/*---------------------------------------------
  values()
---------------------------------------------*/
case class ValuesFn(recv: Fn[Any], posStr: String)
  extends UnaryFn[List[Any]]:

  def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(
      recv = kids.head.asInstanceOf[Fn[Any]],
      posStr = posStr
    )

  val resultType: FieldType = ListType("", ScalarType("", "scala.Any", false), "scala.List[Any]")

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (List[Any], Lens)] =
    recv.resolve(ctx).flatMap {
      case (null | None, mLens) => ZIO.succeed((Nil, mLens))
      case (Some(m: Map[?, ?]), mLens) =>
        val valuesList = m.values.toList.asInstanceOf[List[Any]]
        val valueLens: Lens = mLens match
          case _ => ScalarLens("value", mLens.isOptional, Some(mLens))
        ZIO.succeed((valuesList, valueLens))
      case (m: Map[?, ?], mLens) =>
        val valuesList = m.values.toList.asInstanceOf[List[Any]]
        val valueLens: Lens = mLens match
          case _ => ScalarLens("value", mLens.isOptional, Some(mLens))
        ZIO.succeed((valuesList, valueLens))
      case (Some(inner), _) =>
        ZIO.fail(DynaLensError(posStr, s"values() requires Map receiver, got ${inner.getClass.getSimpleName}"))
      case (other, _) =>
        ZIO.fail(DynaLensError(posStr, s"values() requires Map receiver, got ${other.getClass.getSimpleName}"))
    }


/*---------------------------------------------
  filter()
---------------------------------------------*/
case class FilterFn(recv: Fn[Any], arg: Fn[Any], posStr: String)
  extends BinaryFn[List[Any]]:

  override val resultType: FieldType = recv.resultType match
    case l: ListType => l.copy(isOptional = false) // preserve element type, normalize optional
    case _ =>
      ListType("", ScalarType("", "scala.Any", false), "scala.List[Any]")
      
  def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(
      recv = kids.head.asInstanceOf[Fn[Any]],
      arg = kids(1).asInstanceOf[Fn[Any]],
      posStr = posStr
    )

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (List[Any], Lens)] =
    recv.resolve(ctx).flatMap {
      case (None | null, vLens) => ZIO.succeed((Nil, vLens))
      case (Some(lst: Iterable[?]), vLens) => applyFilter(lst.asInstanceOf[Iterable[Any]], vLens, ctx)
      case (lst: Iterable[?], vLens) => applyFilter(lst.asInstanceOf[Iterable[Any]], vLens, ctx)
      case (other, _) => ZIO.fail(DynaLensError(posStr, s"filter() requires List receiver, got ${other.getClass.getSimpleName}"))
    }

  private def applyFilter(lst: Iterable[Any], vLens: Lens, ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (List[Any], Lens)] =
    vLens match
      case ll: ListLens =>
        val elemLens = ll.elementLens
        ZIO.foreach(lst.toList) { elem =>
          ctx.withThisScoped(elem, elemLens) { scoped =>
            arg.resolve(scoped).map(_._1)
          }.either
        }.map { results =>
          val zipped = lst.toList.zip(results)
          val filtered = zipped.collect { case (x, Right(true)) => x }
          (filtered, vLens)
        }
      case _ =>
        ZIO.fail(DynaLensError(posStr, s"filter() requires List receiver with ListLens, got ${vLens.getClass.getSimpleName}"))


/*---------------------------------------------
  sortAsc()
---------------------------------------------*/
case class SortAscFn(recv: Fn[Any], fieldOpt: Option[String], posStr: String)
  extends MethodFn[List[Any]]:

  override def args: List[Fn[Any]] = Nil
  val resultType: FieldType = ListType("", ScalarType("", "scala.Any", false), "scala.List[Any]")

  def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(
      recv = kids.head.asInstanceOf[Fn[Any]],
      fieldOpt = fieldOpt,
      posStr = posStr
    )

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (List[Any], Lens)] =
    recv.resolve(ctx).flatMap {
      case (None | null, vLens) => ZIO.succeed((Nil, vLens))
      case (Some(xs: Iterable[?]), vLens) =>
        val result = fieldOpt match
          case None => xs.asInstanceOf[Iterable[Any]].toList.sortBy(_.toString)
          case Some(field) =>
            xs.asInstanceOf[Iterable[Any]].toList.sortBy {
              case m: Map[?, ?] => m.asInstanceOf[Map[String, Any]].getOrElse(field, "").toString
              case o => o.toString
            }
        ZIO.succeed((result, vLens))
      case (xs: Iterable[?], vLens) =>
        val result = fieldOpt match
          case None => xs.asInstanceOf[Iterable[Any]].toList.sortBy(_.toString)
          case Some(field) =>
            xs.asInstanceOf[Iterable[Any]].toList.sortBy {
              case m: Map[?, ?] => m.asInstanceOf[Map[String, Any]].getOrElse(field, "").toString
              case o => o.toString
            }
        ZIO.succeed((result, vLens))
      case (other, _) => ZIO.fail(DynaLensError(posStr, s"sortAsc() requires List receiver, got ${other.getClass.getSimpleName}"))
    }


/*---------------------------------------------
  sortDesc()
---------------------------------------------*/
case class SortDescFn(recv: Fn[Any], fieldOpt: Option[String], posStr: String)
  extends MethodFn[List[Any]]:

  override def args: List[Fn[Any]] = Nil
  val resultType: FieldType = ListType("", ScalarType("", "scala.Any", false), "scala.List[Any]")

  def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(
      recv = kids.head.asInstanceOf[Fn[Any]],
      fieldOpt = fieldOpt,
      posStr = posStr
    )

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (List[Any], Lens)] =
    recv.resolve(ctx).flatMap {
      case (None | null, vLens) => ZIO.succeed((Nil, vLens))
      case (Some(xs: Iterable[?]), vLens) =>
        val result = fieldOpt match
          case None => xs.asInstanceOf[Iterable[Any]].toList.sortBy(_.toString).reverse
          case Some(field) =>
            xs.asInstanceOf[Iterable[Any]].toList.sortBy {
              case m: Map[?, ?] => m.asInstanceOf[Map[String, Any]].getOrElse(field, "").toString
              case o => o.toString
            }.reverse
        ZIO.succeed((result, vLens))
      case (xs: Iterable[?], vLens) =>
        val result = fieldOpt match
          case None => xs.asInstanceOf[Iterable[Any]].toList.sortBy(_.toString).reverse
          case Some(field) =>
            xs.asInstanceOf[Iterable[Any]].toList.sortBy {
              case m: Map[?, ?] => m.asInstanceOf[Map[String, Any]].getOrElse(field, "").toString
              case o => o.toString
            }.reverse
        ZIO.succeed((result, vLens))
      case (other, _) => ZIO.fail(DynaLensError(posStr, s"sortDesc() requires List receiver, got ${other.getClass.getSimpleName}"))
    }


/*---------------------------------------------
  distinct()
---------------------------------------------*/
case class DistinctFn(recv: Fn[Any], fieldOpt: Option[String], posStr: String)
  extends MethodFn[List[Any]]:

  override def args: List[Fn[Any]] = Nil
  val resultType: FieldType = ListType("", ScalarType("", "scala.Any", false), "scala.List[Any]")

  def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(
      recv = kids.head.asInstanceOf[Fn[Any]],
      fieldOpt = fieldOpt,
      posStr = posStr
    )

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (List[Any], Lens)] =
    recv.resolve(ctx).flatMap {
      case (None | null, vLens) => ZIO.succeed((Nil, vLens))
      case (Some(xs: Iterable[?]), vLens) =>
        val result = fieldOpt match
          case None => xs.asInstanceOf[Iterable[Any]].toList.distinct
          case Some(field) =>
            xs.asInstanceOf[Iterable[Any]].toList.groupBy {
              case m: Map[?, ?] => m.asInstanceOf[Map[String, Any]].getOrElse(field, "")
              case o => o.toString
            }.map(_._2.head).toList
        ZIO.succeed((result, vLens))
      case (m: Map[?, ?], _) =>
        ZIO.fail(DynaLensError(posStr, s"distinct() requires List receiver, got ${m.getClass.getSimpleName}"))
      case (xs: Iterable[?], vLens) =>
        val result = fieldOpt match
          case None => xs.asInstanceOf[Iterable[Any]].toList.distinct
          case Some(field) =>
            xs.asInstanceOf[Iterable[Any]].toList.groupBy {
              case m: Map[?, ?] => m.asInstanceOf[Map[String, Any]].getOrElse(field, "")
              case o => o.toString
            }.map(_._2.head).toList
        ZIO.succeed((result, vLens))
      case (other, _) => ZIO.fail(DynaLensError(posStr, s"distinct() requires List receiver, got ${other.getClass.getSimpleName}"))
    }


/*---------------------------------------------
  limit()
---------------------------------------------*/
case class LimitFn(recv: Fn[Any], count: Int, posStr: String)
  extends UnaryFn[List[Any]]:

  def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(
      recv = kids.head.asInstanceOf[Fn[Any]],
      count = count,
      posStr = posStr
    )
  val resultType: FieldType = ListType("", ScalarType("", "scala.Any", false), "scala.List[Any]")

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (List[Any], Lens)] =
    recv.resolve(ctx).flatMap {
      case (None | null, vLens) => ZIO.succeed((Nil, vLens))
      case (Some(xs: Iterable[?]), vLens) => ZIO.succeed((xs.take(count).toList, vLens))
      case (xs: Iterable[?], vLens) => ZIO.succeed((xs.take(count).toList, vLens))
      case (other, _) => ZIO.fail(DynaLensError(posStr, s"limit() requires List receiver, got ${other.getClass.getSimpleName}"))
    }


/*---------------------------------------------
  reverse()
---------------------------------------------*/
case class ReverseFn(recv: Fn[Any], posStr: String)
  extends UnaryFn[List[Any]]:

  def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(
      recv = kids.head.asInstanceOf[Fn[Any]],
      posStr = posStr
    )
  val resultType: FieldType = ListType("", ScalarType("", "scala.Any", false), "scala.List[Any]")

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (List[Any], Lens)] =
    recv.resolve(ctx).flatMap {
      case (None | null, vLens) => ZIO.succeed((Nil, vLens))
      case (Some(xs: Iterable[?]), vLens) => ZIO.succeed((xs.toList.reverse, vLens))
      case (xs: Iterable[?], vLens) => ZIO.succeed((xs.toList.reverse, vLens))
      case (other, _) => ZIO.fail(DynaLensError(posStr, s"reverse() requires List receiver, got ${other.getClass.getSimpleName}"))
    }


/*---------------------------------------------
  clean()
---------------------------------------------*/
case class CleanFn(recv: Fn[Any], resultType: FieldType, posStr: String) extends UnaryFn[Any]:
  def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(
      recv = kids.head.asInstanceOf[Fn[Any]],
      resultType = resultType,
      posStr = posStr
    )

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    recv.resolve(ctx).map { case (resolved, lLens) =>
      val cleaned = resolved match
        case null | None => Nil
        case xs: Iterable[_] =>
          xs.filter(e => e != null && e != None).toList
        case Some(iterable: Iterable[_]) =>
          val cleaned = iterable.filterNot(e => e == null || e == None)
          Some(cleaned)
        case x =>
          Nil

      (cleaned, lLens)
    }

/*---------------------------------------------
  len()
---------------------------------------------*/
case class LenFn(recv: Fn[Any], posStr: String)
  extends UnaryFn[Int]:

  def rebuild(kids: List[Fn[?]]): Fn[Int] =
    copy(
      recv = kids.head.asInstanceOf[Fn[Any]],
      posStr = posStr
    )
  val resultType: FieldType = ScalarType("", "scala.Int", false)

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Int, Lens)] =
    recv.resolve(ctx).flatMap {
      case (None | null, vLens) => ZIO.succeed((0, ScalarLens("len", false, Some(vLens))))
      case (Some(s: String), vLens) => ZIO.succeed((s.length, ScalarLens("len", false, Some(vLens))))
      case (s: String, vLens) => ZIO.succeed((s.length, ScalarLens("len", false, Some(vLens))))
      case (Some(m: Map[?, ?]), vLens) => ZIO.succeed((m.size, ScalarLens("len", false, Some(vLens))))
      case (m: Map[?, ?], vLens) => ZIO.succeed((m.size, ScalarLens("len", false, Some(vLens))))
      case (Some(xs: Iterable[?]), vLens) => ZIO.succeed((xs.size, ScalarLens("len", false, Some(vLens))))
      case (xs: Iterable[?], vLens) => ZIO.succeed((xs.size, ScalarLens("len", false, Some(vLens))))
      case (other, _) => ZIO.fail(DynaLensError(posStr, s"len() requires String, List, or Map receiver, got ${other.getClass.getSimpleName}"))
    }


/*---------------------------------------------
  mapTo()
---------------------------------------------*/
case class MapToFn(mapName: String, recv: Fn[Any], posStr: String)
  extends MethodFn[Any]:

  override val methodName: String = "mapTo"
  override val isOptional: Boolean = recv.isOptional

  override def args: List[Fn[Any]] = Nil
  override val resultType: FieldType = recv.resultType

  def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(
      recv = kids.head.asInstanceOf[Fn[Any]],
      posStr = posStr
    )

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    for {
      recvRes <- recv.resolve(ctx)
      (raw, rLens) = recvRes
      res <- ZIO.serviceWithZIO[RuntimeEnv] { reg =>
        reg.biMapRegistry.get(mapName) match
          case Some(bimap) =>
            asSeq(raw, s"mapTo($mapName)", posStr) match
              // ---- collection case ----
              case Right(seq) =>
                ZIO.foreach(seq) { item =>
                  bimap.getForward(item.toString) match
                    case Some(mapped) => ZIO.succeed(mapped)
                    case None =>
                      // soft-fallback: preserve item if not found
                      if isOptional then ZIO.succeed(item)
                      else ZIO.fail(DynaLensError(posStr, s"Key '$item' not found in map '$mapName'"))
                }.map(l => (l.toList, rLens))
              // ---- scalar case ----
              case Left(_) =>
                bimap.getForward(raw.toString) match
                  case Some(mapped) => ZIO.succeed((mapped, rLens))
                  case None =>
                    if isOptional then ZIO.succeed((raw, rLens))
                    else ZIO.fail(DynaLensError(posStr, s"Key '$raw' not found in map '$mapName'"))
          // ---- map not found ----
          case None =>
            if isOptional then ZIO.succeed((raw, rLens))
            else ZIO.fail(DynaLensError(posStr, s"BiMap '$mapName' not registered"))
      }
    } yield res


/*---------------------------------------------
  mapFrom()
---------------------------------------------*/
case class MapFromFn(mapName: String, recv: Fn[Any], posStr: String)
  extends MethodFn[Any]:

  override val methodName: String = "mapFrom"
  override val isOptional: Boolean = recv.isOptional
  override def args: List[Fn[Any]] = Nil
  override val resultType: FieldType = recv.resultType

  def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(
      recv = kids.head.asInstanceOf[Fn[Any]],
      posStr = posStr
    )

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    for {
      recvRes <- recv.resolve(ctx)
      (raw, rLens) = recvRes
      res <- ZIO.serviceWithZIO[RuntimeEnv] { reg =>
        reg.biMapRegistry.get(mapName) match
          case Some(bimap) =>
            asSeq(raw, s"mapFrom($mapName)", posStr) match
              case Right(seq) =>
                ZIO.foreach(seq) { item =>
                  bimap.getReverse(item.toString) match
                    case Some(mapped) => ZIO.succeed(mapped)
                    case None =>
                      if isOptional then ZIO.succeed(item)
                      else ZIO.fail(DynaLensError(posStr, s"Value '$item' not found in reverse map '$mapName'"))
                }.map(l => (l.toList, rLens))
              case Left(_) =>
                bimap.getReverse(raw.toString) match
                  case Some(mapped) => ZIO.succeed((mapped, rLens))
                  case None =>
                    if isOptional then ZIO.succeed((raw, rLens))
                    else ZIO.fail(DynaLensError(posStr, s"Value '$raw' not found in reverse map '$mapName'"))
          case None =>
            if isOptional then ZIO.succeed((raw, rLens))
            else ZIO.fail(DynaLensError(posStr, s"BiMap '$mapName' not registered"))
      }
    } yield res


/*---------------------------------------------
    => (map)
---------------------------------------------*/
case class MapFn(recv: Fn[Any], fn: Fn[Any], posStr: String)
  extends MethodFn[Any]:

  override val methodName = "=>"

  override def args: List[Fn[Any]] = List(fn)

  val resultType: FieldType = fn.resultType match
    case l: ListType => l
    case m: MapType => m
    case s: ScalarType => s
    case _ => ListType("", ScalarType("", "scala.Any", false), "scala.List[Any]")

  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    kids match
      case r :: f :: Nil => copy(recv = r.asInstanceOf[Fn[Any]], fn = f.asInstanceOf[Fn[Any]])
      case _             => this

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    for
      recvRes <- recv.resolve(ctx)
      (raw, rLens) = recvRes
      // unwrap Option containers before dispatch
      res <- {
        def isEmptyColl(v: Any): Boolean = v match
          case m: Map[?, ?] => m.isEmpty
          case it: Iterable[?] => it.isEmpty
          case _ => false

        if (raw == null) {
          // no-op passthrough
          ZIO.succeed((raw, rLens))
        } else if (rLens.isOptional && isEmptyColl(raw)) {
          // logical None for optional receiver => no-op passthrough
          ZIO.succeed((raw, rLens))
        } else {
          val unwrapped = raw match
            case Some(v) => v
            case None    => Nil
            case v       => v

          if (rLens.isOptional && (unwrapped == Nil || unwrapped == None)) {
            val defaultOut =
              if (fn.isInstanceOf[Tuple2Fn]) Map.empty
              else Nil
            ZIO.succeed((defaultOut, rLens))
          } else
            unwrapped match {
              case m: Map[?, ?] =>
                mapOverMap(m.asInstanceOf[Map[Any, Any]], rLens, ctx)
              case xs: Iterable[?] =>
                mapOverList(xs.asInstanceOf[Iterable[Any]], rLens, ctx)
              case other =>
                ZIO.fail(DynaLensError(posStr, s"=> requires List or Map receiver, got ${other.getClass.getSimpleName}"))
            }
        }
      }
    yield res

  private def mapOverMap(m: Map[Any, Any], rLens: Lens, ctx: DynaContext) =
    val (keyKind, valueLens) = rLens match
      case ml: MapLens =>
        (ml.keyKind, withParent(ml.valueLens, rLens)) // re-anchor valueLens properly
      case _ =>
        (MapKeyKind.StringKey, ScalarLens("value", rLens.isOptional, Some(rLens)))

    // Iterate through map entries
    for vals <- ZIO.foreach(m.toList) { case (k, v) =>
      val keyLens = ScalarLens("key", rLens.isOptional, Some(rLens))
      ctx.withThisKeyValueScoped(k, v, keyLens, valueLens) { scoped =>
        fn.resolve(scoped).map(_._1)
      }
    }
    yield {
      val tuples = vals.collect { case t: (Any, Any) => t }
      val allTuples = tuples.size == vals.size
      if allTuples then
        if (rLens.isOptional && tuples.isEmpty)
          (Nil, rLens)
        else
          val resMap = tuples.toMap
          val resLens = MapLens(
            rLens.name,
            rLens.isOptional,
            keyKind,
            ScalarLens("value", rLens.isOptional, Some(rLens)),
            Some(rLens)
          )
          (resMap, resLens)
      else
        val resLens = ListLens(
          rLens.name,
          rLens.isOptional,
          ScalarLens("value", rLens.isOptional, Some(rLens)),
          Some(rLens)
        )
        (vals, resLens)
    }

  private def mapOverList(xs: Iterable[Any], rLens: Lens, ctx: DynaContext) =
    val elemLens = rLens match
      case ll: ListLens => withParent(ll.elementLens, rLens)
      case _            => ScalarLens("value", rLens.isOptional, Some(rLens))

    ZIO.foreach(xs.toList) { elem =>
      ctx.withThisScoped(elem, elemLens) { scoped =>
        fn.resolve(scoped).map(_._1)
      }
    }.flatMap { vals =>
      // Validation: ensure all resolved values are scalar or Tuple2
      ZIO.foreach(vals) {
        case v@(_: Iterable[?] | _: Map[?, ?]) =>
          ZIO.fail(DynaLensError(posStr, s"=> body must return scalar or tuple2, got ${v.getClass.getSimpleName}"))
        case _ => ZIO.unit
      }.as {
        val tuples = vals.collect { case t: (Any, Any) => t }
        val allTuples = tuples.size == vals.size
        if allTuples then
          val resMap = tuples.toMap
          val resLens = MapLens(rLens.name, rLens.isOptional, MapKeyKind.StringKey,
            ScalarLens("value", rLens.isOptional, Some(rLens)), Some(rLens))
          (resMap, resLens)
        else
          val resLens = ListLens(rLens.name, rLens.isOptional,
            ScalarLens("value", rLens.isOptional, Some(rLens)), Some(rLens))
          (vals, resLens)
      }
    }
