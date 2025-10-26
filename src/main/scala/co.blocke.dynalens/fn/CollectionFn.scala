package co.blocke.dynalens
package fn

import zio.*

import co.blocke.dynalens.fn.FnUtils.asSeq


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
case class ConsFn(left: Fn[Any], right: Fn[Any], posStr: String)
  extends BinaryFn[List[Any]]:

  override val recv: Fn[Any] = left
  override val args: List[Fn[Any]] = List(right)
  override def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(
      left = kids.head.asInstanceOf[Fn[Any]],
      right = kids(1).asInstanceOf[Fn[Any]],
      posStr = posStr
    )

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (List[Any], Lens)] =
    for
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
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
case class FilterFn(recv: Fn[Any], other: Fn[Any], posStr: String)
  extends BinaryFn[List[Any]]:

  override val args: List[Fn[Any]] = List(other)
  def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(
      recv = kids.head.asInstanceOf[Fn[Any]],
      other = kids(1).asInstanceOf[Fn[Any]],
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
            other.resolve(scoped).map(_._1)
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
case class CleanFn(recv: Fn[Any], posStr: String)
  extends UnaryFn[List[Any]]:

  def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(
      recv = kids.head.asInstanceOf[Fn[Any]],
      posStr = posStr
    )

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (List[Any], Lens)] =
    recv.resolve(ctx).flatMap {
      case (None | null, vLens) => ZIO.succeed((Nil, vLens))
      case (Some(xs: Iterable[?]), vLens) =>
        ZIO.succeed((xs.collect { case Some(v) => v; case v if v != null => v }.toList, vLens))
      case (xs: Iterable[?], vLens) =>
        ZIO.succeed((xs.collect { case Some(v) => v; case v if v != null => v }.toList, vLens))
      case (other, _) => ZIO.fail(DynaLensError(posStr, s"clean() requires List receiver, got ${other.getClass.getSimpleName}"))
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

  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    kids match
      case r :: f :: Nil => copy(recv = r.asInstanceOf[Fn[Any]], fn = f.asInstanceOf[Fn[Any]])
      case _             => this

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    for
      recvRes <- recv.resolve(ctx)
      (recvVal, rLens) = recvRes
      result <- recvVal match
        // ---------- None or null ----------
        case null | None =>
          ZIO.succeed((Nil, rLens))

        // ---------- Map ----------
        case Some(m: Map[?, ?]) =>
          val m2 = m.asInstanceOf[Map[Any, Any]]
          ZIO.foreach(m2.toList) { case (k, v) =>
            ctx.withThisKeyValueScoped(k, v, ScalarLens("key", false, Some(rLens)), rLens) { scoped =>
              scoped.withThisScoped((k, v), rLens) { s2 =>
                fn.resolve(s2).map(_._1)
              }
            }
          }.map { vals =>
            if vals.forall(_.isInstanceOf[Tuple2[?, ?]]) then
              // TODO: Build a MapLens here for proper lensing
              (vals.asInstanceOf[List[(Any, Any)]].toMap, rLens)
            else
              (vals.toList, rLens)
          }
        case m: Map[?, ?] =>
          val m2 = m.asInstanceOf[Map[Any, Any]]
          ZIO.foreach(m2.toList) { case (k, v) =>
            ctx.withThisKeyValueScoped(k, v, ScalarLens("key", false, Some(rLens)), rLens) { scoped =>
              scoped.withThisScoped((k, v), rLens) { s2 =>
                fn.resolve(s2).map(_._1)
              }
            }
          }.map { vals =>
            if vals.forall(_.isInstanceOf[Tuple2[?, ?]]) then
              // TODO: Build a MapLens here for proper lensing
              (vals.asInstanceOf[List[(Any, Any)]].toMap, rLens)
            else
              (vals.toList, rLens)
          }

        // ---------- List or Iterable ----------
        case Some(xs: Iterable[?]) =>
          rLens match
            case ll: ListLens =>
              val elemLens = ll.elementLens
              ZIO.foreach(xs.asInstanceOf[Iterable[Any]].toList) { elem =>
                ctx.withThisScoped(elem, elemLens) { scoped =>
                  fn.resolve(scoped).map(_._1)
                }
              }.map(l => (l, rLens))
            case _ =>
              ZIO.fail(DynaLensError(posStr, s"Receiver lens for => must be ListLens, got ${rLens.getClass.getSimpleName}"))
        case xs: Iterable[?] =>
          rLens match
            case ll: ListLens =>
              val elemLens = ll.elementLens
              ZIO.foreach(xs.asInstanceOf[Iterable[Any]].toList) { elem =>
                ctx.withThisScoped(elem, elemLens) { scoped =>
                  fn.resolve(scoped).map(_._1)
                }
              }.map(l => (l, rLens))
            case _ =>
              ZIO.fail(DynaLensError(posStr, s"Receiver lens for => must be ListLens, got ${rLens.getClass.getSimpleName}"))

        // ---------- Invalid receiver ----------
        case other =>
          ZIO.fail(
            DynaLensError(posStr, s"Receiver for => must be List or Map, got ${other.getClass.getSimpleName}")
          )
    yield result