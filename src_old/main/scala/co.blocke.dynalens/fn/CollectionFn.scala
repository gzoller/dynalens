package co.blocke.dynalens
package fn

import zio.*
import FnUtils.*


/*---------------------------------------------
  :: (cons)
---------------------------------------------*/
case class ConsFn(left: Fn[Any], right: Fn[Any], posStr: String)
  extends BinaryFn[List[Any]]:

  override val recv: Fn[Any] = left
  override val args: List[Fn[Any]] = List(right)
  override def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(left = kids.head.asInstanceOf[Fn[Any]], right = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, List[Any]] =
    for
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- ZIO.attempt {
        (l, r) match
          case (null, _) | (None, _) => Nil
          case (Some(v), lst: List[?]) => v :: lst.asInstanceOf[List[Any]]
          case (v, lst: List[?]) => v :: lst.asInstanceOf[List[Any]]
          case (v, Some(lst: List[?])) => v :: lst.asInstanceOf[List[Any]]
          case (v, _) => List(v, r)
      }.catchAll(e => ZIO.fail(DynaLensError(posStr, s":: failed: ${e.getMessage}")))
    yield result


/*---------------------------------------------
  keys()
---------------------------------------------*/
case class KeysFn(recv: Fn[Any], posStr: String)
  extends UnaryFn[List[Any]]:

  def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, List[Any]] =
    recv.resolve(ctx).flatMap {
      case null | None => ZIO.succeed(Nil)
      case Some(m: Map[?, ?]) => ZIO.succeed(m.keys.toList)
      case m: Map[?, ?]       => ZIO.succeed(m.keys.toList)
      case Some(inner)        => ZIO.fail(DynaLensError(posStr, s"keys() requires Map receiver, got ${inner.getClass.getSimpleName}"))
      case other              => ZIO.fail(DynaLensError(posStr, s"keys() requires Map receiver, got ${other.getClass.getSimpleName}"))
    }


/*---------------------------------------------
  values()
---------------------------------------------*/
case class ValuesFn(recv: Fn[Any], posStr: String)
  extends UnaryFn[List[Any]]:

  def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, List[Any]] =
    recv.resolve(ctx).flatMap {
      case null | None => ZIO.succeed(Nil)
      case Some(m: Map[?, ?]) => ZIO.succeed(m.values.toList)
      case m: Map[?, ?]       => ZIO.succeed(m.values.toList)
      case Some(inner)        => ZIO.fail(DynaLensError(posStr, s"values() requires Map receiver, got ${inner.getClass.getSimpleName}"))
      case other              => ZIO.fail(DynaLensError(posStr, s"values() requires Map receiver, got ${other.getClass.getSimpleName}"))
    }


/*---------------------------------------------
  filter()
---------------------------------------------*/
case class FilterFn(recv: Fn[Any], other: Fn[Any], posStr: String)
  extends BinaryFn[List[Any]]:

  override val args: List[Fn[Any]] = List(other)
  def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]], other = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, List[Any]] =
    recv.resolve(ctx).flatMap {
      case None | null => ZIO.succeed(Nil)
      case Some(lst: Iterable[?]) => applyFilter(lst.asInstanceOf[Iterable[Any]], ctx)
      case lst: Iterable[?]       => applyFilter(lst.asInstanceOf[Iterable[Any]], ctx)
      case other => ZIO.fail(DynaLensError(posStr, s"filter() requires List receiver, got ${other.getClass.getSimpleName}"))
    }

  private def applyFilter(lst: Iterable[Any], ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, List[Any]] =
    ZIO
      .foreach(lst.toList) { elem =>
        withThisScoped(ctx, elem) { other.resolve(ctx) }.either
      }
      .map(_.collect { case Right(true) => true })
      .zipWith(ZIO.succeed(lst.toList))((bools, items) => items.zip(bools).collect { case (x, true) => x })


/*---------------------------------------------
  sortAsc()
---------------------------------------------*/
case class SortAscFn(recv: Fn[Any], fieldOpt: Option[String], posStr: String)
  extends MethodFn[List[Any]]:

  override def args: List[Fn[Any]] = Nil

  def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, List[Any]] =
    recv.resolve(ctx).flatMap {
      case None | null => ZIO.succeed(Nil)
      case Some(xs: Iterable[?]) => sort(xs.asInstanceOf[Iterable[Any]])
      case xs: Iterable[?]       => sort(xs.asInstanceOf[Iterable[Any]])
      case other => ZIO.fail(DynaLensError(posStr, s"sortAsc() requires List receiver, got ${other.getClass.getSimpleName}"))
    }

  private def sort(xs: Iterable[Any]): ZIO[Any, Nothing, List[Any]] =
    ZIO.succeed {
      fieldOpt match
        case None => xs.toList.sortBy(_.toString)
        case Some(field) =>
          xs.toList.sortBy {
            case m: Map[?, ?] => m.asInstanceOf[Map[String, Any]].getOrElse(field, "").toString
            case o => o.toString
          }
    }


/*---------------------------------------------
  sortDesc()
---------------------------------------------*/
case class SortDescFn(recv: Fn[Any], fieldOpt: Option[String], posStr: String)
  extends MethodFn[List[Any]]:

  override def args: List[Fn[Any]] = Nil

  def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, List[Any]] =
    recv.resolve(ctx).flatMap {
      case None | null => ZIO.succeed(Nil)
      case Some(xs: Iterable[?]) => sort(xs.asInstanceOf[Iterable[Any]])
      case xs: Iterable[?]       => sort(xs.asInstanceOf[Iterable[Any]])
      case other => ZIO.fail(DynaLensError(posStr, s"sortDesc() requires List receiver, got ${other.getClass.getSimpleName}"))
    }

  private def sort(xs: Iterable[Any]): ZIO[Any, Nothing, List[Any]] =
    ZIO.succeed {
      fieldOpt match
        case None => xs.toList.sortBy(_.toString).reverse
        case Some(field) =>
          xs.toList.sortBy {
            case m: Map[?, ?] => m.asInstanceOf[Map[String, Any]].getOrElse(field, "").toString
            case o => o.toString
          }.reverse
    }


/*---------------------------------------------
  distinct()
---------------------------------------------*/
case class DistinctFn(recv: Fn[Any], fieldOpt: Option[String], posStr: String)
  extends MethodFn[List[Any]]:

  override def args: List[Fn[Any]] = Nil

  def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, List[Any]] =
    recv.resolve(ctx).flatMap {
      case None | null => ZIO.succeed(Nil)
      case Some(xs: Iterable[?]) => distinct(xs.asInstanceOf[Iterable[Any]])
      case xs: Iterable[?]       => distinct(xs.asInstanceOf[Iterable[Any]])
      case other => ZIO.fail(DynaLensError(posStr, s"distinct() requires List receiver, got ${other.getClass.getSimpleName}"))
    }

  private def distinct(xs: Iterable[Any]): ZIO[Any, Nothing, List[Any]] =
    ZIO.succeed {
      fieldOpt match
        case None => xs.toList.distinct
        case Some(field) =>
          xs.toList.groupBy {
            case m: Map[?, ?] => m.asInstanceOf[Map[String, Any]].getOrElse(field, "")
            case o => o.toString
          }.map(_._2.head).toList
    }


/*---------------------------------------------
  limit()
---------------------------------------------*/
case class LimitFn(recv: Fn[Any], count: Int, posStr: String)
  extends UnaryFn[List[Any]]:

  def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, List[Any]] =
    recv.resolve(ctx).flatMap {
      case None | null => ZIO.succeed(Nil)
      case Some(xs: Iterable[?]) => ZIO.succeed(xs.take(count).toList)
      case xs: Iterable[?]       => ZIO.succeed(xs.take(count).toList)
      case other => ZIO.fail(DynaLensError(posStr, s"limit() requires List receiver, got ${other.getClass.getSimpleName}"))
    }


/*---------------------------------------------
  reverse()
---------------------------------------------*/
case class ReverseFn(recv: Fn[Any], posStr: String)
  extends UnaryFn[List[Any]]:

  def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, List[Any]] =
    recv.resolve(ctx).flatMap {
      case None | null => ZIO.succeed(Nil)
      case Some(xs: Iterable[?]) => ZIO.succeed(xs.toList.reverse)
      case xs: Iterable[?]       => ZIO.succeed(xs.toList.reverse)
      case other => ZIO.fail(DynaLensError(posStr, s"reverse() requires List receiver, got ${other.getClass.getSimpleName}"))
    }


/*---------------------------------------------
  clean()
---------------------------------------------*/
case class CleanFn(recv: Fn[Any], posStr: String)
  extends UnaryFn[List[Any]]:

  def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, List[Any]] =
    recv.resolve(ctx).flatMap {
      case None | null => ZIO.succeed(Nil)
      case Some(xs: Iterable[?]) => ZIO.succeed(xs.collect { case Some(v) => v; case v if v != null => v }.toList)
      case xs: Iterable[?]       => ZIO.succeed(xs.collect { case Some(v) => v; case v if v != null => v }.toList)
      case other => ZIO.fail(DynaLensError(posStr, s"clean() requires List receiver, got ${other.getClass.getSimpleName}"))
    }


/*---------------------------------------------
  len()
---------------------------------------------*/
case class LenFn(recv: Fn[Any], posStr: String)
  extends UnaryFn[Int]:

  def rebuild(kids: List[Fn[?]]): Fn[Int] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Int] =
    recv.resolve(ctx).flatMap {
      case None | null => ZIO.succeed(0)
      case Some(s: String) => ZIO.succeed(s.length)
      case s: String       => ZIO.succeed(s.length)
      case Some(m: Map[?, ?])    => ZIO.succeed(m.size)
      case m: Map[?, ?]          => ZIO.succeed(m.size)
      case Some(xs: Iterable[?]) => ZIO.succeed(xs.size)
      case xs: Iterable[?]       => ZIO.succeed(xs.size)
      case other => ZIO.fail(DynaLensError(posStr, s"len() requires String, List, or Map receiver, got ${other.getClass.getSimpleName}"))
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
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      raw <- recv.resolve(ctx)
      res <- ZIO.serviceWithZIO[_BiMapRegistry] { reg =>
        reg.get(mapName) match
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
                }.map(_.toList)

              // ---- scalar case ----
              case Left(_) =>
                bimap.getForward(raw.toString) match
                  case Some(mapped) => ZIO.succeed(mapped)
                  case None =>
                    if isOptional then ZIO.succeed(raw)
                    else ZIO.fail(DynaLensError(posStr, s"Key '$raw' not found in map '$mapName'"))

          // ---- map not found ----
          case None =>
            if isOptional then ZIO.succeed(raw)
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
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      raw <- recv.resolve(ctx)
      res <- ZIO.serviceWithZIO[_BiMapRegistry] { reg =>
        reg.get(mapName) match
          case Some(bimap) =>
            asSeq(raw, s"mapFrom($mapName)", posStr) match
              case Right(seq) =>
                ZIO.foreach(seq) { item =>
                  bimap.getReverse(item.toString) match
                    case Some(mapped) => ZIO.succeed(mapped)
                    case None =>
                      if isOptional then ZIO.succeed(item)
                      else ZIO.fail(DynaLensError(posStr, s"Value '$item' not found in reverse map '$mapName'"))
                }.map(_.toList)
              case Left(_) =>
                bimap.getReverse(raw.toString) match
                  case Some(mapped) => ZIO.succeed(mapped)
                  case None =>
                    if isOptional then ZIO.succeed(raw)
                    else ZIO.fail(DynaLensError(posStr, s"Value '$raw' not found in reverse map '$mapName'"))
          case None =>
            if isOptional then ZIO.succeed(raw)
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

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for
      recvVal <- recv.resolve(ctx)
      result <- recvVal match
        // ---------- None or null ----------
        case null | None =>
          ZIO.succeed(Nil)

        // ---------- Map ----------
        case Some(m: Map[?, ?]) =>
          mapMap(m.asInstanceOf[Map[Any, Any]], ctx)

        case m: Map[?, ?] =>
          mapMap(m.asInstanceOf[Map[Any, Any]], ctx)

        // ---------- List or Iterable ----------
        case Some(xs: Iterable[?]) =>
          mapList(xs.asInstanceOf[Iterable[Any]], ctx)

        case xs: Iterable[?] =>
          mapList(xs.asInstanceOf[Iterable[Any]], ctx)

        // ---------- Invalid receiver ----------
        case other =>
          ZIO.fail(
            DynaLensError(posStr, s"Receiver for => must be List or Map, got ${other.getClass.getSimpleName}")
          )
    yield result

  // ---------- Helpers ----------

  private def mapList(xs: Iterable[Any], ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, List[Any]] =
    ZIO.foreach(xs.toList) { elem =>
      withThisScoped(ctx, elem) {
        fn.resolve(ctx)
      }
    }

  private def mapMap(m: Map[Any, Any], ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, List[Any]] =
    ZIO.foreach(m.toList) { case (k, v) =>
      // Bind both key/value for map iteration
      withKeyScoped(ctx, "key", (k, None)) {
        withKeyScoped(ctx, "value", (v, None)) {
          withThisScoped(ctx, (k, v)) {
            fn.resolve(ctx)
          }
        }
      }
    }