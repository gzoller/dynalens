package co.blocke.dynalens
package fn

import zio.*
import NumPromote.*


case class AbsFn(recv: Fn[Any], posStr: String) extends UnaryFn[Any]:
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext) =
    for
      raw <- recv.resolve(ctx)
      out <- raw match
        case null       => ZIO.fail(DynaLensError(posStr, "abs() found null"))
        case x: Byte    => ZIO.succeed(if x < 0 then (-x).toByte else x)
        case x: Short   => ZIO.succeed(Math.abs(x.toInt).toShort)
        case x: Int     => ZIO.succeed(Math.abs(x))
        case x: Long    => ZIO.succeed(Math.abs(x))
        case x: Float   => ZIO.succeed(Math.abs(x))
        case x: Double  => ZIO.succeed(Math.abs(x))
        case other      => ZIO.fail(DynaLensError(posStr, s"abs() expects numeric, got ${other.getClass.getSimpleName}"))
    yield out


case class MinFn(recv: Fn[Any], posStr: String) extends UnaryFn[Any]:
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext) =
    for
      raw <- recv.resolve(ctx)
      box <- ZIO.fromEither(collect(raw, "min()", posStr))
      v    = toPromotedVector(box)
      res <- box.kind match
        case KDouble => ZIO.succeed(v.asInstanceOf[Vector[Double]].minOption.getOrElse(0.0))
        case KFloat  => ZIO.succeed(v.asInstanceOf[Vector[Float]].minOption.getOrElse(0.0f))
        case KLong   => ZIO.succeed(v.asInstanceOf[Vector[Long]].minOption.getOrElse(0L))
        case KInt    => ZIO.succeed(v.asInstanceOf[Vector[Int]].minOption.getOrElse(0))
    yield res


case class MaxFn(recv: Fn[Any], posStr: String) extends UnaryFn[Any]:
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext) =
    for
      raw <- recv.resolve(ctx)
      box <- ZIO.fromEither(collect(raw, "max()", posStr))
      v    = toPromotedVector(box)
      res <- box.kind match
        case KDouble => ZIO.succeed(v.asInstanceOf[Vector[Double]].maxOption.getOrElse(0.0))
        case KFloat  => ZIO.succeed(v.asInstanceOf[Vector[Float]].maxOption.getOrElse(0.0f))
        case KLong   => ZIO.succeed(v.asInstanceOf[Vector[Long]].maxOption.getOrElse(0L))
        case KInt    => ZIO.succeed(v.asInstanceOf[Vector[Int]].maxOption.getOrElse(0))
    yield res


case class SumFn(recv: Fn[Any], posStr: String) extends UnaryFn[Any]:
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext) =
    for
      rawResult <- recv.resolve(ctx).either
      valueOpt  <- rawResult match
        case Right(v) => ZIO.succeed(Some(v))
        case Left(_: DynaLensError) if recv.isOptional => ZIO.succeed(None)
        case Left(e) => ZIO.fail(e)
      box <- valueOpt match
        case None    => ZIO.succeed(NumPromote.emptyBox)
        case Some(v) => ZIO.fromEither(collect(v, "sum()", posStr))
      v   = toPromotedVector(box)
      res <- box.kind match
        case KDouble => ZIO.succeed(v.asInstanceOf[Vector[Double]].sum)
        case KFloat  => ZIO.succeed(v.asInstanceOf[Vector[Float]].sum)
        case KLong   => ZIO.succeed(v.asInstanceOf[Vector[Long]].sum)
        case KInt    => ZIO.succeed(v.asInstanceOf[Vector[Int]].sum)
    yield res


case class AvgFn(recv: Fn[Any], posStr: String) extends UnaryFn[Any]:
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext) =
    for
      raw <- recv.resolve(ctx)
      box <- ZIO.fromEither(collect(raw, "avg()", posStr))
      v    = toPromotedVector(box)
      res <- box.kind match
        case KDouble =>
          val vs = v.asInstanceOf[Vector[Double]]
          ZIO.succeed(if vs.isEmpty then 0.0 else vs.sum / vs.size)
        case KFloat =>
          val vs = v.asInstanceOf[Vector[Float]]
          ZIO.succeed(if vs.isEmpty then 0.0 else vs.map(_.toDouble).sum / vs.size)
        case KLong =>
          val vs = v.asInstanceOf[Vector[Long]]
          ZIO.succeed(if vs.isEmpty then 0.0 else vs.map(_.toDouble).sum / vs.size)
        case KInt =>
          val vs = v.asInstanceOf[Vector[Int]]
          ZIO.succeed(if vs.isEmpty then 0.0 else vs.map(_.toDouble).sum / vs.size)
    yield res


case class MedianFn(recv: Fn[Any], posStr: String) extends UnaryFn[Any]:
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(recv = kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext) =
    for
      raw <- recv.resolve(ctx)
      box <- ZIO.fromEither(collect(raw, "median()", posStr))
      v    = toPromotedVector(box)
      res <- box.kind match
        case KDouble =>
          val s = v.asInstanceOf[Vector[Double]].sorted
          ZIO.succeed(computeMedian(s))
        case KFloat =>
          val s = v.asInstanceOf[Vector[Float]].sorted
          ZIO.succeed(computeMedian(s.map(_.toDouble)))
        case KLong =>
          val s = v.asInstanceOf[Vector[Long]].sorted
          ZIO.succeed(computeMedian(s.map(_.toDouble)))
        case KInt =>
          val s = v.asInstanceOf[Vector[Int]].sorted
          ZIO.succeed(computeMedian(s.map(_.toDouble)))
    yield res

  private def computeMedian(v: Vector[Double]): Double =
    val n = v.length
    if n == 0 then 0.0
    else if (n & 1) == 1 then v(n / 2)
    else (v(n / 2 - 1) + v(n / 2)) / 2.0