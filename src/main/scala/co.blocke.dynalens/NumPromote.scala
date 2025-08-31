package co.blocke.dynalens

object NumPromote {
  sealed trait Kind
  case object KInt    extends Kind  // Byte/Short/Int
  case object KLong   extends Kind
  case object KFloat  extends Kind
  case object KDouble extends Kind

  private def kindOf(x: Any): Option[Kind] = x match {
    case _: java.lang.Byte   | _: java.lang.Short | _: java.lang.Integer => Some(KInt)
    case _: java.lang.Long                                             => Some(KLong)
    case _: java.lang.Float                                            => Some(KFloat)
    case _: java.lang.Double                                           => Some(KDouble)
    // If you want to allow BigInt/BigDecimal/String, handle here (or reject).
    case _ => None
  }

  final case class Box(ints: Vector[Int] = Vector.empty,
                       longs: Vector[Long] = Vector.empty,
                       doubles: Vector[Double] = Vector.empty,
                       kind: Kind = KInt)

  /** Collect an Iterable[Any] into typed buckets + overall promoted kind. */
  def collect(raw: Any, op: String): Either[DynaLensError, Box] = raw match {
    case null                     => Right(Box()) // treat null/None as empty
    case o: Option[?]             => o match {
      case None                   => Right(Box())
      case Some(it: Iterable[?])  => collect(it, op)
      case Some(x)                => Left(DynaLensError(s"$op() expects a list, got ${x.getClass.getSimpleName} in Some(...)"))
    }
    case it: Iterable[?] =>
      var hasDouble = false
      var hasFloat  = false
      var hasLong   = false

      val ints   = Vector.newBuilder[Int]
      val longs  = Vector.newBuilder[Long]
      val dbls   = Vector.newBuilder[Double]

      var idx = 0
      val iter = it.iterator
      while (iter.hasNext) {
        val v = iter.next()
        kindOf(v) match {
          case None =>
            return Left(DynaLensError(s"$op() expects numeric elements; got ${v.getClass.getSimpleName} at index $idx"))
          case Some(KDouble) =>
            hasDouble = true; dbls += v.asInstanceOf[Double]
          case Some(KFloat)  =>
            hasFloat  = true; dbls += v.asInstanceOf[Float].toDouble
          case Some(KLong)   =>
            hasLong   = true; longs += v.asInstanceOf[Long]
          case Some(KInt)    =>
            ints += (v match {
              case b: java.lang.Byte   => b.toInt
              case s: java.lang.Short  => s.toInt
              case i: java.lang.Integer=> i.intValue
            })
        }
        idx += 1
      }

      val promoted =
        if (hasDouble || hasFloat) KDouble
        else if (hasLong)          KLong
        else                       KInt

      Right(Box(ints.result(), longs.result(), dbls.result(), promoted))

    case other =>
      Left(DynaLensError(s"$op() may only be applied to Iterable types, got ${other.getClass.getSimpleName}"))
  }

  /** Convert the Box to a single homogenous Vector in the promoted kind. */
  def toPromotedVector(b: Box): Vector[AnyVal] = b.kind match {
    case KDouble =>
      // push everything to Double
      b.doubles ++ b.longs.map(_.toDouble) ++ b.ints.map(_.toDouble)
    case KLong =>
      // push ints to Long, keep longs
      b.longs ++ b.ints.map(_.toLong)
    case KInt =>
      // keep as Int
      b.ints
  }
}