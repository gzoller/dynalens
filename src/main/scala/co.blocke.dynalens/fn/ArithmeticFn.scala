package co.blocke.dynalens
package fn

import zio.*
import scala.math.{BigInt, BigDecimal}


case class NegateFn(
                     recv: Fn[Any],
                     posStr: String
                   ) extends UnaryFn[Any]:
  override val methodName: String = "-"

  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(kids.head.asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    for {
      (v, lens) <- recv.resolve(ctx)
      result <- (v match {
        case i: Int => ZIO.succeed(-i)
        case l: Long => ZIO.succeed(-l)
        case f: Float => ZIO.succeed(-f)
        case d: Double => ZIO.succeed(-d)
        case bi: BigInt => ZIO.succeed(-bi)
        case bd: BigDecimal => ZIO.succeed(-bd)
        case _ =>
          ZIO.fail(
            DynaLensError(posStr, s"NegateFn does not support operand type: ${v.getClass.getSimpleName}")
          )
      })
    } yield (result, lens)


case class AddFn(
                  recv: Fn[Any],
                  args: List[Fn[Any]],
                  posStr: String
                ) extends BinaryFn[Any]:
  override val methodName: String = "+"

  def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      List(kids(1).asInstanceOf[Fn[Any]])
    )

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    for {
      (lv, lLens) <- recv.resolve(ctx)
      (rv, _) <- args.head.resolve(ctx)
      result <- (lv, rv) match {

        // ----- Int / ... -----
        case (a: Int, b: Int) => ZIO.succeed(a + b)
        case (a: Int, b: Long) => ZIO.succeed(a.toLong + b)
        case (a: Int, b: Float) => ZIO.succeed(a + b)
        case (a: Int, b: Double) => ZIO.succeed(a + b)
        case (a: Int, b: BigInt) => ZIO.succeed(BigInt(a) + b)
        case (a: Int, b: BigDecimal) => ZIO.succeed(BigDecimal(a) + b)

        // ----- Long / ... -----
        case (a: Long, b: Int) => ZIO.succeed(a + b)
        case (a: Long, b: Long) => ZIO.succeed(a + b)
        case (a: Long, b: Float) => ZIO.succeed(a + b)
        case (a: Long, b: Double) => ZIO.succeed(a + b)
        case (a: Long, b: BigInt) => ZIO.succeed(BigInt(a) + b)
        case (a: Long, b: BigDecimal) => ZIO.succeed(BigDecimal(a) + b)

        // ----- Float / ... -----
        case (a: Float, b: Int) => ZIO.succeed(a + b)
        case (a: Float, b: Long) => ZIO.succeed(a + b)
        case (a: Float, b: Float) => ZIO.succeed(a + b)
        case (a: Float, b: Double) => ZIO.succeed(a + b)

        // ----- Double / ... -----
        case (a: Double, b: Int) => ZIO.succeed(a + b)
        case (a: Double, b: Long) => ZIO.succeed(a + b)
        case (a: Double, b: Float) => ZIO.succeed(a + b)
        case (a: Double, b: Double) => ZIO.succeed(a + b)
        case (a: Double, b: BigDecimal) => ZIO.succeed(BigDecimal(a) + b)

        // ----- BigInt / BigDecimal -----
        case (a: BigInt, b: BigInt) => ZIO.succeed(a + b)
        case (a: BigInt, b: Int) => ZIO.succeed(a + BigInt(b))
        case (a: BigInt, b: BigDecimal) => ZIO.succeed(BigDecimal(a) + b)
        case (a: BigDecimal, b: BigDecimal) => ZIO.succeed(a + b)
        case (a: BigDecimal, b: BigInt) => ZIO.succeed(a + BigDecimal(b))
        case (a: BigDecimal, b: Double) => ZIO.succeed(a + BigDecimal(b))
        case (a: BigDecimal, b: Int) => ZIO.succeed(a + BigDecimal(b))

        // ----- Fallback -----
        case _ =>
          ZIO.fail(
            DynaLensError(posStr,
              s"AddFn does not support operand types: ${lv.getClass.getSimpleName}, ${rv.getClass.getSimpleName}"
            )
          )
      }
    } yield (result, lLens)


case class SubtractFn(
                       recv: Fn[Any],
                       args: List[Fn[Any]],
                       posStr: String
                     ) extends BinaryFn[Any]:
  override val methodName: String = "-"

  def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      List(kids(1).asInstanceOf[Fn[Any]])
    )

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    for {
      (lv, lLens) <- recv.resolve(ctx)
      (rv, _) <- args.head.resolve(ctx)
      result <- (lv, rv) match {

        // ----- Int / ... -----
        case (a: Int, b: Int) => ZIO.succeed(a - b)
        case (a: Int, b: Long) => ZIO.succeed(a.toLong - b)
        case (a: Int, b: Float) => ZIO.succeed(a - b)
        case (a: Int, b: Double) => ZIO.succeed(a - b)
        case (a: Int, b: BigInt) => ZIO.succeed(BigInt(a) - b)
        case (a: Int, b: BigDecimal) => ZIO.succeed(BigDecimal(a) - b)

        // ----- Long / ... -----
        case (a: Long, b: Int) => ZIO.succeed(a - b)
        case (a: Long, b: Long) => ZIO.succeed(a - b)
        case (a: Long, b: Float) => ZIO.succeed(a - b)
        case (a: Long, b: Double) => ZIO.succeed(a - b)
        case (a: Long, b: BigInt) => ZIO.succeed(BigInt(a) - b)
        case (a: Long, b: BigDecimal) => ZIO.succeed(BigDecimal(a) - b)

        // ----- Float / ... -----
        case (a: Float, b: Int) => ZIO.succeed(a - b)
        case (a: Float, b: Long) => ZIO.succeed(a - b)
        case (a: Float, b: Float) => ZIO.succeed(a - b)
        case (a: Float, b: Double) => ZIO.succeed(a - b)

        // ----- Double / ... -----
        case (a: Double, b: Int) => ZIO.succeed(a - b)
        case (a: Double, b: Long) => ZIO.succeed(a - b)
        case (a: Double, b: Float) => ZIO.succeed(a - b)
        case (a: Double, b: Double) => ZIO.succeed(a - b)
        case (a: Double, b: BigDecimal) => ZIO.succeed(BigDecimal(a) - b)

        // ----- BigInt / BigDecimal -----
        case (a: BigInt, b: BigInt) => ZIO.succeed(a - b)
        case (a: BigInt, b: Int) => ZIO.succeed(a - BigInt(b))
        case (a: BigInt, b: Long) => ZIO.succeed(a - BigInt(b))
        case (a: BigInt, b: BigDecimal) => ZIO.succeed(BigDecimal(a) - b)
        case (a: BigDecimal, b: BigDecimal) => ZIO.succeed(a - b)
        case (a: BigDecimal, b: BigInt) => ZIO.succeed(a - BigDecimal(b))
        case (a: BigDecimal, b: Double) => ZIO.succeed(a - BigDecimal(b))
        case (a: BigDecimal, b: Int) => ZIO.succeed(a - BigDecimal(b))
        case (a: BigDecimal, b: Long) => ZIO.succeed(a - BigDecimal(b))

        // ----- Fallback -----
        case _ =>
          ZIO.fail(
            DynaLensError(posStr,
              s"SubtractFn does not support operand types: ${lv.getClass.getSimpleName}, ${rv.getClass.getSimpleName}"
            )
          )
      }
    } yield (result, lLens)


case class MultiplyFn(
                       recv: Fn[Any],
                       args: List[Fn[Any]],
                       posStr: String
                     ) extends BinaryFn[Any]:
  override val methodName: String = "*"

  def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      List(kids(1).asInstanceOf[Fn[Any]])
    )

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    for {
      (lv, lLens) <- recv.resolve(ctx)
      (rv, _) <- args.head.resolve(ctx)
      result <- (lv, rv) match {
        // --- Int cases
        case (a: Int, b: Int)       => ZIO.succeed(a * b)
        case (a: Int, b: Long)      => ZIO.succeed(a * b)
        case (a: Int, b: Float)     => ZIO.succeed(a * b)
        case (a: Int, b: Double)    => ZIO.succeed(a * b)

        // --- Long cases
        case (a: Long, b: Int)      => ZIO.succeed(a * b)
        case (a: Long, b: Long)     => ZIO.succeed(a * b)
        case (a: Long, b: Float)    => ZIO.succeed(a * b)
        case (a: Long, b: Double)   => ZIO.succeed(a * b)
        case (a: Long, b: BigDecimal) => ZIO.succeed(BigDecimal(a) * b)

        // --- Float cases
        case (a: Float, b: Int)     => ZIO.succeed(a * b)
        case (a: Float, b: Long)    => ZIO.succeed(a * b)
        case (a: Float, b: Float)   => ZIO.succeed(a * b)
        case (a: Float, b: Double)  => ZIO.succeed(a * b)

        // --- Double cases
        case (a: Double, b: Int)    => ZIO.succeed(a * b)
        case (a: Double, b: Long)   => ZIO.succeed(a * b)
        case (a: Double, b: Float)  => ZIO.succeed(a * b)
        case (a: Double, b: Double) => ZIO.succeed(a * b)

        // --- Big types (optional)
        case (a: BigInt, b: BigInt) => ZIO.succeed(a * b)
        case (a: BigDecimal, b: BigDecimal) => ZIO.succeed(a * b)
        case (a: BigInt, b: Int) => ZIO.succeed(a * BigInt(b))
        case (a: Int, b: BigInt) => ZIO.succeed(BigInt(a) * b)
        case (a: BigDecimal, b: Double) => ZIO.succeed(a * BigDecimal(b))
        case (a: BigDecimal, b: Long) => ZIO.succeed(a * BigDecimal(b))
        case (a: Double, b: BigDecimal) => ZIO.succeed(BigDecimal(a) * b)

        case _ =>
          ZIO.fail(
            DynaLensError(posStr,
              s"MultiplyFn does not support operands of types: ${lv.getClass.getSimpleName}, ${rv.getClass.getSimpleName}"
            )
          )
      }
    } yield (result, lLens)


case class DivideFn(
                     recv: Fn[Any],
                     args: List[Fn[Any]],
                     posStr: String
                   ) extends BinaryFn[Any]:
  override val methodName: String = "/"

  def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      List(kids(1).asInstanceOf[Fn[Any]])
    )

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    for {
      (lv, lLens) <- recv.resolve(ctx)
      (rv, _) <- args.head.resolve(ctx)
      result <- (lv, rv) match {
        // --- Guard divide by zero for numeric types ---
        case (_, 0 | 0L | 0f | 0d) =>
          ZIO.fail(DynaLensError(posStr,"Division by zero"))

        // --- Int / ... (always widen to Double for safety)
        case (a: Int, b: Int)       => ZIO.succeed(a.toDouble / b)
        case (a: Int, b: Long)      => ZIO.succeed(a.toDouble / b.toDouble)
        case (a: Int, b: Float)     => ZIO.succeed(a / b)
        case (a: Int, b: Double)    => ZIO.succeed(a / b)

        // --- Long / ...
        case (a: Long, b: Int)      => ZIO.succeed(a.toDouble / b)
        case (a: Long, b: Long)     => ZIO.succeed(a.toDouble / b.toDouble)
        case (a: Long, b: Float)    => ZIO.succeed(a / b)
        case (a: Long, b: Double)   => ZIO.succeed(a / b)

        // --- Float / ...
        case (a: Float, b: Int)     => ZIO.succeed(a / b)
        case (a: Float, b: Long)    => ZIO.succeed(a / b)
        case (a: Float, b: Float)   => ZIO.succeed(a / b)
        case (a: Float, b: Double)  => ZIO.succeed(a / b)

        // --- Double / ...
        case (a: Double, b: Int)    => ZIO.succeed(a / b)
        case (a: Double, b: Long)   => ZIO.succeed(a / b)
        case (a: Double, b: Float)  => ZIO.succeed(a / b)
        case (a: Double, b: Double) => ZIO.succeed(a / b)

        // --- BigInt / BigDecimal ---
        case (a: BigInt, b: BigInt) =>
          ZIO.succeed(BigDecimal(a) / BigDecimal(b))
        case (a: BigDecimal, b: BigDecimal) =>
          if b == BigDecimal(0) then
            ZIO.fail(DynaLensError(posStr, "Division by zero"))
          else
            ZIO.succeed(a / b)
        case (a: BigInt, b: Int) =>
          ZIO.succeed(BigDecimal(a) / BigDecimal(b))
        case (a: Int, b: BigInt) =>
          ZIO.succeed(BigDecimal(a) / BigDecimal(b))
        case (a: BigDecimal, b: Double) =>
          ZIO.succeed(a / BigDecimal(b))
        case (a: Double, b: BigDecimal) =>
          ZIO.succeed(BigDecimal(a) / b)
        case (a: BigDecimal, b: Long) =>
          ZIO.succeed(a / BigDecimal(b))
        case (a: Long, b: BigDecimal) =>
          ZIO.succeed(BigDecimal(a) / b)

        case _ =>
          ZIO.fail(
            DynaLensError(posStr,
              s"DivideFn does not support operand types: ${lv.getClass.getSimpleName}, ${rv.getClass.getSimpleName}"
            )
          )
      }
    } yield (result, lLens)


case class ModuloFn(
                     recv: Fn[Any],
                     args: List[Fn[Any]],
                     posStr: String
                   ) extends BinaryFn[Any]:
  override val methodName: String = "%"

  def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      List(kids(1).asInstanceOf[Fn[Any]])
    )

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, (Any, Lens)] =
    for {
      (lv, lLens) <- recv.resolve(ctx)
      (rv, _) <- args.head.resolve(ctx)
      result <- (lv, rv) match {
        // --- Prevent divide/mod by zero ---
        case (_, 0 | 0L | 0f | 0d) =>
          ZIO.fail(DynaLensError(posStr, s"Modulus by zero"))
        case (_, bzi: BigInt) if bzi == 0 =>
          ZIO.fail(DynaLensError(posStr, s"Modulus by zero"))
        case (_, bzd: BigDecimal) if bzd == 0 =>
          ZIO.fail(DynaLensError(posStr, s"Modulus by zero"))

        // --- Int / ... (always widen to Double for safety)
        case (a: Int, b: Int)       => ZIO.succeed(a.toDouble % b)
        case (a: Int, b: Long)      => ZIO.succeed(a.toDouble % b.toDouble)
        case (a: Int, b: Float)     => ZIO.succeed(a % b)
        case (a: Int, b: Double)    => ZIO.succeed(a % b)

        // --- Long / ...
        case (a: Long, b: Int)      => ZIO.succeed(a.toDouble % b)
        case (a: Long, b: Long)     => ZIO.succeed(a.toDouble % b.toDouble)
        case (a: Long, b: Float)    => ZIO.succeed(a % b)
        case (a: Long, b: Double)   => ZIO.succeed(a % b)

        // --- Float / ...
        case (a: Float, b: Int)     => ZIO.succeed(a % b)
        case (a: Float, b: Long)    => ZIO.succeed(a % b)
        case (a: Float, b: Float)   => ZIO.succeed(a % b)
        case (a: Float, b: Double)  => ZIO.succeed(a % b)

        // --- Double / ...
        case (a: Double, b: Int)    => ZIO.succeed(a % b)
        case (a: Double, b: Long)   => ZIO.succeed(a % b)
        case (a: Double, b: Float)  => ZIO.succeed(a % b)
        case (a: Double, b: Double) => ZIO.succeed(a % b)

        // --- BigInt / BigDecimal ---
        case (a: BigInt, b: BigInt) =>
          ZIO.succeed(a % b)
        case (a: BigDecimal, b: BigDecimal) =>
          ZIO.succeed(a.remainder(b))
        case (a: BigDecimal, b: Long) =>
          ZIO.succeed(a.remainder(BigDecimal(b)))
        case (a: Long, b: BigDecimal) =>
          ZIO.succeed(BigDecimal(a).remainder(b))
        case (a: BigDecimal, b: Int) =>
          ZIO.succeed(a.remainder(BigDecimal(b)))
        case (a: Int, b: BigDecimal) =>
          ZIO.succeed(BigDecimal(a).remainder(b))
        case (a: BigInt, b: Int) =>
          ZIO.succeed(a % BigInt(b))
        case (a: Int, b: BigInt) =>
          ZIO.succeed(BigInt(a) % b)

        // --- Fallback ---
        case _ =>
          ZIO.fail(
            DynaLensError(posStr,
              s"ModuloFn does not support operand types: ${lv.getClass.getSimpleName}, ${rv.getClass.getSimpleName}"
            )
          )
      }
    } yield (result, lLens)