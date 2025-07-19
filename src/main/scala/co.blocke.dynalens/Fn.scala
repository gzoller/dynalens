package co.blocke.dynalens

import zio.*
import Path.*

trait Fn[R]:
  def resolve(ctx: Map[String,(Any,DynaLens[?])]): ZIO[_BiMapRegistry,DynaLensError,R]

// Constant value
case class ConstantFn[R](out: R) extends Fn[R]:
  def resolve(
               ctx: Map[String, (Any, DynaLens[?])] = Map.empty
             ): ZIO[_BiMapRegistry, DynaLensError, R] =
    ZIO.succeed(out)

case class GetFn(path: String) extends Fn[Any]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Any] =
    parsePath(path) match {
      case Nil => ZIO.fail(DynaLensError("get requires a path"))
      case pathHead :: rest if ctx.contains(pathHead.name) =>
        ctx.get(pathHead.name) match {
          case Some((obj, null)) => // val de-reference
            ZIO.succeed(obj)
          case Some((obj, dynalens)) => // iterable de-reference
            dynalens.get(partialPath(rest), obj.asInstanceOf[dynalens.ThisT])
          case None =>
            ZIO.fail(DynaLensError(s"Field $path not found in context"))
        }
      case _ =>
        ctx.get("top") match {
          case Some((obj, dynalens)) =>
            dynalens.get(path, obj.asInstanceOf[dynalens.ThisT])
          case None =>
            ZIO.fail(DynaLensError(s"Field $path not found"))
        }
    }

case class IfFn[R](
                    condition: Fn[Boolean],
                    thenFn: Fn[R],
                    elseFn: Fn[R]
                  ) extends Fn[R]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, R] =
    condition.resolve(ctx).flatMap {
      case true => thenFn.resolve(ctx)
      case false => elseFn.resolve(ctx)
    }

case class BlockFn[R](
                       statements: List[Statement],
                       finalFn: Fn[R]
                     ) extends Fn[R]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, R] =
    val prepped = statements.foldLeft(
      ZIO.succeed(ctx): ZIO[_BiMapRegistry, DynaLensError, Map[String, (Any, DynaLens[?])]]
    ) { (acc, stmt) => acc.flatMap(stmt.resolve) }
    prepped.flatMap(finalFn.resolve)

//
//===============================
//  Comparator Functions
//===============================
//

case class GreaterThanFn(left: Fn[Any], right: Fn[Any]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match
        case (l: Int, r: Int) => ZIO.succeed(l > r)
        case (l: Int, r: Long) => ZIO.succeed(l.asInstanceOf[Long] > r.asInstanceOf[Long])
        case (l: Int, r: Float) => ZIO.succeed(l.asInstanceOf[Float] > r.asInstanceOf[Float])
        case (l: Int, r: Double) => ZIO.succeed(l.asInstanceOf[Double] > r.asInstanceOf[Double])
        case (l: Long, r: Int) => ZIO.succeed(l.asInstanceOf[Long] > r.asInstanceOf[Long])
        case (l: Long, r: Long) => ZIO.succeed(l > r)
        case (l: Long, r: Float) => ZIO.succeed(l.asInstanceOf[Float] > r.asInstanceOf[Float])
        case (l: Long, r: Double) => ZIO.succeed(l.asInstanceOf[Double] > r.asInstanceOf[Double])
        case (l: Float, r: Int) => ZIO.succeed(l.asInstanceOf[Float] > r.asInstanceOf[Float])
        case (l: Float, r: Long) => ZIO.succeed(l.asInstanceOf[Float] > r.asInstanceOf[Float])
        case (l: Float, r: Float) => ZIO.succeed(l > r)
        case (l: Float, r: Double) => ZIO.succeed(l.asInstanceOf[Double] > r.asInstanceOf[Double])
        case (l: Double, r: Int) => ZIO.succeed(l.asInstanceOf[Double] > r.asInstanceOf[Double])
        case (l: Double, r: Long) => ZIO.succeed(l.asInstanceOf[Double] > r.asInstanceOf[Double])
        case (l: Double, r: Float) => ZIO.succeed(l.asInstanceOf[Double] > r.asInstanceOf[Double])
        case (l: Double, r: Double) => ZIO.succeed(l > r)
        case (ls: String, rs: String) => ZIO.fail(DynaLensError("Cannot perform ordering on strings"))
        case (lVal, rVal) => ZIO.fail(DynaLensError(s"Cannot compare types: ${lVal.getClass} and ${rVal.getClass}"))
    } yield result

case class GreaterThanOrEqualFn(left: Fn[Any], right: Fn[Any]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match
        case (l: Int, r: Int) => ZIO.succeed(l >= r)
        case (l: Int, r: Long) => ZIO.succeed(l.asInstanceOf[Long] >= r.asInstanceOf[Long])
        case (l: Int, r: Float) => ZIO.succeed(l.asInstanceOf[Float] >= r.asInstanceOf[Float])
        case (l: Int, r: Double) => ZIO.succeed(l.asInstanceOf[Double] >= r.asInstanceOf[Double])
        case (l: Long, r: Int) => ZIO.succeed(l.asInstanceOf[Long] >= r.asInstanceOf[Long])
        case (l: Long, r: Long) => ZIO.succeed(l >= r)
        case (l: Long, r: Float) => ZIO.succeed(l.asInstanceOf[Float] >= r.asInstanceOf[Float])
        case (l: Long, r: Double) => ZIO.succeed(l.asInstanceOf[Double] >= r.asInstanceOf[Double])
        case (l: Float, r: Int) => ZIO.succeed(l.asInstanceOf[Float] >= r.asInstanceOf[Float])
        case (l: Float, r: Long) => ZIO.succeed(l.asInstanceOf[Float] >= r.asInstanceOf[Float])
        case (l: Float, r: Float) => ZIO.succeed(l >= r)
        case (l: Float, r: Double) => ZIO.succeed(l.asInstanceOf[Double] >= r.asInstanceOf[Double])
        case (l: Double, r: Int) => ZIO.succeed(l.asInstanceOf[Double] >= r.asInstanceOf[Double])
        case (l: Double, r: Long) => ZIO.succeed(l.asInstanceOf[Double] >= r.asInstanceOf[Double])
        case (l: Double, r: Float) => ZIO.succeed(l.asInstanceOf[Double] >= r.asInstanceOf[Double])
        case (l: Double, r: Double) => ZIO.succeed(l >= r)
        case (ls: String, rs: String) => ZIO.fail(DynaLensError("Cannot perform ordering on strings"))
        case (lVal, rVal) => ZIO.fail(DynaLensError(s"Cannot compare types: ${lVal.getClass} and ${rVal.getClass}"))
    } yield result

case class LessThanFn(left: Fn[Any], right: Fn[Any]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match
        case (l: Int, r: Int) => ZIO.succeed(l < r)
        case (l: Int, r: Long) => ZIO.succeed(l.asInstanceOf[Long] < r.asInstanceOf[Long])
        case (l: Int, r: Float) => ZIO.succeed(l.asInstanceOf[Float] < r.asInstanceOf[Float])
        case (l: Int, r: Double) => ZIO.succeed(l.asInstanceOf[Double] < r.asInstanceOf[Double])
        case (l: Long, r: Int) => ZIO.succeed(l.asInstanceOf[Long] < r.asInstanceOf[Long])
        case (l: Long, r: Long) => ZIO.succeed(l < r)
        case (l: Long, r: Float) => ZIO.succeed(l.asInstanceOf[Float] < r.asInstanceOf[Float])
        case (l: Long, r: Double) => ZIO.succeed(l.asInstanceOf[Double] < r.asInstanceOf[Double])
        case (l: Float, r: Int) => ZIO.succeed(l.asInstanceOf[Float] < r.asInstanceOf[Float])
        case (l: Float, r: Long) => ZIO.succeed(l.asInstanceOf[Float] < r.asInstanceOf[Float])
        case (l: Float, r: Float) => ZIO.succeed(l < r)
        case (l: Float, r: Double) => ZIO.succeed(l.asInstanceOf[Double] < r.asInstanceOf[Double])
        case (l: Double, r: Int) => ZIO.succeed(l.asInstanceOf[Double] < r.asInstanceOf[Double])
        case (l: Double, r: Long) => ZIO.succeed(l.asInstanceOf[Double] < r.asInstanceOf[Double])
        case (l: Double, r: Float) => ZIO.succeed(l.asInstanceOf[Double] < r.asInstanceOf[Double])
        case (l: Double, r: Double) => ZIO.succeed(l < r)
        case (ls: String, rs: String) => ZIO.fail(DynaLensError("Cannot perform ordering on strings"))
        case (lVal, rVal) => ZIO.fail(DynaLensError(s"Cannot compare types: ${lVal.getClass} and ${rVal.getClass}"))
    } yield result

case class LessThanOrEqualFn(left: Fn[Any], right: Fn[Any]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match
        case (l: Int, r: Int) => ZIO.succeed(l <= r)
        case (l: Int, r: Long) => ZIO.succeed(l.asInstanceOf[Long] <= r.asInstanceOf[Long])
        case (l: Int, r: Float) => ZIO.succeed(l.asInstanceOf[Float] <= r.asInstanceOf[Float])
        case (l: Int, r: Double) => ZIO.succeed(l.asInstanceOf[Double] <= r.asInstanceOf[Double])
        case (l: Long, r: Int) => ZIO.succeed(l.asInstanceOf[Long] <= r.asInstanceOf[Long])
        case (l: Long, r: Long) => ZIO.succeed(l <= r)
        case (l: Long, r: Float) => ZIO.succeed(l.asInstanceOf[Float] <= r.asInstanceOf[Float])
        case (l: Long, r: Double) => ZIO.succeed(l.asInstanceOf[Double] <= r.asInstanceOf[Double])
        case (l: Float, r: Int) => ZIO.succeed(l.asInstanceOf[Float] <= r.asInstanceOf[Float])
        case (l: Float, r: Long) => ZIO.succeed(l.asInstanceOf[Float] <= r.asInstanceOf[Float])
        case (l: Float, r: Float) => ZIO.succeed(l <= r)
        case (l: Float, r: Double) => ZIO.succeed(l.asInstanceOf[Double] <= r.asInstanceOf[Double])
        case (l: Double, r: Int) => ZIO.succeed(l.asInstanceOf[Double] <= r.asInstanceOf[Double])
        case (l: Double, r: Long) => ZIO.succeed(l.asInstanceOf[Double] <= r.asInstanceOf[Double])
        case (l: Double, r: Float) => ZIO.succeed(l.asInstanceOf[Double] <= r.asInstanceOf[Double])
        case (l: Double, r: Double) => ZIO.succeed(l <= r)
        case (ls: String, rs: String) => ZIO.fail(DynaLensError("Cannot perform ordering on strings"))
        case (lVal, rVal) => ZIO.fail(DynaLensError(s"Cannot compare types: ${lVal.getClass} and ${rVal.getClass}"))
    } yield result

case class EqualFn(left: Fn[Any], right: Fn[Any]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match
        case (l: Int, r: Int) => ZIO.succeed(l == r)
        case (l: Int, r: Long) => ZIO.succeed(l.asInstanceOf[Long] == r.asInstanceOf[Long])
        case (l: Int, r: Float) => ZIO.succeed(l.asInstanceOf[Float] == r.asInstanceOf[Float])
        case (l: Int, r: Double) => ZIO.succeed(l.asInstanceOf[Double] == r.asInstanceOf[Double])
        case (l: Long, r: Int) => ZIO.succeed(l.asInstanceOf[Long] == r.asInstanceOf[Long])
        case (l: Long, r: Long) => ZIO.succeed(l == r)
        case (l: Long, r: Float) => ZIO.succeed(l.asInstanceOf[Float] == r.asInstanceOf[Float])
        case (l: Long, r: Double) => ZIO.succeed(l.asInstanceOf[Double] == r.asInstanceOf[Double])
        case (l: Float, r: Int) => ZIO.succeed(l.asInstanceOf[Float] == r.asInstanceOf[Float])
        case (l: Float, r: Long) => ZIO.succeed(l.asInstanceOf[Float] == r.asInstanceOf[Float])
        case (l: Float, r: Float) => ZIO.succeed(l == r)
        case (l: Float, r: Double) => ZIO.succeed(l.asInstanceOf[Double] == r.asInstanceOf[Double])
        case (l: Double, r: Int) => ZIO.succeed(l.asInstanceOf[Double] == r.asInstanceOf[Double])
        case (l: Double, r: Long) => ZIO.succeed(l.asInstanceOf[Double] == r.asInstanceOf[Double])
        case (l: Double, r: Float) => ZIO.succeed(l.asInstanceOf[Double] == r.asInstanceOf[Double])
        case (l: Double, r: Double) => ZIO.succeed(l == r)
        case (ls: String, rs: String) => ZIO.succeed(ls == rs)
        case (lVal, rVal) => ZIO.fail(DynaLensError(s"Cannot compare types: ${lVal.getClass} and ${rVal.getClass}"))
    } yield result

case class NotEqualFn(left: Fn[Any], right: Fn[Any]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match
        case (l: Int, r: Int) => ZIO.succeed(l != r)
        case (l: Int, r: Long) => ZIO.succeed(l.asInstanceOf[Long] != r.asInstanceOf[Long])
        case (l: Int, r: Float) => ZIO.succeed(l.asInstanceOf[Float] != r.asInstanceOf[Float])
        case (l: Int, r: Double) => ZIO.succeed(l.asInstanceOf[Double] != r.asInstanceOf[Double])
        case (l: Long, r: Int) => ZIO.succeed(l.asInstanceOf[Long] != r.asInstanceOf[Long])
        case (l: Long, r: Long) => ZIO.succeed(l != r)
        case (l: Long, r: Float) => ZIO.succeed(l.asInstanceOf[Float] != r.asInstanceOf[Float])
        case (l: Long, r: Double) => ZIO.succeed(l.asInstanceOf[Double] != r.asInstanceOf[Double])
        case (l: Float, r: Int) => ZIO.succeed(l.asInstanceOf[Float] != r.asInstanceOf[Float])
        case (l: Float, r: Long) => ZIO.succeed(l.asInstanceOf[Float] != r.asInstanceOf[Float])
        case (l: Float, r: Float) => ZIO.succeed(l != r)
        case (l: Float, r: Double) => ZIO.succeed(l.asInstanceOf[Double] != r.asInstanceOf[Double])
        case (l: Double, r: Int) => ZIO.succeed(l.asInstanceOf[Double] != r.asInstanceOf[Double])
        case (l: Double, r: Long) => ZIO.succeed(l.asInstanceOf[Double] != r.asInstanceOf[Double])
        case (l: Double, r: Float) => ZIO.succeed(l.asInstanceOf[Double] != r.asInstanceOf[Double])
        case (l: Double, r: Double) => ZIO.succeed(l != r)
        case (ls: String, rs: String) => ZIO.succeed(ls != rs)
        case (lVal, rVal) => ZIO.fail(DynaLensError(s"Cannot compare types: ${lVal.getClass} and ${rVal.getClass}"))
    } yield result

case class NotFn(inner: Fn[Boolean]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    inner.resolve(ctx).map(b => !b)

case class AndFn(left: Fn[Boolean], right: Fn[Boolean]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
    } yield l && r

case class OrFn(left: Fn[Boolean], right: Fn[Boolean]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
    } yield l || r


case class StartsWithFn(left: Fn[String], right: Fn[String]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
    } yield l.startsWith(r)

case class EndsWithFn(left: Fn[String], right: Fn[String]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
    } yield l.endsWith(r)

case class ContainsFn(left: Fn[String], right: Fn[String]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
    } yield l.contains(r)

case class EqualsIgnoreCaseFn(left: Fn[String], right: Fn[String]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
    } yield l.equalsIgnoreCase(r)

case class MatchesRegexFn(left: Fn[String], regex: Fn[String]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- regex.resolve(ctx)
    } yield l.matches(r)

//
//===============================
//  Math Functions
//===============================
//

case class AddFn(
                  left: Fn[Any],
                  right: Fn[Any]
                ) extends Fn[Any]:

  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match {
        // Int cases
        case (a: Int, b: Int)       => ZIO.succeed(a + b)
        case (a: Int, b: Long)      => ZIO.succeed(a + b)
        case (a: Int, b: Float)     => ZIO.succeed(a + b)
        case (a: Int, b: Double)    => ZIO.succeed(a + b)

        // Long cases
        case (a: Long, b: Int)      => ZIO.succeed(a + b)
        case (a: Long, b: Long)     => ZIO.succeed(a + b)
        case (a: Long, b: Float)    => ZIO.succeed(a + b)
        case (a: Long, b: Double)   => ZIO.succeed(a + b)

        // Float cases
        case (a: Float, b: Int)     => ZIO.succeed(a + b)
        case (a: Float, b: Long)    => ZIO.succeed(a + b)
        case (a: Float, b: Float)   => ZIO.succeed(a + b)
        case (a: Float, b: Double)  => ZIO.succeed(a + b)

        // Double cases
        case (a: Double, b: Int)    => ZIO.succeed(a + b)
        case (a: Double, b: Long)   => ZIO.succeed(a + b)
        case (a: Double, b: Float)  => ZIO.succeed(a + b)
        case (a: Double, b: Double) => ZIO.succeed(a + b)

        // Unsupported types
        case _ =>
          ZIO.fail(DynaLensError(s"AddFn does not support operands of types: ${l.getClass}, ${r.getClass}"))
      }
    } yield result

case class SubtractFn(
                       left: Fn[Any],
                       right: Fn[Any]
                     ) extends Fn[Any]:

  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match {
        case (a: Int, b: Int) => ZIO.succeed(a - b)
        case (a: Int, b: Long) => ZIO.succeed(a - b)
        case (a: Int, b: Float) => ZIO.succeed(a - b)
        case (a: Int, b: Double) => ZIO.succeed(a - b)

        case (a: Long, b: Int) => ZIO.succeed(a - b)
        case (a: Long, b: Long) => ZIO.succeed(a - b)
        case (a: Long, b: Float) => ZIO.succeed(a - b)
        case (a: Long, b: Double) => ZIO.succeed(a - b)

        case (a: Float, b: Int) => ZIO.succeed(a - b)
        case (a: Float, b: Long) => ZIO.succeed(a - b)
        case (a: Float, b: Float) => ZIO.succeed(a - b)
        case (a: Float, b: Double) => ZIO.succeed(a - b)

        case (a: Double, b: Int) => ZIO.succeed(a - b)
        case (a: Double, b: Long) => ZIO.succeed(a - b)
        case (a: Double, b: Float) => ZIO.succeed(a - b)
        case (a: Double, b: Double) => ZIO.succeed(a - b)

        case _ => ZIO.fail(DynaLensError(s"SubtractFn does not support operands of types: ${l.getClass}, ${r.getClass}"))
      }
    } yield result

case class MultiplyFn(
                       left: Fn[Any],
                       right: Fn[Any]
                     ) extends Fn[Any]:

  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match {
        case (a: Int, b: Int) => ZIO.succeed(a * b)
        case (a: Int, b: Long) => ZIO.succeed(a * b)
        case (a: Int, b: Float) => ZIO.succeed(a * b)
        case (a: Int, b: Double) => ZIO.succeed(a * b)

        case (a: Long, b: Int) => ZIO.succeed(a * b)
        case (a: Long, b: Long) => ZIO.succeed(a * b)
        case (a: Long, b: Float) => ZIO.succeed(a * b)
        case (a: Long, b: Double) => ZIO.succeed(a * b)

        case (a: Float, b: Int) => ZIO.succeed(a * b)
        case (a: Float, b: Long) => ZIO.succeed(a * b)
        case (a: Float, b: Float) => ZIO.succeed(a * b)
        case (a: Float, b: Double) => ZIO.succeed(a * b)

        case (a: Double, b: Int) => ZIO.succeed(a * b)
        case (a: Double, b: Long) => ZIO.succeed(a * b)
        case (a: Double, b: Float) => ZIO.succeed(a * b)
        case (a: Double, b: Double) => ZIO.succeed(a * b)

        case _ => ZIO.fail(DynaLensError(s"MultiplyFn does not support operands of types: ${l.getClass}, ${r.getClass}"))
      }
    } yield result

case class DivideFn(
                     left: Fn[Any],
                     right: Fn[Any]
                   ) extends Fn[Any]:

  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match {
        case (_, 0 | 0L | 0.0f | 0.0d) => ZIO.fail(DynaLensError("Divide by zero"))

        case (a: Int, b: Int) => ZIO.succeed(a / b)
        case (a: Int, b: Long) => ZIO.succeed(a / b)
        case (a: Int, b: Float) => ZIO.succeed(a / b)
        case (a: Int, b: Double) => ZIO.succeed(a / b)

        case (a: Long, b: Int) => ZIO.succeed(a / b)
        case (a: Long, b: Long) => ZIO.succeed(a / b)
        case (a: Long, b: Float) => ZIO.succeed(a / b)
        case (a: Long, b: Double) => ZIO.succeed(a / b)

        case (a: Float, b: Int) => ZIO.succeed(a / b)
        case (a: Float, b: Long) => ZIO.succeed(a / b)
        case (a: Float, b: Float) => ZIO.succeed(a / b)
        case (a: Float, b: Double) => ZIO.succeed(a / b)

        case (a: Double, b: Int) => ZIO.succeed(a / b)
        case (a: Double, b: Long) => ZIO.succeed(a / b)
        case (a: Double, b: Float) => ZIO.succeed(a / b)
        case (a: Double, b: Double) => ZIO.succeed(a / b)

        case _ => ZIO.fail(DynaLensError(s"DivideFn does not support operands of types: ${l.getClass}, ${r.getClass}"))
      }
    } yield result

//
//===============================
//  String Builder Functions
//===============================
//

case class TrimFn(in: Fn[Any]) extends Fn[String]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, String] =
    in.resolve(ctx).map {
      case null => ""
      case s => s.toString.trim
    }

case class ToLowerFn(in: Fn[Any]) extends Fn[String]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, String] =
    in.resolve(ctx).map {
      case null => ""
      case s => s.toString.toLowerCase
    }

case class ToUpperFn(in: Fn[Any]) extends Fn[String]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, String] =
    in.resolve(ctx).map {
      case null => ""
      case s => s.toString.toUpperCase
    }

case class ConcatFn(parts: List[Fn[Any]]) extends Fn[String]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, String] =
    ZIO.foreach(parts)(_.resolve(ctx)).map(_.map {
      case null => ""
      case s => s.toString
    }.mkString)

case class LengthFn(in: Fn[Any]) extends Fn[Int]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Int] =
    in.resolve(ctx).map {
      case null => 0
      case c if c.isInstanceOf[Seq[?]] => c.asInstanceOf[Seq[?]].length
      case s => s.toString.length
    }

case class InterpolateFn(template: Fn[Any], variables: Map[String, Fn[Any]]) extends Fn[String]:

  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, String] =
    for {
      templStr <- template.resolve(ctx).map {
        case null => ""
        case s => s.toString
      }

      // If we don’t already have variables mapped, extract them from the template
      varsToUse = if variables.isEmpty then
        TemplateUtils.extractVariables(templStr).map(v => v -> GetFn(v)).toMap
      else variables

      resolvedVars <- ZIO.foreach(varsToUse.toList) {
        case (k, fn) =>
          fn.resolve(ctx).map(v => k -> Option(v).map(_.toString).getOrElse(""))
      }

      result = {
        val varMap = resolvedVars.toMap

        // This regex captures:
        // 1. variable name with dot/bracket support
        // 2. optional format string
        // 3. optional default value
        val pattern =
          """\{([a-zA-Z_][a-zA-Z0-9_]*(?:\[[0-9]+\]|\.[a-zA-Z_][a-zA-Z0-9_]*(?:\[[0-9]+\])?)*)(?:%([^}:]+))?(?::([^}]+))?\}""".r

        pattern.replaceAllIn(templStr, m => {
          val varName = m.group(1)
          val formatOpt = Option(m.group(2)) // e.g., 0.2f
          val defaultOpt = Option(m.group(3)) // fallback string
          val rawValueOpt = varMap.get(varName).filter(_.nonEmpty)

          (rawValueOpt, formatOpt) match {
            case (Some(value), Some(fmt)) =>
              try String.format(s"%${fmt}", value.toDouble)
              catch case _: Throwable => value

            case (Some(value), None) =>
              value

            case (None, Some(fmt)) =>
              defaultOpt match {
                case Some(dflt) =>
                  try String.format(s"%${fmt}", dflt.toDouble)
                  catch case _: Throwable => dflt
                case None => ""
              }

            case (None, None) =>
              defaultOpt.getOrElse("")
          }
        })
      }
    } yield result

// Extract vars for interpolation
object TemplateUtils {
  private val varPattern =
    """(?:\{([a-zA-Z_][a-zA-Z0-9_]*(?:\[[0-9]+\]|\.[a-zA-Z_][a-zA-Z0-9_]*(?:\[[0-9]+\])?)*)(?:%[^}:]+)?(?::[^}]+)?\}|\$\{([a-zA-Z_][a-zA-Z0-9_]*(?:\[[0-9]+\]|\.[a-zA-Z_][a-zA-Z0-9_]*(?:\[[0-9]+\])?)*)(?:%[^}:]+)?(?::[^}]+)?\})""".r

  def extractVariables(template: String): Set[String] =
    varPattern.findAllMatchIn(template).flatMap { m =>
      Option(m.group(1)).orElse(Option(m.group(2)))
    }.toSet
}

case class SubstringFn(str: Fn[Any], start: Fn[Int], end: Option[Fn[Int]]) extends Fn[String]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, String] =
    for {
      s <- str.resolve(ctx).map {
        case null => ""
        case v => v.toString
      }
      startIdx <- start.resolve(ctx)
      result <- end match
        case Some(endFn) =>
          endFn.resolve(ctx).map(endIdx => s.substring(startIdx, endIdx))
        case None =>
          ZIO.succeed(s.substring(startIdx))
    } yield result

case class ReplaceFn(in: Fn[Any], target: Fn[Any], replacement: Fn[Any]) extends Fn[String]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, String] =
    for {
      str <- in.resolve(ctx).map(v => if v == null then "" else v.toString)
      t <- target.resolve(ctx).map(v => if v == null then "" else v.toString)
      r <- replacement.resolve(ctx).map(v => if v == null then "" else v.toString)
    } yield str.replace(t, r)

case class FilterFn(
                     predicate: Fn[Boolean]
                   ) extends Fn[Any]:

  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ctx.get("this") match
      case Some((v, lens)) =>
        v match
          case items: Iterable[?] =>
            for {
              filtered <- ZIO.foreach(items) { item =>
                val localCtx = ctx + ("this" -> (item, lens)) // replace map's "this" for our filter fn
                predicate.resolve(localCtx).map( b => if b then Some(item) else None )
              }
            } yield filtered.flatten
          case other =>
            ZIO.fail(DynaLensError(s"filter() may only be applied to Iterable types, but got: ${other.getClass.getSimpleName}"))
      case None =>
        ZIO.fail(DynaLensError(s"'this' not found in context"))

case class MapFwdFn(mapName: String) extends Fn[Any]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ctx.get("this") match
      case Some((value, _)) =>
        ZIO.serviceWithZIO[_BiMapRegistry] { registry =>
          registry.get(mapName) match
            case Some(bimap) =>
              bimap.getForward(value.toString) match
                case Some(result) => ZIO.succeed(result)
                case None => ZIO.fail(DynaLensError(s"Key '$value' not found in forward map '$mapName'"))
            case None =>
              ZIO.fail(DynaLensError(s"BiMap '$mapName' not found"))
        }
      case None =>
        ZIO.fail(DynaLensError(s"'this' is not defined in context"))

case class MapRevFn(mapName: String) extends Fn[Any]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ctx.get("this") match
      case Some((value, _)) =>
        ZIO.serviceWithZIO[_BiMapRegistry] { registry =>
          registry.get(mapName) match
            case Some(bimap) =>
              bimap.getReverse(value.toString) match
                case Some(result) => ZIO.succeed(result)
                case None => ZIO.fail(DynaLensError(s"Key '$value' not found in reverse map '$mapName'"))
            case None =>
              ZIO.fail(DynaLensError(s"BiMap '$mapName' not found"))
        }
      case None =>
        ZIO.fail(DynaLensError(s"'this' is not defined in context"))