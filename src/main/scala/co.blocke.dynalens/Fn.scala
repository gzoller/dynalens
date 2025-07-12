package co.blocke.dynalens

import zio.*
import Path.*

trait Fn[R]:
  def resolve(ctx: Map[String,(Any,DynaLens[?])]): ZIO[Any,DynaLensError,R]

// Constant value
case class ConstantFn[R](out: R) extends Fn[R]:
  def resolve(
               ctx: Map[String,(Any,DynaLens[?])] = Map.empty[String,(Any,DynaLens[?])]
             ): ZIO[Any,DynaLensError,R] =
    ZIO.succeed( out )

// Map iteration value:  map{ p => ... }  <-- get p
case class MapParamFn[R]() extends Fn[R]:
  def resolve(
               ctx: Map[String, (Any, DynaLens[?])]
             ): ZIO[Any, DynaLensError, R] =
    ctx.get("__p_").map(_._1) match {
      case Some(v) => ZIO.succeed(v.asInstanceOf[R])
      case None => ZIO.fail(DynaLensError("Attempt to access map parameter 'p' when none has been set (ie outside a map operation)"))
    }

case class GetFn(path: String) extends Fn[Any]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Any] =
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
        ctx.get("this") match {
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
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, R] =
    condition.resolve(ctx).flatMap {
      case true => thenFn.resolve(ctx)
      case false => elseFn.resolve(ctx)
    }

case class BlockFn[R](
                       statements: List[Statement],
                       finalFn: Fn[R]
                     ) extends Fn[R]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, R] =
    val prepped = statements.foldLeft(ZIO.succeed(ctx)) {
      (acc, stmt) => acc.flatMap(stmt.resolve)
    }
    prepped.flatMap(finalFn.resolve)

//
//===============================
//  Comparator Functions
//===============================
//

case class GreaterThanFn(left: Fn[Any], right: Fn[Any]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Boolean] =
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
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Boolean] =
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
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Boolean] =
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
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Boolean] =
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
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Boolean] =
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
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Boolean] =
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

case class OpFn[I, J](
                       left: Fn[I],
                       right: Fn[J],
                       op: (I, J) => Boolean
                     ) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
    } yield op(l, r)

def and(a: Fn[Boolean], b: Fn[Boolean]): Fn[Boolean] =
  OpFn(a, b, _ && _)

def or(a: Fn[Boolean], b: Fn[Boolean]): Fn[Boolean] =
  OpFn(a, b, _ || _)

def not(inner: Fn[Boolean]): Fn[Boolean] = new Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Boolean] =
    inner.resolve(ctx).map(b => !b)

case class StartsWithFn(left: Fn[String], right: Fn[String]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
    } yield l.startsWith(r)

case class EndsWithFn(left: Fn[String], right: Fn[String]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
    } yield l.endsWith(r)

case class ContainsFn(left: Fn[String], right: Fn[String]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
    } yield l.contains(r)

case class EqualsIgnoreCaseFn(left: Fn[String], right: Fn[String]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
    } yield l.equalsIgnoreCase(r)

case class MatchesRegexFn(left: Fn[String], regex: Fn[String]) extends Fn[Boolean]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Boolean] =
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

  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Any] =
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

  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Any] =
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

  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Any] =
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

  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Any] =
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
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, String] =
    in.resolve(ctx).map {
      case null => ""
      case s => s.toString.trim
    }

case class ToLowerFn(in: Fn[Any]) extends Fn[String]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, String] =
    in.resolve(ctx).map {
      case null => ""
      case s => s.toString.toLowerCase
    }

case class ToUpperFn(in: Fn[Any]) extends Fn[String]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, String] =
    in.resolve(ctx).map {
      case null => ""
      case s => s.toString.toUpperCase
    }

case class ConcatFn(parts: List[Fn[Any]]) extends Fn[String]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, String] =
    ZIO.foreach(parts)(_.resolve(ctx)).map(_.map {
      case null => ""
      case s => s.toString
    }.mkString)

case class InterpolateFn(template: String, variables: Map[String, Fn[Any]]) extends Fn[String]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, String] =
    for {
      resolvedVars <- ZIO.foreach(variables.toList) {
        case (k, fn) =>
          fn.resolve(ctx).map(v => k -> (if v == null then "" else v.toString))
      }
      result = resolvedVars.foldLeft(template) { case (acc, (k, v)) =>
        acc.replace(s"{$k}", v)
      }
    } yield result

case class SubstringFn(str: Fn[Any], start: Fn[Int], end: Option[Fn[Int]]) extends Fn[String]:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, String] =
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
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, String] =
    for {
      str <- in.resolve(ctx).map(v => if v == null then "" else v.toString)
      t <- target.resolve(ctx).map(v => if v == null then "" else v.toString)
      r <- replacement.resolve(ctx).map(v => if v == null then "" else v.toString)
    } yield str.replace(t, r)
