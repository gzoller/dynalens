/*
 * Copyright (c) 2025 Greg Zoller
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package co.blocke.dynalens

import zio.*
import Path.*

trait Fn[+R]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, R]
  def as[T]: Fn[T] = this.asInstanceOf[Fn[T]]

// Marker trait for boolean-returning functions
trait BooleanFn extends Fn[Boolean]

// --- Core Functions ----

case class ConstantFn[R](out: R) extends Fn[R]:
  def resolve(
      ctx: DynaContext = scala.collection.mutable.Map.empty
  ): ZIO[_BiMapRegistry, DynaLensError, R] =
    ZIO.succeed(out)

case class GetFn(path: String, searchThis: Boolean = false, elseValue: Option[Fn[Any]] = None, isDefined: Boolean = false, useRawValue: Boolean = false) extends Fn[Any]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    // Multi-part path
    parsePath(path) match {
      case Nil =>
        ZIO.fail(DynaLensError("get requires a path"))

      case first :: rest if ctx.contains(first.name) =>
        ctx.get(first.name) match {
          case Some((obj, None)) => // val de-reference
            ZIO.succeed(obj)

          case Some((obj, Some(dynalens))) =>
            if rest.isEmpty then ZIO.succeed(obj)
            else dynalens.get(partialPath(rest), obj.asInstanceOf[dynalens.ThisT])

          case None =>
            if searchThis then GetFn(path = "this." + path, elseValue = elseValue).resolve(ctx)
            else ZIO.fail(DynaLensError(s"Field $path not found in context"))
        }

      // Single-part path
      case _ =>
        ctx.get("top") match {
          case Some((obj, Some(dynalens))) =>
            dynalens.get(path, obj.asInstanceOf[dynalens.ThisT]).catchSome {
              case e: DynaLensError if e.msg.startsWith("Field not found") && searchThis =>
                GetFn(path = "this." + path, elseValue = elseValue).resolve(ctx)
            }

          case _ =>
            ZIO.fail(DynaLensError(s"Field $path not found"))
        }
    }
      .flatMap { obj =>
        unwrapOption(path, obj, elseValue, isDefined, useRawValue) match
          case Left(err) =>
            ZIO.fail(err)

          case Right(Left(altFn)) =>
            altFn.resolve(ctx)

          case Right(Right(value)) =>
            ZIO.succeed(value)
      }

case class IfFn[R](
    condition: Fn[Boolean],
    thenFn: Fn[R],
    elseFn: Fn[R]
) extends Fn[R]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, R] =
    condition.resolve(ctx).flatMap {
      case true  => thenFn.resolve(ctx)
      case false => elseFn.resolve(ctx)
    }

case class BlockFn[R](
    statements: Seq[Statement],
    finalFn: Fn[R]
) extends Fn[R]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, R] =
    val prepped = statements.foldLeft(
      ZIO.succeed(ctx): ZIO[_BiMapRegistry, DynaLensError, DynaContext]
    )((acc, stmt) => acc.flatMap(stmt.resolve))
    prepped.flatMap(finalFn.resolve)

// --- Boolean Functions ----

case class EqualFn(left: Fn[Any], right: Fn[Any]) extends BooleanFn:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match
        case (l: Int, r: Int)         => ZIO.succeed(l == r)
        case (l: Int, r: Long)        => ZIO.succeed(l.asInstanceOf[Long] == r.asInstanceOf[Long])
        case (l: Int, r: Float)       => ZIO.succeed(l.asInstanceOf[Float] == r.asInstanceOf[Float])
        case (l: Int, r: Double)      => ZIO.succeed(l.asInstanceOf[Double] == r.asInstanceOf[Double])
        case (l: Long, r: Int)        => ZIO.succeed(l.asInstanceOf[Long] == r.asInstanceOf[Long])
        case (l: Long, r: Long)       => ZIO.succeed(l == r)
        case (l: Long, r: Float)      => ZIO.succeed(l.asInstanceOf[Float] == r.asInstanceOf[Float])
        case (l: Long, r: Double)     => ZIO.succeed(l.asInstanceOf[Double] == r.asInstanceOf[Double])
        case (l: Float, r: Int)       => ZIO.succeed(l.asInstanceOf[Float] == r.asInstanceOf[Float])
        case (l: Float, r: Long)      => ZIO.succeed(l.asInstanceOf[Float] == r.asInstanceOf[Float])
        case (l: Float, r: Float)     => ZIO.succeed(l == r)
        case (l: Float, r: Double)    => ZIO.succeed(l.asInstanceOf[Double] == r.asInstanceOf[Double])
        case (l: Double, r: Int)      => ZIO.succeed(l.asInstanceOf[Double] == r.asInstanceOf[Double])
        case (l: Double, r: Long)     => ZIO.succeed(l.asInstanceOf[Double] == r.asInstanceOf[Double])
        case (l: Double, r: Float)    => ZIO.succeed(l.asInstanceOf[Double] == r.asInstanceOf[Double])
        case (l: Double, r: Double)   => ZIO.succeed(l == r)
        case (ls: String, rs: String) => ZIO.succeed(ls == rs)
        case (lVal, rVal)             => ZIO.fail(DynaLensError(s"Cannot compare types: ${lVal.getClass} and ${rVal.getClass}"))
    } yield result

case class NotEqualFn(left: Fn[Any], right: Fn[Any]) extends BooleanFn:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match
        case (l: Int, r: Int)         => ZIO.succeed(l != r)
        case (l: Int, r: Long)        => ZIO.succeed(l.asInstanceOf[Long] != r.asInstanceOf[Long])
        case (l: Int, r: Float)       => ZIO.succeed(l.asInstanceOf[Float] != r.asInstanceOf[Float])
        case (l: Int, r: Double)      => ZIO.succeed(l.asInstanceOf[Double] != r.asInstanceOf[Double])
        case (l: Long, r: Int)        => ZIO.succeed(l.asInstanceOf[Long] != r.asInstanceOf[Long])
        case (l: Long, r: Long)       => ZIO.succeed(l != r)
        case (l: Long, r: Float)      => ZIO.succeed(l.asInstanceOf[Float] != r.asInstanceOf[Float])
        case (l: Long, r: Double)     => ZIO.succeed(l.asInstanceOf[Double] != r.asInstanceOf[Double])
        case (l: Float, r: Int)       => ZIO.succeed(l.asInstanceOf[Float] != r.asInstanceOf[Float])
        case (l: Float, r: Long)      => ZIO.succeed(l.asInstanceOf[Float] != r.asInstanceOf[Float])
        case (l: Float, r: Float)     => ZIO.succeed(l != r)
        case (l: Float, r: Double)    => ZIO.succeed(l.asInstanceOf[Double] != r.asInstanceOf[Double])
        case (l: Double, r: Int)      => ZIO.succeed(l.asInstanceOf[Double] != r.asInstanceOf[Double])
        case (l: Double, r: Long)     => ZIO.succeed(l.asInstanceOf[Double] != r.asInstanceOf[Double])
        case (l: Double, r: Float)    => ZIO.succeed(l.asInstanceOf[Double] != r.asInstanceOf[Double])
        case (l: Double, r: Double)   => ZIO.succeed(l != r)
        case (ls: String, rs: String) => ZIO.succeed(ls != rs)
        case (lVal, rVal)             => ZIO.fail(DynaLensError(s"Cannot compare types: ${lVal.getClass} and ${rVal.getClass}"))
    } yield result

case class GreaterThanFn(left: Fn[Any], right: Fn[Any]) extends BooleanFn:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match
        case (l: Int, r: Int)         => ZIO.succeed(l > r)
        case (l: Int, r: Long)        => ZIO.succeed(l.asInstanceOf[Long] > r.asInstanceOf[Long])
        case (l: Int, r: Float)       => ZIO.succeed(l.asInstanceOf[Float] > r.asInstanceOf[Float])
        case (l: Int, r: Double)      => ZIO.succeed(l.asInstanceOf[Double] > r.asInstanceOf[Double])
        case (l: Long, r: Int)        => ZIO.succeed(l.asInstanceOf[Long] > r.asInstanceOf[Long])
        case (l: Long, r: Long)       => ZIO.succeed(l > r)
        case (l: Long, r: Float)      => ZIO.succeed(l.asInstanceOf[Float] > r.asInstanceOf[Float])
        case (l: Long, r: Double)     => ZIO.succeed(l.asInstanceOf[Double] > r.asInstanceOf[Double])
        case (l: Float, r: Int)       => ZIO.succeed(l.asInstanceOf[Float] > r.asInstanceOf[Float])
        case (l: Float, r: Long)      => ZIO.succeed(l.asInstanceOf[Float] > r.asInstanceOf[Float])
        case (l: Float, r: Float)     => ZIO.succeed(l > r)
        case (l: Float, r: Double)    => ZIO.succeed(l.asInstanceOf[Double] > r.asInstanceOf[Double])
        case (l: Double, r: Int)      => ZIO.succeed(l.asInstanceOf[Double] > r.asInstanceOf[Double])
        case (l: Double, r: Long)     => ZIO.succeed(l.asInstanceOf[Double] > r.asInstanceOf[Double])
        case (l: Double, r: Float)    => ZIO.succeed(l.asInstanceOf[Double] > r.asInstanceOf[Double])
        case (l: Double, r: Double)   => ZIO.succeed(l > r)
        case (ls: String, rs: String) => ZIO.fail(DynaLensError("Cannot perform ordering on strings"))
        case (lVal, rVal)             => ZIO.fail(DynaLensError(s"Cannot compare types: ${lVal.getClass} and ${rVal.getClass}"))
    } yield result

case class LessThanFn(left: Fn[Any], right: Fn[Any]) extends BooleanFn:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match
        case (l: Int, r: Int)         => ZIO.succeed(l < r)
        case (l: Int, r: Long)        => ZIO.succeed(l.asInstanceOf[Long] < r.asInstanceOf[Long])
        case (l: Int, r: Float)       => ZIO.succeed(l.asInstanceOf[Float] < r.asInstanceOf[Float])
        case (l: Int, r: Double)      => ZIO.succeed(l.asInstanceOf[Double] < r.asInstanceOf[Double])
        case (l: Long, r: Int)        => ZIO.succeed(l.asInstanceOf[Long] < r.asInstanceOf[Long])
        case (l: Long, r: Long)       => ZIO.succeed(l < r)
        case (l: Long, r: Float)      => ZIO.succeed(l.asInstanceOf[Float] < r.asInstanceOf[Float])
        case (l: Long, r: Double)     => ZIO.succeed(l.asInstanceOf[Double] < r.asInstanceOf[Double])
        case (l: Float, r: Int)       => ZIO.succeed(l.asInstanceOf[Float] < r.asInstanceOf[Float])
        case (l: Float, r: Long)      => ZIO.succeed(l.asInstanceOf[Float] < r.asInstanceOf[Float])
        case (l: Float, r: Float)     => ZIO.succeed(l < r)
        case (l: Float, r: Double)    => ZIO.succeed(l.asInstanceOf[Double] < r.asInstanceOf[Double])
        case (l: Double, r: Int)      => ZIO.succeed(l.asInstanceOf[Double] < r.asInstanceOf[Double])
        case (l: Double, r: Long)     => ZIO.succeed(l.asInstanceOf[Double] < r.asInstanceOf[Double])
        case (l: Double, r: Float)    => ZIO.succeed(l.asInstanceOf[Double] < r.asInstanceOf[Double])
        case (l: Double, r: Double)   => ZIO.succeed(l < r)
        case (ls: String, rs: String) => ZIO.fail(DynaLensError("Cannot perform ordering on strings"))
        case (lVal, rVal)             => ZIO.fail(DynaLensError(s"Cannot compare types: ${lVal.getClass} and ${rVal.getClass}"))
    } yield result

case class GreaterThanOrEqualFn(left: Fn[Any], right: Fn[Any]) extends BooleanFn:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match
        case (l: Int, r: Int)         => ZIO.succeed(l >= r)
        case (l: Int, r: Long)        => ZIO.succeed(l.asInstanceOf[Long] >= r.asInstanceOf[Long])
        case (l: Int, r: Float)       => ZIO.succeed(l.asInstanceOf[Float] >= r.asInstanceOf[Float])
        case (l: Int, r: Double)      => ZIO.succeed(l.asInstanceOf[Double] >= r.asInstanceOf[Double])
        case (l: Long, r: Int)        => ZIO.succeed(l.asInstanceOf[Long] >= r.asInstanceOf[Long])
        case (l: Long, r: Long)       => ZIO.succeed(l >= r)
        case (l: Long, r: Float)      => ZIO.succeed(l.asInstanceOf[Float] >= r.asInstanceOf[Float])
        case (l: Long, r: Double)     => ZIO.succeed(l.asInstanceOf[Double] >= r.asInstanceOf[Double])
        case (l: Float, r: Int)       => ZIO.succeed(l.asInstanceOf[Float] >= r.asInstanceOf[Float])
        case (l: Float, r: Long)      => ZIO.succeed(l.asInstanceOf[Float] >= r.asInstanceOf[Float])
        case (l: Float, r: Float)     => ZIO.succeed(l >= r)
        case (l: Float, r: Double)    => ZIO.succeed(l.asInstanceOf[Double] >= r.asInstanceOf[Double])
        case (l: Double, r: Int)      => ZIO.succeed(l.asInstanceOf[Double] >= r.asInstanceOf[Double])
        case (l: Double, r: Long)     => ZIO.succeed(l.asInstanceOf[Double] >= r.asInstanceOf[Double])
        case (l: Double, r: Float)    => ZIO.succeed(l.asInstanceOf[Double] >= r.asInstanceOf[Double])
        case (l: Double, r: Double)   => ZIO.succeed(l >= r)
        case (ls: String, rs: String) => ZIO.fail(DynaLensError("Cannot perform ordering on strings"))
        case (lVal, rVal)             => ZIO.fail(DynaLensError(s"Cannot compare types: ${lVal.getClass} and ${rVal.getClass}"))
    } yield result

case class LessThanOrEqualFn(left: Fn[Any], right: Fn[Any]) extends BooleanFn:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match
        case (l: Int, r: Int)         => ZIO.succeed(l <= r)
        case (l: Int, r: Long)        => ZIO.succeed(l.asInstanceOf[Long] <= r.asInstanceOf[Long])
        case (l: Int, r: Float)       => ZIO.succeed(l.asInstanceOf[Float] <= r.asInstanceOf[Float])
        case (l: Int, r: Double)      => ZIO.succeed(l.asInstanceOf[Double] <= r.asInstanceOf[Double])
        case (l: Long, r: Int)        => ZIO.succeed(l.asInstanceOf[Long] <= r.asInstanceOf[Long])
        case (l: Long, r: Long)       => ZIO.succeed(l <= r)
        case (l: Long, r: Float)      => ZIO.succeed(l.asInstanceOf[Float] <= r.asInstanceOf[Float])
        case (l: Long, r: Double)     => ZIO.succeed(l.asInstanceOf[Double] <= r.asInstanceOf[Double])
        case (l: Float, r: Int)       => ZIO.succeed(l.asInstanceOf[Float] <= r.asInstanceOf[Float])
        case (l: Float, r: Long)      => ZIO.succeed(l.asInstanceOf[Float] <= r.asInstanceOf[Float])
        case (l: Float, r: Float)     => ZIO.succeed(l <= r)
        case (l: Float, r: Double)    => ZIO.succeed(l.asInstanceOf[Double] <= r.asInstanceOf[Double])
        case (l: Double, r: Int)      => ZIO.succeed(l.asInstanceOf[Double] <= r.asInstanceOf[Double])
        case (l: Double, r: Long)     => ZIO.succeed(l.asInstanceOf[Double] <= r.asInstanceOf[Double])
        case (l: Double, r: Float)    => ZIO.succeed(l.asInstanceOf[Double] <= r.asInstanceOf[Double])
        case (l: Double, r: Double)   => ZIO.succeed(l <= r)
        case (ls: String, rs: String) => ZIO.fail(DynaLensError("Cannot perform ordering on strings"))
        case (lVal, rVal)             => ZIO.fail(DynaLensError(s"Cannot compare types: ${lVal.getClass} and ${rVal.getClass}"))
    } yield result

case class AndFn(left: BooleanFn, right: BooleanFn) extends BooleanFn:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
    } yield l && r

case class OrFn(left: BooleanFn, right: BooleanFn) extends BooleanFn:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
    } yield l || r

case class NotFn(inner: BooleanFn) extends BooleanFn:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    inner.resolve(ctx).map(b => !b)

case class IsDefinedFn(inner: Fn[Any]) extends BooleanFn {
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      value <- inner.resolve(ctx)
    } yield value match {
      case opt: Option[?] => opt.isDefined
      case null           => false
      case _              =>
        // Non-option types are always considered defined
        true
    }
}

// Special converter: Fn[Any]->BooleanFn
case class toBooleanFn(inner: Fn[Any]) extends BooleanFn {
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      r <- inner.resolve(ctx)
      typedResult <- r match {
        case b: Boolean => ZIO.succeed(b)
        case other      => ZIO.fail(DynaLensError(s"Expected Boolean result at runtime, but got: ${other.getClass.getSimpleName} = $other"))
      }
    } yield typedResult
}

// ---- String Boolean Funcitons ----

case class StartsWithFn(left: Fn[String], right: Fn[String]) extends BooleanFn:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
    } yield l.startsWith(r)

case class EndsWithFn(left: Fn[String], right: Fn[String]) extends BooleanFn:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
    } yield l.endsWith(r)

case class ContainsFn(left: Fn[String], right: Fn[String]) extends BooleanFn:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
    } yield l.contains(r)

case class EqualsIgnoreCaseFn(left: Fn[String], right: Fn[String]) extends BooleanFn:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
    } yield l.equalsIgnoreCase(r)

case class MatchesRegexFn(left: Fn[String], regex: Fn[String]) extends BooleanFn:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- regex.resolve(ctx)
    } yield l.matches(r)

// --- Arithmetic  Functions ----

case class NegateFn(
    target: Fn[Any]
) extends Fn[Any]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      t <- target.resolve(ctx)
      result <- t match {
        case a: Int    => ZIO.succeed(a * -1)
        case a: Long   => ZIO.succeed(a * -1)
        case a: Float  => ZIO.succeed(a * -1)
        case a: Double => ZIO.succeed(a * -1)
        case _ =>
          ZIO.fail(DynaLensError(s"NegateFn does not support an operand of type: ${t.getClass}"))
      }
    } yield result

case class ModuloFn(left: Fn[Any], right: Fn[Any]) extends Fn[Any]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match
        case (li: Int, ri: Int)   => ZIO.succeed(li % ri)
        case (li: Long, ri: Long) => ZIO.succeed(li % ri)
        case (li: Int, rl: Long)  => ZIO.succeed(li.toLong % rl)
        case (ll: Long, ri: Int)  => ZIO.succeed(ll % ri.toLong)
        case _ =>
          ZIO.fail(DynaLensError(s"Modulo (%) only supported for Int and Long, not (${l.getClass}, ${r.getClass})"))
    } yield result

case class AddFn(
    left: Fn[Any],
    right: Fn[Any]
) extends Fn[Any]:

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match {
        // Int cases
        case (a: Int, b: Int)    => ZIO.succeed(a + b)
        case (a: Int, b: Long)   => ZIO.succeed(a + b)
        case (a: Int, b: Float)  => ZIO.succeed(a + b)
        case (a: Int, b: Double) => ZIO.succeed(a + b)

        // Long cases
        case (a: Long, b: Int)    => ZIO.succeed(a + b)
        case (a: Long, b: Long)   => ZIO.succeed(a + b)
        case (a: Long, b: Float)  => ZIO.succeed(a + b)
        case (a: Long, b: Double) => ZIO.succeed(a + b)

        // Float cases
        case (a: Float, b: Int)    => ZIO.succeed(a + b)
        case (a: Float, b: Long)   => ZIO.succeed(a + b)
        case (a: Float, b: Float)  => ZIO.succeed(a + b)
        case (a: Float, b: Double) => ZIO.succeed(a + b)

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

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match {
        case (a: Int, b: Int)    => ZIO.succeed(a - b)
        case (a: Int, b: Long)   => ZIO.succeed(a - b)
        case (a: Int, b: Float)  => ZIO.succeed(a - b)
        case (a: Int, b: Double) => ZIO.succeed(a - b)

        case (a: Long, b: Int)    => ZIO.succeed(a - b)
        case (a: Long, b: Long)   => ZIO.succeed(a - b)
        case (a: Long, b: Float)  => ZIO.succeed(a - b)
        case (a: Long, b: Double) => ZIO.succeed(a - b)

        case (a: Float, b: Int)    => ZIO.succeed(a - b)
        case (a: Float, b: Long)   => ZIO.succeed(a - b)
        case (a: Float, b: Float)  => ZIO.succeed(a - b)
        case (a: Float, b: Double) => ZIO.succeed(a - b)

        case (a: Double, b: Int)    => ZIO.succeed(a - b)
        case (a: Double, b: Long)   => ZIO.succeed(a - b)
        case (a: Double, b: Float)  => ZIO.succeed(a - b)
        case (a: Double, b: Double) => ZIO.succeed(a - b)

        case _ => ZIO.fail(DynaLensError(s"SubtractFn does not support operands of types: ${l.getClass}, ${r.getClass}"))
      }
    } yield result

case class MultiplyFn(
    left: Fn[Any],
    right: Fn[Any]
) extends Fn[Any]:

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match {
        case (a: Int, b: Int)    => ZIO.succeed(a * b)
        case (a: Int, b: Long)   => ZIO.succeed(a * b)
        case (a: Int, b: Float)  => ZIO.succeed(a * b)
        case (a: Int, b: Double) => ZIO.succeed(a * b)

        case (a: Long, b: Int)    => ZIO.succeed(a * b)
        case (a: Long, b: Long)   => ZIO.succeed(a * b)
        case (a: Long, b: Float)  => ZIO.succeed(a * b)
        case (a: Long, b: Double) => ZIO.succeed(a * b)

        case (a: Float, b: Int)    => ZIO.succeed(a * b)
        case (a: Float, b: Long)   => ZIO.succeed(a * b)
        case (a: Float, b: Float)  => ZIO.succeed(a * b)
        case (a: Float, b: Double) => ZIO.succeed(a * b)

        case (a: Double, b: Int)    => ZIO.succeed(a * b)
        case (a: Double, b: Long)   => ZIO.succeed(a * b)
        case (a: Double, b: Float)  => ZIO.succeed(a * b)
        case (a: Double, b: Double) => ZIO.succeed(a * b)

        case _ => ZIO.fail(DynaLensError(s"MultiplyFn does not support operands of types: ${l.getClass}, ${r.getClass}"))
      }
    } yield result

case class DivideFn(
    left: Fn[Any],
    right: Fn[Any]
) extends Fn[Any]:

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match {
        case (_, 0 | 0L | 0.0f | 0.0d) => ZIO.fail(DynaLensError("Divide by zero"))

        case (a: Int, b: Int)    => ZIO.succeed(a / b)
        case (a: Int, b: Long)   => ZIO.succeed(a / b)
        case (a: Int, b: Float)  => ZIO.succeed(a / b)
        case (a: Int, b: Double) => ZIO.succeed(a / b)

        case (a: Long, b: Int)    => ZIO.succeed(a / b)
        case (a: Long, b: Long)   => ZIO.succeed(a / b)
        case (a: Long, b: Float)  => ZIO.succeed(a / b)
        case (a: Long, b: Double) => ZIO.succeed(a / b)

        case (a: Float, b: Int)    => ZIO.succeed(a / b)
        case (a: Float, b: Long)   => ZIO.succeed(a / b)
        case (a: Float, b: Float)  => ZIO.succeed(a / b)
        case (a: Float, b: Double) => ZIO.succeed(a / b)

        case (a: Double, b: Int)    => ZIO.succeed(a / b)
        case (a: Double, b: Long)   => ZIO.succeed(a / b)
        case (a: Double, b: Float)  => ZIO.succeed(a / b)
        case (a: Double, b: Double) => ZIO.succeed(a / b)

        case _ => ZIO.fail(DynaLensError(s"DivideFn does not support operands of types: ${l.getClass}, ${r.getClass}"))
      }
    } yield result

// --- String Builder Functions ----

case class TrimFn(in: Fn[Any]) extends Fn[String]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    in.resolve(ctx).map {
      case null => ""
      case s    => s.toString.trim
    }

case class ToLowerFn(in: Fn[Any]) extends Fn[String]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    in.resolve(ctx).map {
      case null => ""
      case s    => s.toString.toLowerCase
    }

case class ToUpperFn(in: Fn[Any]) extends Fn[String]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    in.resolve(ctx).map {
      case null => ""
      case s    => s.toString.toUpperCase
    }

case class ConcatFn(parts: List[Fn[Any]]) extends Fn[String]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    ZIO
      .foreach(parts)(_.resolve(ctx))
      .map(_.map {
        case null => ""
        case s    => s.toString
      }.mkString)

case class InterpolateFn(template: Fn[Any], variables: Map[String, Fn[Any]]) extends Fn[String]:

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    for {
      templStr <- template.resolve(ctx).map {
        case null => ""
        case s    => s.toString
      }

      // If we don’t already have variables mapped, extract them from the template
      varsToUse =
        if variables.isEmpty then TemplateUtils.extractVariables(templStr).map(v => v -> GetFn(v)).toMap
        else variables

      resolvedVars <- ZIO.foreach(varsToUse.toList) { case (k, fn) =>
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

        pattern.replaceAllIn(
          templStr,
          m => {
            val varName = m.group(1)
            val formatOpt = Option(m.group(2)) // e.g., 0.2f
            val defaultOpt = Option(m.group(3)) // fallback string
            val rawValueOpt = varMap.get(varName).filter(_.nonEmpty)

            (rawValueOpt, formatOpt) match {
              case (Some(value), Some(fmt)) =>
                try String.format(s"%$fmt", value.toDouble)
                catch case _: Throwable => value

              case (Some(value), None) =>
                value

              case (None, Some(fmt)) =>
                defaultOpt match {
                  case Some(dflt) =>
                    try String.format(s"%$fmt", dflt.toDouble)
                    catch case _: Throwable => dflt
                  case None => ""
                }

              case (None, None) =>
                defaultOpt.getOrElse("")
            }
          }
        )
      }
    } yield result

// Extract vars for interpolation
object TemplateUtils {
  private val varPattern =
    """\{([a-zA-Z_][a-zA-Z0-9_]*(?:\[[0-9]+\]|\.[a-zA-Z_][a-zA-Z0-9_]*(?:\[[0-9]+\])?)*)(?:%[^}:]+)?(?::[^}]+)?\}""".r

  def extractVariables(template: String): Set[String] =
    varPattern
      .findAllMatchIn(template)
      .flatMap { m =>
        Option(m.group(1)).orElse(Option(m.group(2)))
      }
      .toSet
}

case class SubstringFn(str: Fn[Any], start: Fn[Int], end: Option[Fn[Int]]) extends Fn[String]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    for {
      s <- str.resolve(ctx).map {
        case null => ""
        case v    => v.toString
      }
      startIdx <- start.resolve(ctx)
      result <- end match {
        case Some(endFn) =>
          endFn.resolve(ctx).map(endIdx => s.substring(startIdx, endIdx))
        case None =>
          ZIO.succeed(s.substring(startIdx))
      }
    } yield result

case class ReplaceFn(in: Fn[Any], target: Fn[Any], replacement: Fn[Any]) extends Fn[String]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    for {
      str <- in.resolve(ctx).map(v => if v == null then "" else v.toString)
      t <- target.resolve(ctx).map(v => if v == null then "" else v.toString)
      r <- replacement.resolve(ctx).map(v => if v == null then "" else v.toString)
    } yield str.replace(t, r)

// --- Collection (Iterable) Functions ----

case class FilterFn(predicate: Fn[Boolean]) extends Fn[Any]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ctx.get("this") match
      case Some((v: Iterable[?], lens)) =>
        ZIO
          .foreach(v) { item =>
            val localCtx = ctx.updatedWith("this", (item, lens))
            predicate.resolve(localCtx).map(b => if b then Some(item) else None)
          }
          .map(_.flatten)
      case Some((other, _)) =>
        ZIO.fail(DynaLensError(s"filter() may only be applied to Iterable types, got: ${other.getClass.getSimpleName}"))
      case None =>
        ZIO.fail(DynaLensError(s"'this' not found in context"))

case class SortFn(
    path: Option[String],
    asc: Boolean = true
) extends Fn[Any]:
  private def sortV(v: Iterable[Any]): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      seq <- ZIO.foreach(v) {
        case c: Comparable[?] => ZIO.succeed(c.asInstanceOf[Comparable[Any]])
        case other =>
          ZIO.fail(DynaLensError(s"Item '$other' is not Comparable"))
      }
    } yield {
      implicit val ordering: Ordering[Comparable[Any]] =
        (a: Comparable[Any], b: Comparable[Any]) => a.compareTo(b)

      val sorted = seq.toSeq.sorted
      if asc then sorted else sorted.reverse
    }

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ctx.get("this") match {
      case Some((maybeIterable, lens)) =>
        unwrapOption("[]", maybeIterable, Some(ConstantFn(Nil)), false) match {
          case Left(err)             => ZIO.fail(err)
          case Right(Left(fallback)) => fallback.resolve(ctx)
          case Right(Right(value)) =>
            value match
              case v: Iterable[?] =>
                lens match
                  case Some(dlens: DynaLens[?]) =>
                    path match
                      case Some(pth) =>
                        ZIO
                          .foreach(v) { item =>
                            dlens
                              .asInstanceOf[DynaLens[Any]]
                              .get(pth, item.asInstanceOf[dlens.ThisT])
                              .map { value =>
                                (item, value.asInstanceOf[Comparable[Any]])
                              }
                          }
                          .map { itemValuePairs =>
                            val sorted = itemValuePairs.toSeq.sortBy(_._2)
                            if asc then sorted.map(_._1) else sorted.reverse.map(_._1)
                          }

                      case None =>
                        sortV(v)

                  case None =>
                    sortV(v)

              case _ =>
                ZIO.fail(
                  DynaLensError(
                    s"sort${if asc then "Asc" else "Desc"}() may only be applied to Iterable types, but got: ${value.getClass.getSimpleName}"
                  )
                )
        }
      case None =>
        ZIO.fail(DynaLensError(s"'this' not found in context"))
    }

case class DistinctFn(fieldPath: Option[String]) extends Fn[Any]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ctx.get("this") match
      case Some((items: Iterable[?], lens)) =>
        fieldPath match
          case Some(field) =>
            val dlens = lens.get // <-- Safe because ._1 would be None not Iterable if this is a simple type
            for {
              itemPairs <- ZIO.foreach(items) { item =>
                dlens.get(field, item.asInstanceOf[dlens.ThisT]).map(v => (v, item))
              }
              // groupBy first value, then keep distinct items
              deduped = itemPairs.groupBy(_._1).values.map(_.head._2)
            } yield deduped.toList

          case None =>
            ZIO.succeed(items.toSet.toList)

      case Some((nonIterable, _)) =>
        ZIO.fail(DynaLensError(s"distinct() may only be applied to Iterable types, but got: ${nonIterable.getClass.getSimpleName}"))

      case None =>
        ZIO.fail(DynaLensError(s"'this' not found in context"))

case class LimitFn(count: Int) extends Fn[Any]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ctx.get("this") match
      case Some((items: Iterable[?], _)) =>
        ZIO.succeed(items.take(count).toList)

      case Some((nonIterable, _)) =>
        ZIO.fail(DynaLensError(s"limit() may only be applied to Iterable types, but got: ${nonIterable.getClass.getSimpleName}"))

      case None =>
        ZIO.fail(DynaLensError(s"'this' not found in context"))

case class ReverseFn() extends Fn[Any]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ctx.get("this") match
      case Some((items: Iterable[?], _)) =>
        ZIO.succeed(items.toList.reverse)

      case Some((nonIterable, _)) =>
        ZIO.fail(DynaLensError(s"reverse() may only be applied to Iterable types, but got: ${nonIterable.getClass.getSimpleName}"))

      case None =>
        ZIO.fail(DynaLensError(s"'this' not found in context"))

case class CleanFn() extends Fn[Any]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ctx.get("this") match
      case Some((items: Iterable[?], _)) =>
        ZIO.succeed(items.filter {
          case null    => false
          case None    => false
          case _: Unit => false
          case ""      => false
          case _       => true
        })

      case Some((nonIterable, _)) =>
        ZIO.fail(DynaLensError(s"clean() may only be applied to Iterable types, but got: ${nonIterable.getClass.getSimpleName}"))
      case None =>
        ZIO.fail(DynaLensError(s"'this' not found in context"))

// --- Misc Functions ----

case class PolyFn(fns: List[Fn[Any]]) extends Fn[Any] {
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ctx.get("this") match
      case Some((value, lens)) =>
        ZIO.foreachDiscard(fns) { fn =>
          for {
            result <- fn.resolve(ctx)
            _ = ctx.put("this", (result, lens))
          } yield ()
        } *> ZIO.succeed(ctx("this")._1)
      case None =>
        ZIO.fail(DynaLensError(s"'this' not found in context"))
}

case object IdentityFn extends Fn[Any]: // No-op function (for chaining)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ctx.get("this") match
      case Some((v, _)) => ZIO.succeed(v)
      case None         => ZIO.fail(DynaLensError("'this' not found in context"))

case class LengthFn(in: Fn[Any]) extends Fn[Int]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Int] =
    in.resolve(ctx).map {
      case null                        => 0
      case c if c.isInstanceOf[Seq[?]] => c.asInstanceOf[Seq[?]].length
      case s                           => s.toString.length
    }

case class MapFwdFn(mapName: String) extends Fn[Any]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ctx.get("this") match
      case Some((value, _)) =>
        ZIO.serviceWithZIO[_BiMapRegistry] { registry =>
          registry.get(mapName) match
            case Some(bimap) =>
              value match
                case iter: Iterable[?] =>
                  ZIO
                    .foreach(iter) { item =>
                      bimap.getForward(item.toString) match
                        case Some(res) => ZIO.succeed(res)
                        case None      => ZIO.fail(DynaLensError(s"Key '$item' not found in forward map '$mapName'"))
                    }
                    .map(_.toList)
                case _ =>
                  bimap.getForward(value.toString) match
                    case Some(result) => ZIO.succeed(result)
                    case None         => ZIO.fail(DynaLensError(s"Key '$value' not found in forward map '$mapName'"))
            case None =>
              ZIO.fail(DynaLensError(s"BiMap '$mapName' not found"))
        }
      case None =>
        ZIO.fail(DynaLensError(s"'this' is not defined in context"))

case class MapRevFn(mapName: String) extends Fn[Any]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ctx.get("this") match
      case Some((value, _)) =>
        ZIO.serviceWithZIO[_BiMapRegistry] { registry =>
          registry.get(mapName) match
            case Some(bimap) =>
              value match
                case iter: Iterable[?] =>
                  ZIO
                    .foreach(iter) { item =>
                      bimap.getReverse(item.toString) match
                        case Some(res) => ZIO.succeed(res)
                        case None      => ZIO.fail(DynaLensError(s"Key '$item' not found in reverse map '$mapName'"))
                    }
                    .map(_.toList)
                case _ =>
                  bimap.getReverse(value.toString) match
                    case Some(result) => ZIO.succeed(result)
                    case None         => ZIO.fail(DynaLensError(s"Key '$value' not found in reverse map '$mapName'"))
            case None =>
              ZIO.fail(DynaLensError(s"BiMap '$mapName' not found"))
        }
      case None =>
        ZIO.fail(DynaLensError(s"'this' is not defined in context"))

case class FormatDateFn(dateExpr: Fn[Any], pattern: Fn[String]) extends Fn[Any] {
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      d <- dateExpr.resolve(ctx)
      p <- pattern.resolve(ctx)
      result <- d match
        case d: java.util.Date =>
          ZIO
            .attempt {
              val sdf = new java.text.SimpleDateFormat(p)
              sdf.format(d)
            }
            .mapError(e => DynaLensError(s"Error formatting date: ${e.getMessage}"))
        case other =>
          ZIO.fail(DynaLensError(s"Expected java.util.Date but got ${other.getClass.getName}"))
    } yield result
}

case class ParseDateFn(strExpr: Fn[Any], pattern: Fn[String]) extends Fn[Any] {
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      s <- strExpr.resolve(ctx)
      p <- pattern.resolve(ctx)
      result <- s match
        case s: String =>
          ZIO
            .attempt {
              val sdf = new java.text.SimpleDateFormat(p)
              sdf.parse(s)
            }
            .mapError(e => DynaLensError(s"Date parse error: ${e.getMessage}"))
        case other =>
          ZIO.fail(DynaLensError(s"Expected String for toDate but got ${other.getClass.getName}"))
    } yield result
}

// --- Stand-Alone Functions ----

case object NowFn extends Fn[Any] {
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ZIO.succeed(new java.util.Date())
}

case object UUIDFn extends Fn[Any] {
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ZIO.succeed(java.util.UUID.randomUUID())
}
