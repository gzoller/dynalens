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

import java.util.Locale
import NumPromote.*

import scala.annotation.tailrec

trait Fn[+R]:
  def recv: Option[Fn[?]] = None
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, R]
  def as[T]: Fn[T] = this.asInstanceOf[Fn[T]]
  def methodName: String = this.getClass.getSimpleName.stripSuffix("Fn").decapitalize

// Marker trait for boolean-returning functions
trait BooleanFn extends Fn[Boolean]

case class NoneFn() extends Fn[Any]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, None.type] = ZIO.succeed(None)

// --- Core Functions ----

case class ConstantFn[R](out: R) extends Fn[R]:
  def resolve(
      ctx: DynaContext = scala.collection.mutable.Map.empty
  ): ZIO[_BiMapRegistry, DynaLensError, R] =
    ZIO.succeed(out)

case class GetFn(path: String) extends Fn[Any] {

  import Path.* // for parsePath/PathElement/Field/IndexedField/partialPath

  // Generic walker that can traverse case classes, Maps and (indexed) collections
  @tailrec
  private def walk(obj: Any, parts: List[PathElement]): Either[DynaLensError, Any] = {
    def fieldOf(p: Product, name: String): Option[Any] = {
      val names = p.productElementNames.iterator
      var i = 0
      while names.hasNext do {
        if names.next() == name then return Some(p.productElement(i))
        i += 1
      }
      None
    }

    parts match {
      case Nil => Right(obj)

      case Field(name, _) :: tail =>
        val nextOpt =
          obj match {
            case m: Map[?, ?] =>
              m.asInstanceOf[Map[String, Any]].get(name)
            case p: Product =>
              fieldOf(p, name)
            case other =>
              None
          }
        nextOpt match {
          case Some(next) => walk(next, tail)
          case None       => Left(DynaLensError(s"Field not found: '$name'"))
        }

      case IndexedField(name, idxOpt, _) :: tail =>
        // first get the collection under 'name'
        val collOpt =
          obj match {
            case m: Map[?, ?] =>
              m.asInstanceOf[Map[String, Any]].get(name)
            case p: Product =>
              fieldOf(p, name)
            case _ =>
              None
          }

        collOpt match {
          case None =>
            Left(DynaLensError(s"Field not found: '$name'"))

          case Some(coll) =>
            (coll, idxOpt) match {
              case (xs: Seq[?], Some(i)) =>
                val s = xs.asInstanceOf[Seq[Any]]
                if i >= 0 && i < s.length then walk(s(i), tail)
                else Left(DynaLensError(s"Index $i out of bounds for field '$name'"))

              // wildcard `[]` makes no sense for a scalar get; treat as error
              case (_: Seq[?], None) =>
                Left(DynaLensError(s"Wildcard index not allowed in value context for '$name[]'"))

              // You can add Array/List/Vector/etc. cases similarly if needed
              case _ =>
                Left(DynaLensError(s"Field '$name' is not indexable"))
            }
        }
    }
  }

  private def getFromTop(fullPath: String, ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] = {
    val parts = parsePath(fullPath)
    parts match {
      case Path.IndexedField(base, None, _) :: rest if ctx.contains(base) || ctx.contains(base + "[]") =>
        if rest.isEmpty then {
          // prefer the collection binding if present
          ctx.get(base + "[]") match {
            case Some((coll, _)) => ZIO.succeed(coll)
            case None =>
              ctx.get(base) match {
                case Some((v, _)) => ZIO.succeed(v)
                case None         => ZIO.fail(DynaLensError(s"Symbol '$base' not found in context"))
              }
          }
        } else {
          // tail exists → resolve off the current element binding as you already do
          ctx.get(base) match {
            case Some((v, Some(bl))) => bl.get(Path.partialPath(rest), v.asInstanceOf[bl.ThisT])
            case Some((v, None))     => ZIO.fromEither(walk(v, rest))
            case None                => ZIO.fail(DynaLensError(s"Symbol '$base' not found in context"))
          }
        }

      case _ =>
        // original top-lens / generic walk
        ctx.get("top") match {
          case Some((obj, Some(dynalens))) => dynalens.get(fullPath, obj.asInstanceOf[dynalens.ThisT])
          case Some((obj, None))           => ZIO.fromEither(walk(obj, parts))
          case _                           => ZIO.fail(DynaLensError(s"Missing 'top' in context for path '$fullPath'"))
        }
    }
  }

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] = {
    if !path.contains('.') && path.endsWith("[]") then {
      val k = path // exactly "items[]", "orders[]", ...
      if ctx.contains(k) then {
        val v = ctx(k)._1
        return ZIO.succeed(v)
      }
    }

    val parts = parsePath(path)

    parts match {
      // this.key  (first try "key", then derive from this=(k,v))
      case Field("this", _) :: Field("key", _) :: Nil =>
        ctx
          .valueOf("key")
          .map(ZIO.succeed(_))
          .getOrElse {
            ctx.valueOf("this") match {
              case Some(t: (Any, Any))                 => ZIO.succeed(t._1)
              case Some(me: java.util.Map.Entry[?, ?]) => ZIO.succeed(me.getKey)
              case _                                   => ZIO.fail(DynaLensError("Field not found: 'key'"))
            }
          }

      // this.value  (first try "value", then derive from this=(k,v))
      case Field("this", _) :: Field("value", _) :: Nil =>
        ctx
          .valueOf("value")
          .map(ZIO.succeed(_))
          .getOrElse {
            ctx.valueOf("this") match {
              case Some(t: (Any, Any))                 => ZIO.succeed(t._2)
              case Some(me: java.util.Map.Entry[?, ?]) => ZIO.succeed(me.getValue)
              case _                                   => ZIO.fail(DynaLensError("Field not found: 'value'"))
            }
          }

      // existing branch that handles generic `this.xxx` (navigate receiver)
      case Field("this", _) :: rest =>
        ctx.get("this") match {
          case Some((root, lensOpt)) =>
            lensOpt match {
              case Some(l) if rest.nonEmpty =>
                l.get(partialPath(rest), root.asInstanceOf[l.ThisT])
              case _ =>
                ZIO.fromEither(walk(root, rest))
            }
          case None =>
            ZIO.fail(DynaLensError("Use of 'this' with no receiver in scope"))
        }

      case (first @ IndexedField(name, idxOpt, _)) :: rest if ctx.contains(name) =>
        ctx.get(name) match {
          case Some((v, _)) =>
            v match {
              // --- value is a Seq: honor index if provided ---
              case seq: Seq[?] =>
                val s = seq.asInstanceOf[Seq[Any]]
                idxOpt match {
                  case Some(i) =>
                    if i >= 0 && i < s.length then {
                      val elem = s(i)
                      if rest.isEmpty then ZIO.succeed(elem)
                      else ZIO.fromEither(walk(elem, rest))
                    } else ZIO.fail(DynaLensError(s"Index $i out of bounds for '$name'"))
                  case None =>
                    // wildcard: return the whole seq (or walk further if needed)
                    if rest.isEmpty then ZIO.succeed(s)
                    else ZIO.fromEither(walk(s, rest))
                }

              // --- value is some Iterable (but not Seq): convert and do same logic ---
              case it: Iterable[?] =>
                val s = it.asInstanceOf[Iterable[Any]].toList
                idxOpt match {
                  case Some(i) =>
                    if i >= 0 && i < s.length then {
                      val elem = s(i)
                      if rest.isEmpty then ZIO.succeed(elem)
                      else ZIO.fromEither(walk(elem, rest))
                    } else ZIO.fail(DynaLensError(s"Index $i out of bounds for '$name'"))
                  case None =>
                    if rest.isEmpty then ZIO.succeed(s)
                    else ZIO.fromEither(walk(s, rest))
                }

              // --- value is NOT indexable ---
              case other =>
                idxOpt match {
                  case Some(i) =>
                    ZIO.fail(DynaLensError(s"Value bound to '$name' is not indexable (${other.getClass.getSimpleName}); cannot use [$i]"))
                  case None =>
                    // You used a wildcard on a scalar; previous behavior tried from top.
                    // Keep that fallback if you truly want "respect the declared path even if ctx binding is scalar".
                    getFromTop(path, ctx)
                }
            }

          case None =>
            // Not in ctx after all (unlikely since contains(name) was true), but keep the prior fallback:
            getFromTop(path, ctx)
        }

      case Path.IndexedField(name, None, _) :: rest if ctx.contains(name + "[]") =>
        ctx.get(name + "[]") match {
          case Some((coll: Iterable[?] @unchecked, _)) =>
            if rest.isEmpty then ZIO.succeed(coll) // "items[]" alone → whole list
            else
              ZIO.fail(
                DynaLensError(
                  s"Internal: wildcard index for '$name[]' with a remaining path ('${Path.partialPath(rest)}'); " +
                    s"this should be resolved via an element context, not the top lens."
                )
              )
          case Some((other, _)) =>
            ZIO.fail(DynaLensError(s"Expected iterable bound at '$name[]', found: ${other.getClass.getSimpleName}"))
          case None =>
            ZIO.fail(DynaLensError(s"Missing collection binding for '$name[]'"))
        }

      // existing: first segment bound in ctx (vals/loop symbol), using first.name ("items")
      case first :: rest if ctx.contains(first.name) =>
        ctx.get(first.name) match {
          // --- val/symbol binding (no lens) ---
          case Some((v, None)) =>
            first match {
              // x[2] ...
              case IndexedField(_, Some(i), _) =>
                v match {
                  case seq: Seq[?] =>
                    if i >= 0 && i < seq.length then {
                      val elem = seq(i)
                      if rest.isEmpty then ZIO.succeed(elem)
                      else ZIO.fromEither(walk(elem, rest)) // allow x[2].field, etc.
                    } else ZIO.fail(DynaLensError(s"Index $i out of bounds for '${first.name}'"))
                  case other =>
                    ZIO.fail(DynaLensError(s"Value bound to '${first.name}' is not indexable (${other.getClass.getSimpleName})"))
                }

              // wildcard `x[]` doesn’t make sense on a scalar get from ctx
              case IndexedField(_, None, _) =>
                ZIO.fail(DynaLensError(s"Wildcard index not allowed for '${first.name}[]' in value context"))

              // plain `x` or `x.something`
              case Field(_, _) =>
                if rest.isEmpty then ZIO.succeed(v)
                else ZIO.fromEither(walk(v, rest))
            }

          // --- lens-bound roots (unchanged) ---
          case Some((v, Some(boundLens))) =>
            if rest.isEmpty then ZIO.succeed(v)
            else boundLens.get(partialPath(rest), v.asInstanceOf[boundLens.ThisT])

          case None =>
            ZIO.fail(DynaLensError(s"Field ${first.name} not found in context"))
        }

      case _ =>
        getFromTop(path, ctx)
    }
  }
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

case class BooleanConstantFn(out: Boolean) extends BooleanFn:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    ZIO.succeed(out)

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
      case Nil            => false
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

// Helpers
private def toStr(v: Any, op: String): Either[DynaLensError, String] = v match {
  case null             => Right("")
  case None             => Right("")
  case s: String        => Right(s)
  case cs: CharSequence => Right(cs.toString)
  case _: Iterable[?]   => Left(DynaLensError(s"$op may only be applied to a single value, not an Iterable"))
  case _: Array[?]      => Left(DynaLensError(s"$op may only be applied to a single value, not an Array"))
  case p: Product       => Right(p.toString) // case classes/tuples become "a(b,c)"
  case other            => Right(other.toString) // numbers, booleans, etc.
}
private def toDbl(v: String, op: String): Either[DynaLensError, Double] =
  try Right(v.toDouble)
  catch {
    case _: NumberFormatException =>
      Left(DynaLensError(s"$op expected a numeric value for formatting, but got: '$v'"))
  }

case class StartsWithFn(receiver: Fn[Any], other: Fn[Any]) extends BooleanFn {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      aAny <- receiver.resolve(ctx)
      bAny <- other.resolve(ctx)
      aStr <- ZIO.fromEither(toStr(aAny, "startsWith receiver"))
      bStr <- ZIO.fromEither(toStr(bAny, "startsWith argument"))
    } yield aStr.startsWith(bStr)
}

case class EndsWithFn(receiver: Fn[Any], other: Fn[Any]) extends BooleanFn {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      aAny <- receiver.resolve(ctx)
      bAny <- other.resolve(ctx)
      aStr <- ZIO.fromEither(toStr(aAny, "endsWith receiver"))
      bStr <- ZIO.fromEither(toStr(bAny, "endsWith argument"))
    } yield aStr.endsWith(bStr)
}

case class ContainsFn(receiver: Fn[Any], needle: Fn[Any]) extends BooleanFn {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      hay <- receiver.resolve(ctx)
      res <- ContainsFn.containsDynamic(hay, needle, ctx)
    } yield res
}

object ContainsFn {

  private def containsDynamic(hay: Any, needle: Fn[Any], ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] = hay match {

    // ---- Option unwraps ----
    case null        => ZIO.succeed(false)
    case None        => ZIO.succeed(false)
    case Some(inner) => containsDynamic(inner, needle, ctx)

    // ---- String: substring ----
    case cs: CharSequence =>
      for {
        ndlAny <- needle.resolve(ctx)
      } yield cs.toString.contains(Option(ndlAny).fold("null")(_.toString))

    // ---- Map: key presence (needle evaluated once) ----
    case m: Map[?, ?] =>
      for {
        ndlVal <- needle.resolve(ctx)
      } yield m.asInstanceOf[Map[Any, Any]].contains(ndlVal)

    // ---- Iterable: supports predicate OR value check ----
    case it: Iterable[?] =>
      needle match {
        // Predicate case: evaluate per element with `this` bound
        case pred: BooleanFn =>
          ZIO
            .foreach(it.asInstanceOf[Iterable[Any]]) { elem =>
              withThisScoped(ctx, elem) {
                pred.resolve(ctx)
              }.either
            }
            .map(_.exists {
              case Right(true) => true
              case _           => false
            })

        // Value case: compute the target value once, then == compare
        case _ =>
          for {
            ndlVal <- needle.resolve(ctx)
          } yield it.asInstanceOf[Iterable[Any]].exists(_ == ndlVal)
      }

    // ---- Unsupported receiver types ----
    case other =>
      ZIO.fail(
        DynaLensError(
          s"contains() on ${other.getClass.getSimpleName} is not supported; " +
            s"expected String, Iterable, Map, or Option thereof"
        )
      )
  }
}

case class EqualsIgnoreCaseFn(receiver: Fn[Any], other: Fn[Any]) extends BooleanFn {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      aAny <- receiver.resolve(ctx)
      bAny <- other.resolve(ctx)
      aStr <- ZIO.fromEither(toStr(aAny, "equalsIgnoreCase receiver"))
      bStr <- ZIO.fromEither(toStr(bAny, "equalsIgnoreCase argument"))
    } yield aStr.equalsIgnoreCase(bStr)
}

case class MatchesRegexFn(receiver: Fn[Any], pattern: Fn[Any]) extends BooleanFn {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      aAny <- receiver.resolve(ctx)
      pAny <- pattern.resolve(ctx)
      aStr <- ZIO.fromEither(toStr(aAny, "matchesRegex receiver"))
      pStr <- ZIO.fromEither(toStr(pAny, "matchesRegex pattern"))
      // compile once; fail cleanly on bad regexes
      pat <- ZIO
        .attempt(java.util.regex.Pattern.compile(pStr))
        .mapError(e => DynaLensError(s"Invalid regex: ${e.getMessage}"))
    } yield pat.matcher(aStr).matches()
}

// --- Arithmetic  Functions ----

case class AbsFn(receiver: Fn[Any]) extends Fn[Any] {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext) =
    for {
      raw <- receiver.resolve(ctx)
      out <- raw match {
        case null                 => ZIO.fail(DynaLensError("abs() found null"))
        case x: java.lang.Byte    => ZIO.succeed((if x < 0 then (-x).toByte else x): Byte)
        case x: java.lang.Short   => ZIO.succeed(Math.abs(x.toInt).toShort)
        case x: java.lang.Integer => ZIO.succeed(Math.abs(x))
        case x: java.lang.Long    => ZIO.succeed(Math.abs(x))
        case x: java.lang.Float   => ZIO.succeed(Math.abs(x))
        case x: java.lang.Double  => ZIO.succeed(Math.abs(x))
        case other                => ZIO.fail(DynaLensError(s"abs() expects numeric, got ${other.getClass.getSimpleName}"))
      }
    } yield out
}

case class MinFn(receiver: Fn[Any]) extends Fn[Any] {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext) =
    for {
      raw <- receiver.resolve(ctx)
      box <- ZIO.fromEither(collect(raw, "min"))
      v = toPromotedVector(box)
      res <- box.kind match {
        case KDouble =>
          val vs = v.asInstanceOf[Vector[Double]]
          ZIO.succeed(vs.minOption.getOrElse(0.0))
        case KFloat =>
          val vs = v.asInstanceOf[Vector[Float]]
          ZIO.succeed(vs.minOption.getOrElse(0.0f))
        case KLong =>
          val vs = v.asInstanceOf[Vector[Long]]
          ZIO.succeed(vs.minOption.getOrElse(0L))
        case KInt =>
          val vs = v.asInstanceOf[Vector[Int]]
          ZIO.succeed(vs.minOption.getOrElse(0))
      }
    } yield res
}

case class MaxFn(receiver: Fn[Any]) extends Fn[Any] {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext) =
    for {
      raw <- receiver.resolve(ctx)
      box <- ZIO.fromEither(collect(raw, "max"))
      v = toPromotedVector(box)
      res <- box.kind match {
        case KDouble =>
          val vs = v.asInstanceOf[Vector[Double]]
          ZIO.succeed(vs.maxOption.getOrElse(0.0))
        case KFloat =>
          val vs = v.asInstanceOf[Vector[Float]]
          ZIO.succeed(vs.maxOption.getOrElse(0.0f))
        case KLong =>
          val vs = v.asInstanceOf[Vector[Long]]
          ZIO.succeed(vs.maxOption.getOrElse(0L))
        case KInt =>
          val vs = v.asInstanceOf[Vector[Int]]
          ZIO.succeed(vs.maxOption.getOrElse(0))
      }
    } yield res
}

case class SumFn(receiver: Fn[Any]) extends Fn[Any] {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext) =
    for {
      raw <- receiver.resolve(ctx)
      box <- ZIO.fromEither(collect(raw, "sum"))
      v = toPromotedVector(box)
      res <- box.kind match {
        case KDouble =>
          val vs = v.asInstanceOf[Vector[Double]]
          ZIO.succeed(vs.foldLeft(0.0)(_ + _))
        case KFloat =>
          val vs = v.asInstanceOf[Vector[Float]]
          ZIO.succeed(vs.foldLeft(0.0f)(_ + _))
        case KLong =>
          val vs = v.asInstanceOf[Vector[Long]]
          ZIO.succeed(vs.foldLeft(0L)(_ + _))
        case KInt =>
          val vs = v.asInstanceOf[Vector[Int]]
          ZIO.succeed(vs.foldLeft(0)(_ + _))
      }
    } yield res
}

case class AvgFn(receiver: Fn[Any]) extends Fn[Any] {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext) =
    for {
      raw <- receiver.resolve(ctx)
      box <- ZIO.fromEither(collect(raw, "avg"))
      v = toPromotedVector(box)
      res <- box.kind match {
        // Average is always Double (even for Int/Long/Float inputs)
        case KDouble =>
          val vs = v.asInstanceOf[Vector[Double]]
          ZIO.succeed(if vs.isEmpty then 0.0 else vs.sum / vs.size.toDouble)
        case KFloat =>
          val vs = v.asInstanceOf[Vector[Float]]
          ZIO.succeed(if vs.isEmpty then 0.0 else vs.foldLeft(0.0)(_ + _.toDouble) / vs.size.toDouble)
        case KLong =>
          val vs = v.asInstanceOf[Vector[Long]]
          ZIO.succeed(if vs.isEmpty then 0.0 else vs.foldLeft(0.0)(_ + _.toDouble) / vs.size.toDouble)
        case KInt =>
          val vs = v.asInstanceOf[Vector[Int]]
          ZIO.succeed(if vs.isEmpty then 0.0 else vs.foldLeft(0.0)(_ + _.toDouble) / vs.size.toDouble)
      }
    } yield res
}

case class MedianFn(receiver: Fn[Any]) extends Fn[Any] {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext) =
    for {
      raw <- receiver.resolve(ctx)
      box <- ZIO.fromEither(collect(raw, "median"))
      v = toPromotedVector(box)
      res <- box.kind match {
        // Median returns Double (common analytics convention)
        case KDouble =>
          val s = v.asInstanceOf[Vector[Double]].sorted
          ZIO.succeed {
            val n = s.length
            if n == 0 then 0.0
            else if (n & 1) == 1 then s(n / 2)
            else (s(n / 2 - 1) + s(n / 2)) / 2.0
          }
        case KFloat =>
          val s = v.asInstanceOf[Vector[Float]].sorted
          ZIO.succeed {
            val n = s.length
            if n == 0 then 0.0
            else if (n & 1) == 1 then s(n / 2).toDouble
            else (s(n / 2 - 1) + s(n / 2)).toDouble / 2.0
          }
        case KLong =>
          val s = v.asInstanceOf[Vector[Long]].sorted
          ZIO.succeed {
            val n = s.length
            if n == 0 then 0.0
            else if (n & 1) == 1 then s(n / 2).toDouble
            else (s(n / 2 - 1) + s(n / 2)).toDouble / 2.0
          }
        case KInt =>
          val s = v.asInstanceOf[Vector[Int]].sorted
          ZIO.succeed {
            val n = s.length
            if n == 0 then 0.0
            else if (n & 1) == 1 then s(n / 2).toDouble
            else (s(n / 2 - 1) + s(n / 2)).toDouble / 2.0
          }
      }
    } yield res
}

case class NegateFn(
                     receiver: Fn[Any]
) extends Fn[Any]:
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      t <- receiver.resolve(ctx)
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

case class TrimFn(receiver: Fn[Any]) extends Fn[String] {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    receiver.resolve(ctx).flatMap { v =>
      ZIO.fromEither(toStr(v, "trim()")).map(_.trim)
    }
}

case class ToLowerFn(receiver: Fn[Any]) extends Fn[String] {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    receiver.resolve(ctx).flatMap { v =>
      ZIO.fromEither(toStr(v, "toLowerCase()")).map(_.toLowerCase)
    }
}

case class ToUpperFn(receiver: Fn[Any]) extends Fn[String] {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    receiver.resolve(ctx).flatMap { v =>
      ZIO.fromEither(toStr(v, "toUpperCase()")).map(_.toUpperCase)
    }
}

case class ConcatFn(parts: List[Fn[Any]]) extends Fn[String]:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    ZIO
      .foreach(parts)(_.resolve(ctx))
      .map(_.map {
        case null => ""
        case s    => s.toString
      }.mkString)

case class InterpolateFn(receiver: Fn[Any], variables: Map[String, Fn[Any]]) extends Fn[String] {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    for {
      templStr <- receiver.resolve(ctx).flatMap { v =>
        ZIO.fromEither(toStr(v, "interpolate(template)"))
      }

      varsToUse =
        if variables.isEmpty then TemplateUtils.extractVariables(templStr).map(v => v -> GetFn(v)).toMap
        else variables

      resolvedVars <- ZIO.foreach(varsToUse.toList) { case (k, fn) =>
        fn.resolve(ctx).flatMap { v =>
          ZIO.fromEither(toStr(v, s"interpolate($k)")).map(str => k -> str)
        }
      }

      result <- ZIO
        .attempt {
          val varMap = resolvedVars.toMap

          val pattern =
            """\{([a-zA-Z_][a-zA-Z0-9_]*(?:\[[0-9]+\]|\.[a-zA-Z_][a-zA-Z0-9_]*(?:\[[0-9]+\])?)*)(?:%([^}:]+))?(?::([^}]+))?\}""".r

          pattern.replaceAllIn(
            templStr,
            m => {
              val varName = m.group(1)
              val fmtOpt = Option(m.group(2)) // e.g. "0.2f"
              val dfltOpt = Option(m.group(3)) // fallback string
              val valueOpt = varMap.get(varName).filter(_.nonEmpty)

              (valueOpt, fmtOpt) match {
                case (Some(value), Some(fmt)) =>
                  toDbl(value, s"interpolate($varName)").fold(
                    _ => value,
                    d => String.format(Locale.US, s"%$fmt", Double.box(d))
                  )

                case (Some(value), None) =>
                  value

                case (None, Some(fmt)) =>
                  dfltOpt match {
                    case Some(dflt) =>
                      toDbl(dflt, s"interpolate(default for $varName)").fold(
                        _ => dflt,
                        d => String.format(Locale.US, s"%$fmt", Double.box(d))
                      )
                    case None => ""
                  }

                case (None, None) =>
                  dfltOpt.getOrElse("")
              }
            }
          )
        }
        .mapError(th => DynaLensError(s"interpolate() failed: ${th.getMessage}"))
    } yield result
}

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

case class SubstringFn(receiver: Fn[Any], start: Fn[Int], end: Option[Fn[Int]]) extends Fn[String]:
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    for {
      s <- receiver.resolve(ctx).map {
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

case class ReplaceFn(receiver: Fn[Any], target: Fn[Any], replacement: Fn[Any]) extends Fn[String]:
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    for {
      str <- receiver.resolve(ctx).map(v => if v == null then "" else v.toString)
      t <- target.resolve(ctx).map(v => if v == null then "" else v.toString)
      r <- replacement.resolve(ctx).map(v => if v == null then "" else v.toString)
    } yield str.replace(t, r)

// --- Option Functions ----

case class ElseFn(primary: Fn[Any], fallback: Fn[Any]) extends Fn[Any] {
  override def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    primary.resolve(ctx).flatMap {
      case opt: Option[?] =>
        opt match {
          case Some(v) => ZIO.succeed(v) // pass-through value
          case None    => fallback.resolve(ctx) // only None triggers fallback
        }
      case v =>
        ZIO.succeed(v) // non-Option: pass-through
    }
}

// --- Map Functions --- (except ContainsFn, which is multipurpose... given in another section)

case class KeysFn(receiver: Fn[Any]) extends Fn[List[Any]]:
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, List[Any]] =
    for {
      mAny <- receiver.resolve(ctx)
      result <- mAny match {
        case null               => ZIO.succeed(Nil) // safe: no keys
        case m: Map[?, ?]       => ZIO.succeed(m.keys.toList)
        case Some(m: Map[?, ?]) => ZIO.succeed(m.keys.toList) // option-wrapped
        case None               => ZIO.succeed(Nil)
        case other =>
          ZIO.fail(DynaLensError(s"keys() can only be used on Map or Option[Map], but found ${other.getClass.getName}"))
      }
    } yield result

case class ValuesFn(receiver: Fn[Any]) extends Fn[List[Any]]:
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, List[Any]] =
    for {
      mAny <- receiver.resolve(ctx)
      result <- mAny match {
        case null               => ZIO.succeed(Nil)
        case m: Map[?, ?]       => ZIO.succeed(m.values.toList)
        case Some(m: Map[?, ?]) => ZIO.succeed(m.values.toList)
        case None               => ZIO.succeed(Nil)
        case other =>
          ZIO.fail(DynaLensError(s"values() can only be used on Map or Option[Map], but found ${other.getClass.getName}"))
      }
    } yield result

case class MapGetFn(receiver: Fn[Any], key: Fn[Any]) extends Fn[Any] {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext) =
    for {
      m <- receiver.resolve(ctx)
      k <- key.resolve(ctx)
      v <- m match {
        case mm: Map[?, ?] @unchecked =>
          mm.asInstanceOf[Map[Any, Any]].get(k) match
            case Some(v) => ZIO.succeed(v)
            case None    => ZIO.fail(DynaLensError(s"get(): key '$k' not found"))
        case Some(mm: Map[?, ?] @unchecked) =>
          mm.asInstanceOf[Map[Any, Any]].get(k) match
            case Some(v) => ZIO.succeed(v)
            case None    => ZIO.fail(DynaLensError(s"get(): key '$k' not found"))
        case other =>
          ZIO.fail(DynaLensError(s"get(): receiver is not a Map (got ${other.getClass.getSimpleName})"))
      }
    } yield v
}

case class Tuple2Fn(_1: Fn[Any], _2: Fn[Any]) extends Fn[Any] {
  def resolve(ctx: DynaContext) =
    for {
      a <- _1.resolve(ctx)
      b <- _2.resolve(ctx)
    } yield (a, b)
}

// --- Collection (Iterable) Functions ----

// Wrap any Fn that produces a collection; pick element at fixed index
case class IndexFn(receiver: Fn[Any], index: Int) extends Fn[Any] {
  override val recv: Option[Fn[?]] = Some(receiver)
  private def toList(v: Any): Either[DynaLensError, List[Any]] = v match {
    case null                  => Left(DynaLensError(s"Cannot index into null"))
    case None                  => Left(DynaLensError(s"Cannot index into None"))
    case Some(s: Seq[?])       => Right(s.asInstanceOf[Seq[Any]].toList)
    case Some(it: Iterable[?]) => Right(it.asInstanceOf[Iterable[Any]].toList)
    case s: Seq[?]             => Right(s.asInstanceOf[Seq[Any]].toList)
    case it: Iterable[?]       => Right(it.asInstanceOf[Iterable[Any]].toList)
    case other                 => Left(DynaLensError(s"Indexing requires a collection, got: ${other.getClass.getSimpleName}"))
  }

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      raw <- receiver.resolve(ctx)
      list <- ZIO.fromEither(toList(raw))
      elem <- list.lift(index) match {
        case Some(e) => ZIO.succeed(e)
        case None    => ZIO.fail(DynaLensError(s"Index $index out of bounds"))
      }
    } yield elem
}

case class LoopFn(predicate: Fn[Any]) extends Fn[Any] {
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ctx.get("this") match {
      case Some((v: Iterable[?], lens)) =>
        ZIO
          .foreach(v) { item =>
            val localCtx = ctx.updatedWith("this", (item, lens))
            predicate.resolve(localCtx)
          }
          .map(_.toList) // replace with same type if you need
      case _ =>
        ZIO.fail(DynaLensError("LoopFn resolve() missing root object in context"))
    }
}

case class FilterFn(receiver: Fn[Any], predicate: BooleanFn) extends Fn[Any] {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      raw <- receiver.resolve(ctx)
      seq <- ZIO.fromEither(asSeq(raw, "filter"))
      kept <- ZIO.foreach(seq) { e =>
        predicate.resolve(withElemCtx(e, ctx)).map {
          case true  => Some(e)
          case false => None
        }
      }
    } yield kept.flatten
}

case class SortAscFn(receiver: Fn[Any], keyPath: Option[String]) extends SortFn:
  val asc: Boolean = true

case class SortDescFn(receiver: Fn[Any], keyPath: Option[String]) extends SortFn:
  val asc: Boolean = false

trait SortFn extends Fn[Any] {
  val receiver: Fn[Any]
  val keyPath: Option[String]
  val asc: Boolean
  override val recv: Option[Fn[?]] = Some(receiver)
  private inline def opName: String = if asc then "sortAsc" else "sortDesc"

  private def asSeq(v: Any): Either[DynaLensError, List[Any]] = v match {
    case null           => Right(Nil)
    case s: Seq[?]      => Right(s.asInstanceOf[Seq[Any]].toList)
    case i: Iterable[?] => Right(i.asInstanceOf[Iterable[Any]].toList)
    case o: Option[?] =>
      o match {
        case Some(s: Seq[?])      => Right(s.asInstanceOf[Seq[Any]].toList)
        case Some(i: Iterable[?]) => Right(i.asInstanceOf[Iterable[Any]].toList)
        case Some(x)              => Left(DynaLensError(s"$opName() expects an Iterable; got ${x.getClass.getSimpleName} in Some(...)"))
        case None                 => Right(Nil)
      }
    case other =>
      Left(DynaLensError(s"$opName() may only be applied to Iterable types, but got: ${other.getClass.getSimpleName}"))
  }

  implicit private val cmpOrd: Ordering[Comparable[Any]] =
    (a: Comparable[Any], b: Comparable[Any]) => a.compareTo(b)

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      raw <- receiver.resolve(ctx)
      seq <- ZIO.fromEither(asSeq(raw))
      sorted <- keyPath match {
        case Some(pth) =>
          // sort by key extracted from each element via `this`
          for {
            pairs <- ZIO.foreach(seq) { elem =>
              GetFn(pth).resolve(withElemCtx(elem, ctx)).flatMap {
                case c: Comparable[?] =>
                  ZIO.succeed((elem, c.asInstanceOf[Comparable[Any]]))
                case other =>
                  ZIO.fail(DynaLensError(s"$opName($pth): key '$other' is not Comparable"))
              }
            }
          } yield {
            val s = pairs.sortBy(_._2)
            if asc then s.map(_._1) else s.reverse.map(_._1)
          }

        case None =>
          // sort elements directly
          ZIO
            .foreach(seq) {
              case c: Comparable[?] => ZIO.succeed(c.asInstanceOf[Comparable[Any]])
              case other            => ZIO.fail(DynaLensError(s"$opName(): item '$other' is not Comparable"))
            }
            .map { comps =>
              val s = comps.sorted
              if asc then s.toList else s.reverse.toList
            }
      }
    } yield sorted
}

case class DistinctFn(receiver: Fn[Any], fieldPath: Option[String]) extends Fn[Any] {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      raw <- receiver.resolve(ctx)
      items <- ZIO.fromEither(asSeq(raw, "distinct")) // or asIterable; returns Seq[Any]

      pairs <- fieldPath match {
        case None =>
          // distinct by whole element (preserve first occurrence)
          ZIO.succeed(items.map(x => (x, x)))

        case Some(rawKey) =>
          // accept both "number" and "this.number"
          val keyPath =
            if rawKey.startsWith("this.") then rawKey
            else s"this.$rawKey"

          ZIO.foreach(items) { item =>
            val itemCtx = withElemCtx(item, ctx) // bind this=item
            GetFn(keyPath).resolve(itemCtx).map(k => (k, item))
          }
      }

      // preserve first occurrence of each key (stable)
      deduped = {
        val seen = scala.collection.mutable.HashSet[Any]()
        val buf = scala.collection.mutable.ArrayBuffer[Any]()
        pairs.foreach { case (k, v) =>
          if !seen.contains(k) then { seen += k; buf += v }
        }
        buf.toList
      }
    } yield deduped
}

case class LimitFn(receiver: Fn[Any], count: Int) extends Fn[Any] {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      raw <- receiver.resolve(ctx)
      seq <- ZIO.fromEither(asSeq(raw, "limit"))
    } yield seq.take(count)
}

case class ReverseFn(receiver: Fn[Any]) extends Fn[Any] {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      raw <- receiver.resolve(ctx)
      seq <- ZIO.fromEither(asSeq(raw, "reverse"))
    } yield seq.reverse
}

case class CleanFn(receiver: Fn[Any]) extends Fn[Any] {
  override val recv: Option[Fn[?]] = Some(receiver)
  private def truthy(v: Any): Boolean = v match {
    case null            => false
    case None            => false
    case _: Unit         => false
    case s: CharSequence => s.toString.trim.nonEmpty
    case it: Iterable[?] => it.iterator.hasNext // keep non-empty collections
    case _               => true
  }

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      raw <- receiver.resolve(ctx)
      seq <- ZIO.fromEither(asSeq(raw, "clean"))
    } yield seq.iterator.filter(truthy).toList
}

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

case class LengthFn(receiver: Fn[Any]) extends Fn[Int]:
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Int] =
    receiver.resolve(ctx).map {
      case null => 0
      case c if c.isInstanceOf[Seq[?]] =>
        c.asInstanceOf[Seq[?]].length
      case s =>
        s.toString.length
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

case class FormatDateFn(receiver: Fn[Any], pattern: Fn[String]) extends Fn[Any] {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      d <- receiver.resolve(ctx)
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

case class ParseDateFn(receiver: Fn[Any], pattern: Fn[String]) extends Fn[Any] {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      s <- receiver.resolve(ctx)
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

case class NowFn() extends Fn[Any] {
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ZIO.succeed(new java.util.Date())
}

case class UUIDFn() extends Fn[Any] {
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ZIO.succeed(java.util.UUID.randomUUID())
}

// --- Case Function ----

case class CaseWhenFn(
    receiver: Fn[Any],
    cases: Vector[(Any, Fn[Any])], // pattern literal -> RHS expr
    default: Option[Fn[Any]],
    permissive: Boolean = false
) extends Fn[Any] {
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      v <- receiver.resolve(ctx)
      out <-
        // first match by Scala “==”
        cases
          .collectFirst { case (p, rhs) if v == p => rhs }
          .map(_.resolve(ctx))
          .getOrElse {
            default match {
              case Some(df) => df.resolve(ctx)
              case None =>
                if permissive then ZIO.succeed(v)
                else ZIO.fail(DynaLensError(s"No case matched for value: $v"))
            }
          }
    } yield out
}
