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
import FnUtils.*

import scala.annotation.tailrec

trait Fn[R]:
  def recv: Option[Fn[?]] = None
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, R]
  def as[T]: Fn[T] = this.asInstanceOf[Fn[T]]
  def methodName: String = this.getClass.getSimpleName.stripSuffix("Fn").decapitalize
  val isOptional: Boolean = false
  def args: List[Fn[Any]] = Nil

  /** Default: no sub-nodes. Override in composite nodes. */
  def children: List[Fn[?]] = args

  /** Rebuild with new children (in the same order as `children`). */
  def rebuild(kids: List[Fn[?]]): Fn[R]

  /** Walk the tree and patch every `GetFn("this")` with the given receiver. */
  def withReceiver(r: Fn[?]): Fn[R] =
    this match
      case g: GetFn =>
        // Attach receiver to any GetFn, even if path doesn't start with "this"
        g.copy(recv = Some(r)).asInstanceOf[Fn[R]]

      case other if other.children.nonEmpty =>
        other.rebuild(other.children.map(_.withReceiver(r)))

      case other => other


trait ReceiverUnaryFn[R] extends Fn[R]:
  def receiver: Fn[Any]
  override def args: List[Fn[Any]] = List(receiver)
  override val recv: Option[Fn[?]] = Some(receiver)
  override val isOptional: Boolean = receiver.isOptional

trait ReceiverBinaryFn[R] extends Fn[R]:
  def receiver: Fn[Any]
  def other: Fn[Any]
  override def args: List[Fn[Any]] = List(receiver, other)
  override val recv: Option[Fn[?]] = Some(receiver)
  override val isOptional: Boolean = receiver.isOptional || other.isOptional

trait OperandUnaryFn[R] extends Fn[R]:
  def operand: Fn[Any]
  override def args: List[Fn[Any]] = List(operand)
  override val recv: Option[Fn[?]] = None
  override val isOptional: Boolean = operand.isOptional

trait OperandBinaryFn[R] extends Fn[R]:
  def left: Fn[Any]
  def right: Fn[Any]
  override def args: List[Fn[Any]] = List(left, right)
  override val recv: Option[Fn[?]] = None
  override val isOptional: Boolean = left.isOptional || right.isOptional

trait NAryFn[R] extends Fn[R]:
  def parts: List[Fn[Any]]
  override def args: List[Fn[Any]] = parts
  override def children: List[Fn[?]] = args
// usually no recv by default; override in specific n-ary “method-like” nodes if you want


// Marker trait for boolean-returning functions
trait BooleanFn extends Fn[Boolean]:
  override def withReceiver(r: Fn[?]): BooleanFn =
    super.withReceiver(r).asInstanceOf[BooleanFn]


case object NoneFn extends Fn[Any]:
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, None.type] =
    ZIO.succeed(None)

// --- Core Functions ----

case class ConstantFn[R](out: R) extends Fn[R]:
  override def rebuild(kids: List[Fn[?]]): Fn[R] = this
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, R] =
    ZIO.succeed(out)


case class GetFn(
                  path: String,
                  override val isOptional: Boolean,
                  override val recv: Option[Fn[?]] = None) extends Fn[Any] {

  import Path.* // for parsePath/PathElement/Field/IndexedField/partialPath

  override def withReceiver(r: Fn[?]): Fn[Any] =
    copy(recv = Some(r)).asInstanceOf[Fn[Any]]
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this

  // soften only "missing" style failures when this path is optional
  private inline def softMissing[A](zio: ZIO[_BiMapRegistry, DynaLensError, A]): ZIO[_BiMapRegistry, DynaLensError, Any] =
    if (isOptional)
      zio.either.flatMap {
        case Right(v) => ZIO.succeed(v)
        case Left(_) => ZIO.succeed(None) // return None for missing optional
      }
    else zio.asInstanceOf[ZIO[_BiMapRegistry, DynaLensError, Any]]

  // Generic walker that can traverse case classes, Maps and (indexed) collections
  @tailrec
  private def walk(obj: Any, parts: List[PathElement]): Either[DynaLensError, Any] = {
    def fieldOf(p: Product, name: String): Option[Any] = {
      val names = p.productElementNames.iterator
      var i = 0
      while (names.hasNext) {
        if (names.next() == name) return Some(p.productElement(i))
        i += 1
      }
      None
    }

    parts match {
      case Nil => Right(obj)

      case Field(name, _) :: tail =>
        val nextOpt =
          obj match {
            case m: Map[?, ?] => m.asInstanceOf[Map[String, Any]].get(name)
            case p: Product => fieldOf(p, name)
            case _ => None
          }
        nextOpt match {
          case Some(next) => walk(next, tail)
          case None => Left(DynaLensError(s"Field not found: '$name'"))
        }

      case IndexedField(name, idxOpt, _) :: tail =>
        val collOpt =
          obj match {
            case m: Map[?, ?] => m.asInstanceOf[Map[String, Any]].get(name)
            case p: Product => fieldOf(p, name)
            case _ => None
          }

        collOpt match {
          case None =>
            Left(DynaLensError(s"Field not found: '$name'"))

          case Some(coll) =>
            (coll, idxOpt) match {
              case (xs: Seq[?], Some(i)) =>
                val s = xs.asInstanceOf[Seq[Any]]
                if (i >= 0 && i < s.length) walk(s(i), tail)
                else Left(DynaLensError(s"Index $i out of bounds for field '$name'"))

              case (_: Seq[?], None) =>
                Left(DynaLensError(s"Wildcard index not allowed in value context for '$name[]'"))

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
        if (rest.isEmpty) {
          ctx.get(base + "[]") match {
            case Some((coll, _)) => ZIO.succeed(coll)
            case None =>
              ctx.get(base) match {
                case Some((v, _)) => ZIO.succeed(v)
                case None => if (isOptional) ZIO.succeed(None) else ZIO.fail(DynaLensError(s"Symbol '$base' not found in context"))
              }
          }
        } else {
          ctx.get(base) match {
            case Some((v, Some(bl))) => softMissing(bl.get(Path.partialPath(rest), v.asInstanceOf[bl.ThisT])) // soften missing
            case Some((v, None)) => softMissing(ZIO.fromEither(walk(v, rest))) // soften missing
            case None => if (isOptional) ZIO.succeed(None) else ZIO.fail(DynaLensError(s"Symbol '$base' not found in context"))
          }
        }

      case _ =>
        ctx.get("top") match {
          case Some((obj, Some(dynalens))) =>
            // soften missing from lens
            softMissing(dynalens.get(fullPath, obj.asInstanceOf[dynalens.ThisT]))

          case Some((obj, None)) =>
            // soften missing from raw walk
            softMissing(ZIO.fromEither(walk(obj, parts)))

          case _ =>
            if (isOptional) ZIO.succeed(None)
            else ZIO.fail(DynaLensError(s"Missing 'top' in context for path '$fullPath'"))
        }
    }
  }

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] = {
    if (!path.contains('.') && path.endsWith("[]")) {
      val k = path // exactly "items[]", "orders[]", ...
      if (ctx.contains(k)) {
        val v = ctx(k)._1
        return ZIO.succeed(v)
      }
    }

    val parts = parsePath(path)

    parts match {
      // this.key  (first try "key", then derive from this=(k,v))
      case Field("this", _) :: Field("key", _) :: Nil =>
        ctx.valueOf("key").map(ZIO.succeed(_)).getOrElse {
          ctx.valueOf("this") match {
            case Some(t: (Any, Any)) => ZIO.succeed(t._1)
            case Some(me: java.util.Map.Entry[?, ?]) => ZIO.succeed(me.getKey)
            case _ => if (isOptional) ZIO.succeed(None) else ZIO.fail(DynaLensError("Field not found: 'key'"))
          }
        }

      // this.value  (first try "value", then derive from this=(k,v))
      case Field("this", _) :: Field("value", _) :: Nil =>
        ctx.valueOf("value").map(ZIO.succeed(_)).getOrElse {
          ctx.valueOf("this") match {
            case Some(t: (Any, Any)) => ZIO.succeed(t._2)
            case Some(me: java.util.Map.Entry[?, ?]) => ZIO.succeed(me.getValue)
            case _ => if (isOptional) ZIO.succeed(None) else ZIO.fail(DynaLensError("Field not found: 'value'"))
          }
        }

      // generic `this.xxx`
      case Field("this", _) :: rest =>
        ctx.get("this") match {
          case Some((root, Some(l))) if rest.nonEmpty =>
            softMissing(l.get(partialPath(rest), root.asInstanceOf[l.ThisT])) // soften missing

          case Some((root, _)) =>
            softMissing(ZIO.fromEither(walk(root, rest))) // soften missing

          case None =>
            // "this" missing is a programming/scope error → hard fail
            ZIO.fail(DynaLensError("Use of 'this' with no receiver in scope"))
        }

      case (first@IndexedField(name, idxOpt, _)) :: rest if ctx.contains(name) =>
        ctx.get(name) match {
          case Some((v, _)) =>
            v match {
              case seq: Seq[?] =>
                val s = seq.asInstanceOf[Seq[Any]]
                idxOpt match {
                  case Some(i) =>
                    if (i >= 0 && i < s.length) {
                      val elem = s(i)
                      if (rest.isEmpty) ZIO.succeed(elem)
                      else softMissing(ZIO.fromEither(walk(elem, rest))) // soften missing on tail
                    } else ZIO.fail(DynaLensError(s"Index $i out of bounds for '$name'"))
                  case None =>
                    if (rest.isEmpty) ZIO.succeed(s)
                    else softMissing(ZIO.fromEither(walk(s, rest))) // soften missing on tail
                }

              case it: Iterable[?] =>
                val s = it.asInstanceOf[Iterable[Any]].toList
                idxOpt match {
                  case Some(i) =>
                    if (i >= 0 && i < s.length) {
                      val elem = s(i)
                      if (rest.isEmpty) ZIO.succeed(elem)
                      else softMissing(ZIO.fromEither(walk(elem, rest))) // soften missing on tail
                    } else ZIO.fail(DynaLensError(s"Index $i out of bounds for '$name'"))
                  case None =>
                    if (rest.isEmpty) ZIO.succeed(s)
                    else softMissing(ZIO.fromEither(walk(s, rest))) // soften missing on tail
                }

              case other =>
                idxOpt match {
                  case Some(i) =>
                    ZIO.fail(DynaLensError(s"Value bound to '$name' is not indexable (${other.getClass.getSimpleName}); cannot use [$i]"))
                  case None =>
                    getFromTop(path, ctx) // this already softens appropriately
                }
            }

          case None =>
            getFromTop(path, ctx) // already softens
        }

      case Path.IndexedField(name, None, _) :: rest if ctx.contains(name + "[]") =>
        ctx.get(name + "[]") match {
          case Some((coll: Iterable[?] @unchecked, _)) =>
            if (rest.isEmpty) ZIO.succeed(coll)
            else ZIO.fail(
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

      case first :: rest if ctx.contains(first.name) =>
        ctx.get(first.name) match {
          case Some((v, None)) =>
            first match {
              case IndexedField(_, Some(i), _) =>
                v match {
                  case seq: Seq[?] =>
                    if (i >= 0 && i < seq.length) {
                      val elem = seq(i)
                      if (rest.isEmpty) ZIO.succeed(elem)
                      else softMissing(ZIO.fromEither(walk(elem, rest)))
                    } else ZIO.fail(DynaLensError(s"Index $i out of bounds for '${first.name}'"))
                  case other =>
                    ZIO.fail(DynaLensError(s"Value bound to '${first.name}' is not indexable (${other.getClass.getSimpleName})"))
                }

              case IndexedField(_, None, _) =>
                ZIO.fail(DynaLensError(s"Wildcard index not allowed for '${first.name}[]' in value context"))

              case Field(_, _) =>
                if (rest.isEmpty) ZIO.succeed(v)
                else softMissing(ZIO.fromEither(walk(v, rest)))
            }

          case Some((v, Some(boundLens))) =>
            if (rest.isEmpty) ZIO.succeed(v)
            else softMissing(boundLens.get(partialPath(rest), v.asInstanceOf[boundLens.ThisT]))

          case None =>
            ZIO.fail(DynaLensError(s"Field ${first.name} not found in context"))
        }

      //
      // fallback to current element (`this`) when present <<<<<<
      // If no top-level hit for the first segment but we *do* have a `this`
      // (i.e., we’re inside a per-element context like filter/map),
      // then resolve the whole path relative to that element, with soft-missing.
      case segs @ (first :: _)
        if ctx.get("this").nonEmpty &&
          this.recv.nonEmpty &&
          !ctx.contains(first.name) =>
        println(s"--> In new segs case (relative-only): $segs")
        ctx.get("this") match {
          case Some((root, Some(lens))) =>
            softMissing(lens.get(Path.partialPath(segs), root.asInstanceOf[lens.ThisT]))
          case Some((root, None)) =>
            softMissing(ZIO.fromEither(walk(root, segs)))
          case None =>
            ZIO.fail(DynaLensError("Use of element-relative path but no 'this' in context"))
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
  override def args: List[Fn[Any]] = List(condition.asInstanceOf[Fn[Any]], thenFn.asInstanceOf[Fn[Any]], elseFn.asInstanceOf[Fn[Any]])
  override def children: List[Fn[?]] = args
  override def rebuild(kids: List[Fn[?]]): Fn[R] =
    copy(
      condition = kids(0).asInstanceOf[Fn[Boolean]],
      thenFn = kids(1).asInstanceOf[Fn[R]],
      elseFn = kids(2).asInstanceOf[Fn[R]]
    )
  override val isOptional: Boolean =
    thenFn.isOptional || elseFn.isOptional
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, R] =
    condition.resolve(ctx).flatMap {
      case true  => thenFn.resolve(ctx)
      case false => elseFn.resolve(ctx)
    }

case class BlockFn[R](
    statements: Seq[Statement],
    finalFn: Fn[R]
) extends Fn[R]:
  override def args: List[Fn[Any]] = List(finalFn.asInstanceOf[Fn[Any]])
  override def children: List[Fn[?]] = List(finalFn)
  override def rebuild(kids: List[Fn[?]]): Fn[R] =
    copy(
      statements = this.statements,                 // statements aren’t rebuilt here
      finalFn    = kids.head.asInstanceOf[Fn[R]]    // only rebuild finalFn
    )
  override val isOptional: Boolean = finalFn.isOptional
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, R] =
    val prepped = statements.foldLeft(
      ZIO.succeed(ctx): ZIO[_BiMapRegistry, DynaLensError, DynaContext]
    )((acc, stmt) => acc.flatMap(stmt.resolve))
    prepped.flatMap(finalFn.resolve)

// --- Boolean Functions ----

case class BooleanConstantFn(out: Boolean) extends BooleanFn:
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] = this
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    ZIO.succeed(out)

private def numericEq(l: Any, r: Any): Option[Boolean] = (l, r) match
  case (a: Number, b: Number) => Some(a.doubleValue() == b.doubleValue())
  case _ => None

private def numericCompare(l: Any, r: Any)(cmp: (Double, Double) => Boolean): Option[Boolean] =
  (l, r) match
    case (a: Number, b: Number) => Some(cmp(a.doubleValue(), b.doubleValue()))
    case _ => None

case class EqualFn(left: Fn[Any], right: Fn[Any]) extends BooleanFn with OperandBinaryFn[Boolean]:
  override val methodName: String = "=="
  override def rebuild(kids: List[Fn[?]]): EqualFn =
    copy(
      left = kids.head.asInstanceOf[Fn[Any]],
      right = kids(1).asInstanceOf[Fn[Any]]
    )
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match
        case (ls: String, rs: String) => ZIO.succeed(ls == rs)
        case _ =>
          numericEq(l, r) match
            case Some(eq) => ZIO.succeed(eq)
            case None =>
              // final fallback: same type, rely on standard equality
              if l != null && r != null && l.getClass == r.getClass then
                ZIO.succeed(l == r)
              else
                ZIO.fail(DynaLensError(s"Cannot compare types: ${l.getClass}, ${r.getClass}"))
    } yield result

case class NotEqualFn(left: Fn[Any], right: Fn[Any]) extends BooleanFn with OperandBinaryFn[Boolean]:
  override val methodName: String = "!="
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(
      left = kids.head.asInstanceOf[Fn[Any]],
      right = kids(1).asInstanceOf[Fn[Any]]
    )
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match
        case (ls: String, rs: String) => ZIO.succeed(ls != rs)
        case _ =>
          numericEq(l, r) match
            case Some(eq) => ZIO.succeed(!eq)
            case None =>
              // final fallback: same type, rely on standard equality
              if l != null && r != null && l.getClass == r.getClass then
                ZIO.succeed(l != r)
              else
                ZIO.fail(DynaLensError(s"Cannot compare types: ${l.getClass}, ${r.getClass}"))
    } yield result

case class GreaterThanFn(left: Fn[Any], right: Fn[Any]) extends BooleanFn with OperandBinaryFn[Boolean]:
  override val methodName: String = ">"
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(
      left = kids.head.asInstanceOf[Fn[Any]],
      right = kids(1).asInstanceOf[Fn[Any]]
    )
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- numericCompare(l, r)(_ > _) match
        case Some(b) => ZIO.succeed(b)
        case None    => ZIO.fail(DynaLensError(s"Cannot perform '>' on types: ${l.getClass}, ${r.getClass}"))
    yield result

case class LessThanFn(left: Fn[Any], right: Fn[Any]) extends BooleanFn with OperandBinaryFn[Boolean]:
  override val methodName: String = "<"
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(
      left = kids.head.asInstanceOf[Fn[Any]],
      right = kids(1).asInstanceOf[Fn[Any]]
    )
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- numericCompare(l, r)(_ < _) match
        case Some(b) => ZIO.succeed(b)
        case None    => ZIO.fail(DynaLensError(s"Cannot perform '<' on types: ${l.getClass}, ${r.getClass}"))
    yield result

case class GreaterThanOrEqualFn(left: Fn[Any], right: Fn[Any]) extends BooleanFn with OperandBinaryFn[Boolean]:
  override val methodName: String = ">="
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(
      left = kids.head.asInstanceOf[Fn[Any]],
      right = kids(1).asInstanceOf[Fn[Any]]
    )
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- numericCompare(l, r)(_ >= _) match
        case Some(b) => ZIO.succeed(b)
        case None => ZIO.fail(DynaLensError(s"Cannot perform '>=' on types: ${l.getClass}, ${r.getClass}"))
    yield result

case class LessThanOrEqualFn(left: Fn[Any], right: Fn[Any]) extends BooleanFn with OperandBinaryFn[Boolean]:
  override val methodName: String = "<="
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] =
    copy(
      left = kids.head.asInstanceOf[Fn[Any]],
      right = kids(1).asInstanceOf[Fn[Any]]
    )
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- numericCompare(l, r)(_ <= _) match
        case Some(b) => ZIO.succeed(b)
        case None    => ZIO.fail(DynaLensError(s"Cannot perform '<=' on types: ${l.getClass}, ${r.getClass}"))
    yield result

case class AndFn(left: Fn[Any], right: Fn[Any]) extends BooleanFn with OperandBinaryFn[Boolean]:
  override val methodName: String = "&&"
  override def rebuild(kids: List[Fn[?]]): BooleanFn =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      kids(1).asInstanceOf[Fn[Any]]
    )
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx).map(_.asInstanceOf[Boolean])
      r <- right.resolve(ctx).map(_.asInstanceOf[Boolean])
    } yield l && r

case class OrFn(left: Fn[Any], right: Fn[Any]) extends BooleanFn with OperandBinaryFn[Boolean]:
  override val methodName: String = "||"
  override def rebuild(kids: List[Fn[?]]): BooleanFn =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      kids(1).asInstanceOf[Fn[Any]]
    )
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      l <- left.resolve(ctx).map(_.asInstanceOf[Boolean])
      r <- right.resolve(ctx).map(_.asInstanceOf[Boolean])
    } yield l || r

case class NotFn(operand: Fn[Any]) extends BooleanFn with OperandUnaryFn[Boolean]:
  override val methodName: String = "!"
  override def rebuild(kids: List[Fn[?]]): BooleanFn =
    copy(operand = kids.head.asInstanceOf[Fn[Any]])
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    operand.resolve(ctx).map(_.asInstanceOf[Boolean]).map(b => !b)

case class IsDefinedFn(operand: Fn[Any]) extends BooleanFn with OperandUnaryFn[Boolean] {
  override def rebuild(kids: List[Fn[?]]): BooleanFn = copy(operand = kids.head.asInstanceOf[Fn[Any]])
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      value <- operand.resolve(ctx)
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
case class ToBooleanFn(operand: Fn[Any]) extends BooleanFn with OperandUnaryFn[Boolean] {
  override def rebuild(kids: List[Fn[?]]): BooleanFn = copy(operand = kids.head.asInstanceOf[Fn[Any]])
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      r <- operand.resolve(ctx)
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

case class StartsWithFn(receiver: Fn[Any], other: Fn[Any]) extends BooleanFn with ReceiverBinaryFn[Boolean] {
  override def rebuild(kids: List[Fn[?]]): BooleanFn =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      kids(1).asInstanceOf[Fn[Any]]
    )
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      aAny <- receiver.resolve(ctx)
      bAny <- other.resolve(ctx)
      aStr <- ZIO.fromEither(toStr(aAny, "startsWith receiver"))
      bStr <- ZIO.fromEither(toStr(bAny, "startsWith argument"))
    } yield aStr.startsWith(bStr)
}

case class EndsWithFn(receiver: Fn[Any], other: Fn[Any]) extends BooleanFn with ReceiverBinaryFn[Boolean] {
  override def rebuild(kids: List[Fn[?]]): BooleanFn =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      kids(1).asInstanceOf[Fn[Any]]
    )
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      aAny <- receiver.resolve(ctx)
      bAny <- other.resolve(ctx)
      aStr <- ZIO.fromEither(toStr(aAny, "endsWith receiver"))
      bStr <- ZIO.fromEither(toStr(bAny, "endsWith argument"))
    } yield aStr.endsWith(bStr)
}

case class ContainsFn(receiver: Fn[Any], other: Fn[Any]) extends BooleanFn with ReceiverBinaryFn[Boolean] {
  override def rebuild(kids: List[Fn[?]]): BooleanFn =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      kids(1).asInstanceOf[Fn[Any]]
    )
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      hay <- receiver.resolve(ctx)
      res <- ContainsFn.containsDynamic(hay, other, ctx)
    } yield res
}

object ContainsFn {

  @tailrec
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

case class EqualsIgnoreCaseFn(receiver: Fn[Any], other: Fn[Any]) extends BooleanFn with ReceiverBinaryFn[Boolean] {
  override def rebuild(kids: List[Fn[?]]): BooleanFn =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      kids(1).asInstanceOf[Fn[Any]]
    )
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      aAny <- receiver.resolve(ctx)
      bAny <- other.resolve(ctx)
      aStr <- ZIO.fromEither(toStr(aAny, "equalsIgnoreCase receiver"))
      bStr <- ZIO.fromEither(toStr(bAny, "equalsIgnoreCase argument"))
    } yield aStr.equalsIgnoreCase(bStr)
}

case class MatchesRegexFn(receiver: Fn[Any], other: Fn[Any]) extends BooleanFn with ReceiverBinaryFn[Boolean] {
  override def rebuild(kids: List[Fn[?]]): BooleanFn =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      kids(1).asInstanceOf[Fn[Any]]
    )
  override val recv: Option[Fn[?]] = Some(receiver)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    for {
      aAny <- receiver.resolve(ctx)
      pAny <- other.resolve(ctx)
      aStr <- ZIO.fromEither(toStr(aAny, "matchesRegex receiver"))
      pStr <- ZIO.fromEither(toStr(pAny, "matchesRegex pattern"))
      // compile once; fail cleanly on bad regexes
      pat <- ZIO
        .attempt(java.util.regex.Pattern.compile(pStr))
        .mapError(e => DynaLensError(s"Invalid regex: ${e.getMessage}"))
    } yield pat.matcher(aStr).matches()
}

// --- Arithmetic  Functions ----

case class AbsFn(receiver: Fn[Any]) extends Fn[Any] with ReceiverUnaryFn[Any] {
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(kids.head.asInstanceOf[Fn[Any]])
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

case class MinFn(receiver: Fn[Any]) extends Fn[Any] with ReceiverUnaryFn[Any] {
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = copy(receiver = kids.head.asInstanceOf[Fn[Any]])
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

case class MaxFn(receiver: Fn[Any]) extends Fn[Any] with ReceiverUnaryFn[Any] {
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = copy(receiver = kids.head.asInstanceOf[Fn[Any]])
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

case class SumFn(receiver: Fn[Any]) extends Fn[Any] with ReceiverUnaryFn[Any] {
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = copy(receiver = kids.head.asInstanceOf[Fn[Any]])
  def resolve(ctx: DynaContext) =
    for {
      raw <- receiver.resolve(ctx).either   // capture success or failure
      value <- raw match {
        case Right(r) => ZIO.succeed(Some(r))
        case Left(e: DynaLensError) if receiver.isOptional =>
          // Treat None (missing optional list) as empty list
          ZIO.succeed(None)
        case Left(e) => ZIO.fail(e)
      }
      // value is None => empty list of zeros
      box <- value match
        case None    => ZIO.succeed(NumPromote.emptyBox)             // special empty vector with numeric kind
        case Some(r) => ZIO.fromEither(collect(r, "sum"))
      v   = toPromotedVector(box)
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

case class AvgFn(receiver: Fn[Any]) extends Fn[Any] with ReceiverUnaryFn[Any] {
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = copy(receiver = kids.head.asInstanceOf[Fn[Any]])
  def resolve(ctx: DynaContext) =
    for {
      raw <- receiver.resolve(ctx)
      box <- ZIO.fromEither(collect(raw, "avg"))
      v   =  toPromotedVector(box)
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

case class MedianFn(receiver: Fn[Any]) extends Fn[Any] with ReceiverUnaryFn[Any] {
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = copy(receiver = kids.head.asInstanceOf[Fn[Any]])
  def resolve(ctx: DynaContext) =
    for {
      raw <- receiver.resolve(ctx)
      box <- ZIO.fromEither(collect(raw, "median"))
      v   =  toPromotedVector(box)
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

case class NegateFn( operand: Fn[Any] ) extends Fn[Any] with OperandUnaryFn[Any]:
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = copy(operand = kids.head.asInstanceOf[Fn[Any]])
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      t <- operand.resolve(ctx)
      result <- t match {
        case a: Int    => ZIO.succeed(a * -1)
        case a: Long   => ZIO.succeed(a * -1)
        case a: Float  => ZIO.succeed(a * -1)
        case a: Double => ZIO.succeed(a * -1)
        case _ =>
          ZIO.fail(DynaLensError(s"NegateFn does not support an operand of type: ${t.getClass}"))
      }
    } yield result

case class ModuloFn(left: Fn[Any], right: Fn[Any]) extends Fn[Any] with OperandBinaryFn[Any] {
  override val methodName: String = "%"
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      kids(1).asInstanceOf[Fn[Any]]
    )
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      l <- left.resolve(ctx)
      r <- right.resolve(ctx)
      result <- (l, r) match
        case (li: Int,  ri: Int)  => ZIO.succeed(li % ri)
        case (li: Long, ri: Long) => ZIO.succeed(li % ri)
        case (li: Int,  rl: Long) => ZIO.succeed(li.toLong % rl)
        case (ll: Long, ri: Int)  => ZIO.succeed(ll % ri.toLong)
        case _ =>
          ZIO.fail(
            DynaLensError(
              s"Modulo (%) only supported for Int and Long, not (${l.getClass}, ${r.getClass})"
            )
          )
    } yield result
}

case class AddFn(
    left: Fn[Any],
    right: Fn[Any]
) extends Fn[Any] with OperandBinaryFn[Any]:
  override val methodName: String = "+"
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      kids(1).asInstanceOf[Fn[Any]]
    )
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
) extends Fn[Any] with OperandBinaryFn[Any]:
  override val methodName: String = "-"
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      kids(1).asInstanceOf[Fn[Any]]
    )
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
) extends Fn[Any] with OperandBinaryFn[Any]:
  override val methodName: String = "*"
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      kids(1).asInstanceOf[Fn[Any]]
    )
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
) extends Fn[Any] with OperandBinaryFn[Any]:
  override val methodName: String = "/"
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      kids(1).asInstanceOf[Fn[Any]]
    )
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

case class TrimFn(receiver: Fn[Any]) extends Fn[String] with ReceiverUnaryFn[String]:
  override def rebuild(kids: List[Fn[?]]): Fn[String] =
    copy(kids.head.asInstanceOf[Fn[Any]])
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    receiver.resolve(ctx).flatMap(v =>
      ZIO.fromEither(toStr(v, "trim()")).map(_.trim)
    )

case class ToLowerFn(receiver: Fn[Any]) extends Fn[String] with ReceiverUnaryFn[String]:
  override def rebuild(kids: List[Fn[?]]): Fn[String] =
    copy(kids.head.asInstanceOf[Fn[Any]])
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    receiver.resolve(ctx).flatMap(v =>
      ZIO.fromEither(toStr(v, "toLowerCase()")).map(_.toLowerCase)
    )

case class ToUpperFn(receiver: Fn[Any]) extends Fn[String] with ReceiverUnaryFn[String]:
  override def rebuild(kids: List[Fn[?]]): Fn[String] =
    copy(kids.head.asInstanceOf[Fn[Any]])
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    receiver.resolve(ctx).flatMap(v =>
      ZIO.fromEither(toStr(v, "toUpperCase()")).map(_.toUpperCase)
    )

case class ConcatFn(parts: List[Fn[Any]]) extends Fn[String] with NAryFn[String]:
  override val methodName: String = "+"
  override def rebuild(kids: List[Fn[?]]): Fn[String] =
    copy(kids.asInstanceOf[List[Fn[Any]]])
  override val isOptional: Boolean = parts.exists(_.isOptional)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    ZIO.foreach(parts)(_.resolve(ctx)).map { resolvedParts =>
      resolvedParts.flatMap {
        case null => Nil
        case None => Nil
        case Some(xs: Seq[?])      => xs.collect { case s: String => s }
        case Some(xs: Iterable[?]) => xs.collect { case s: String => s }
        case xs: Seq[?]            => xs.collect { case s: String => s }
        case xs: Iterable[?]       => xs.collect { case s: String => s }
        case other                 => List(other.toString)
      }.mkString
    }

case class InterpolateFn(receiver: Fn[Any], variables: Map[String, Fn[Any]])
  extends Fn[String] with ReceiverUnaryFn[String]:  // receiver is primary input
  override def args: List[Fn[Any]] = receiver :: variables.values.toList
  override def rebuild(kids: List[Fn[?]]): Fn[String] =
    val newReceiver = kids.head.asInstanceOf[Fn[Any]]
    val newVars     = variables.keys.zip(kids.tail.asInstanceOf[List[Fn[Any]]]).toMap
    copy(newReceiver, newVars)

  override val isOptional: Boolean =
    receiver.isOptional || variables.values.exists(_.isOptional)

  // --- Runtime interpolation -------------------------------------------------
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    for {
      // Resolve template string itself
      templStr <- receiver.resolve(ctx).flatMap { v =>
        ZIO.fromEither(toStr(v, "interpolate(template)"))
      }

      // If no explicit variables were passed, infer them from placeholders
      varsToUse =
        if variables.isEmpty then
          TemplateUtils
            .extractVariables(templStr)
            .map(v => v -> GetFn(v, isOptional = true))
            .toMap
        else variables

      // Resolve all variable values to strings
      resolvedVars <- ZIO.foreach(varsToUse.toList) { case (k, fn) =>
        fn.resolve(ctx).flatMap { v =>
          ZIO.fromEither(toStr(v, s"interpolate($k)")).map(str => k -> str)
        }
      }

      // Perform the interpolation with optional formatting / default values
      result <- ZIO.attempt {
        val varMap = resolvedVars.toMap
        val pattern =
          """\{([a-zA-Z_][a-zA-Z0-9_]*(?:\[[0-9]+\]|\.[a-zA-Z_][a-zA-Z0-9_]*(?:\[[0-9]+\])?)*)(?:%([^}:]+))?(?::([^}]+))?\}""".r

        pattern.replaceAllIn(
          templStr,
          m => {
            val varName  = m.group(1)
            val fmtOpt   = Option(m.group(2)) // e.g. "0.2f"
            val dfltOpt  = Option(m.group(3)) // default string
            val valueOpt = varMap.get(varName).filter(_.nonEmpty)

            (valueOpt, fmtOpt) match {
              case (Some(value), Some(fmt)) =>
                toDbl(value, s"interpolate($varName)").fold(
                  _ => value,
                  d => String.format(Locale.US, s"%$fmt", Double.box(d))
                )

              case (Some(value), None) => value

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
      }.mapError(th => DynaLensError(s"interpolate() failed: ${th.getMessage}"))
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

case class SubstringFn(
                        receiver: Fn[Any],
                        start: Fn[Int],
                        end: Option[Fn[Int]]
                      ) extends Fn[String] with ReceiverUnaryFn[String]:

  override def args: List[Fn[Any]] =
    (receiver :: start :: end.toList).asInstanceOf[List[Fn[Any]]]
  override def rebuild(kids: List[Fn[?]]): Fn[String] =
    val newReceiver = kids.head.asInstanceOf[Fn[Any]]
    val newStart = kids(1).asInstanceOf[Fn[Int]]
    val newEnd = kids.lift(2).map(_.asInstanceOf[Fn[Int]])
    copy(newReceiver, newStart, newEnd)
  override val isOptional: Boolean =
    receiver.isOptional || start.isOptional || end.exists(_.isOptional)
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    for
      s <- receiver.resolve(ctx).map {
        case null => ""
        case v => v.toString
      }
      startIdx <- start.resolve(ctx)
      result <- end match
        case Some(endFn) =>
          endFn.resolve(ctx).map(endIdx => s.substring(startIdx, endIdx))
        case None =>
          ZIO.succeed(s.substring(startIdx))
    yield result


case class ReplaceFn(
                      receiver: Fn[Any],
                      target: Fn[Any],
                      replacement: Fn[Any]
                    ) extends Fn[String] with ReceiverUnaryFn[String]:

  override def args: List[Fn[Any]] = List(receiver, target, replacement)
  override def rebuild(kids: List[Fn[?]]): Fn[String] =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      kids(1).asInstanceOf[Fn[Any]],
      kids(2).asInstanceOf[Fn[Any]]
    )
  override val isOptional: Boolean =
    receiver.isOptional || target.isOptional || replacement.isOptional
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, String] =
    for
      str <- receiver.resolve(ctx).map(v => if v == null then "" else v.toString)
      t <- target.resolve(ctx).map(v => if v == null then "" else v.toString)
      r <- replacement.resolve(ctx).map(v => if v == null then "" else v.toString)
    yield str.replace(t, r)

// --- Option Functions ----

// left = primary, right = fallback
case class ElseFn(left: Fn[Any], right: Fn[Any]) extends Fn[Any] with OperandBinaryFn[Any]:
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      kids(1).asInstanceOf[Fn[Any]]
    )
  override val isOptional: Boolean = left.isOptional || right.isOptional
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    left.resolve(ctx).flatMap {
      case opt: Option[?] =>
        opt match
          case Some(v) => ZIO.succeed(v)         // pass-through
          case None    => right.resolve(ctx)  // only None triggers fallback
      case v =>
        ZIO.succeed(v) // non-Option: pass-through
    }

// --- Map Functions --- (except ContainsFn, which is multipurpose... given in another section)

case class KeysFn(receiver: Fn[Any]) extends Fn[List[Any]] with ReceiverUnaryFn[List[Any]]:
  override def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(kids.head.asInstanceOf[Fn[Any]])
  override val isOptional: Boolean = receiver.isOptional
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, List[Any]] =
    for {
      mAny <- receiver.resolve(ctx)
      result <- mAny match
        case null => ZIO.succeed(Nil)
        case m: Map[?, ?] => ZIO.succeed(m.keys.toList)
        case Some(m: Map[?, ?]) => ZIO.succeed(m.keys.toList)
        case None => ZIO.succeed(Nil)
        case other =>
          ZIO.fail(DynaLensError(
            s"keys() can only be used on Map or Option[Map], but found ${other.getClass.getName}"
          ))
    } yield result

case class ValuesFn(receiver: Fn[Any]) extends Fn[List[Any]] with ReceiverUnaryFn[List[Any]]:
  override def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(kids.head.asInstanceOf[Fn[Any]])
  override val isOptional: Boolean = receiver.isOptional
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, List[Any]] =
    for {
      mAny <- receiver.resolve(ctx)
      result <- mAny match
        case null => ZIO.succeed(Nil)
        case m: Map[?, ?] => ZIO.succeed(m.values.toList)
        case Some(m: Map[?, ?]) => ZIO.succeed(m.values.toList)
        case None => ZIO.succeed(Nil)
        case other =>
          ZIO.fail(DynaLensError(
            s"values() can only be used on Map or Option[Map], but found ${other.getClass.getName}"
          ))
    } yield result

case class MapGetFn(receiver: Fn[Any], other: Fn[Any]) extends Fn[Any] with ReceiverBinaryFn[Any]:
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      kids(1).asInstanceOf[Fn[Any]]
    )
  override val isOptional: Boolean = receiver.isOptional
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      m <- receiver.resolve(ctx)
      k <- other.resolve(ctx)
      v <- m match
        case mm: Map[?, ?] @unchecked =>
          mm.asInstanceOf[Map[Any, Any]].get(k) match
            case Some(v) => ZIO.succeed(v)
            case None    => ZIO.fail(DynaLensError(s"get(): key '$k' not found"))
        case Some(mm: Map[?, ?] @unchecked) =>
          mm.asInstanceOf[Map[Any, Any]].get(k) match
            case Some(v) => ZIO.succeed(v)
            case None    => ZIO.fail(DynaLensError(s"get(): key '$k' not found"))
        case other =>
          ZIO.fail(DynaLensError(
            s"get(): receiver is not a Map (got ${other.getClass.getSimpleName})"
          ))
    } yield v

case class Tuple2Fn(left: Fn[Any], right: Fn[Any]) extends Fn[Any] with OperandBinaryFn[Any]:
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(
      kids(0).asInstanceOf[Fn[Any]],
      kids(1).asInstanceOf[Fn[Any]]
    )
  override val isOptional: Boolean = left.isOptional || right.isOptional
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      a <- left.resolve(ctx)
      b <- right.resolve(ctx)
    } yield (a, b)

// --- Collection (Iterable) Functions ----

// Wrap any Fn that produces a collection; pick element at fixed index
case class IndexFn(receiver: Fn[Any], index: Int) extends Fn[Any] with ReceiverUnaryFn[Any]:
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(kids.head.asInstanceOf[Fn[Any]], index)
  // special: let rhsType see the full node
  override val recv: Option[Fn[?]] = Some(this)
  override val isOptional: Boolean = receiver.isOptional
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for
      raw  <- receiver.resolve(ctx)
      list <- ZIO.fromEither(asSeq(raw, "index"))
      elem <- list.lift(index) match
        case Some(e) => ZIO.succeed(e)
        case None    => ZIO.fail(DynaLensError(s"Index $index out of bounds"))
    yield elem


case class LoopFn(inner: Fn[Any]) extends Fn[Any]:
  override def args: List[Fn[Any]] = List(inner)
  override def children: List[Fn[?]] = List(inner)
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(kids.head.asInstanceOf[Fn[Any]])
  override val isOptional: Boolean = inner.isOptional

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ctx.get("this") match
      case Some((raw, lens)) =>
        for
          seq     <- ZIO.fromEither(asSeq(raw, "loop"))
          results <- ZIO.foreach(seq) { item =>
            val localCtx = ctx.updatedWith("this", (item, lens))
            inner.resolve(localCtx)
          }
        yield results.toList
      case None =>
        ZIO.fail(DynaLensError("LoopFn requires 'this' bound to a collection"))


// other is the predicate fn
case class FilterFn(receiver: Fn[Any], other: Fn[Any]) extends Fn[Any] with ReceiverBinaryFn[Any]:
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(kids(0).asInstanceOf[Fn[Any]], kids(1).asInstanceOf[Fn[Any]])
  override val isOptional: Boolean = receiver.isOptional
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for
      raw  <- receiver.resolve(ctx)
      seq  <- ZIO.fromEither(asSeq(raw, "filter"))
      kept <- ZIO.foreach(seq) { e =>
        other.asInstanceOf[BooleanFn]
          .resolve(withElemCtx(e, ctx))
          .map(b => if b then Some(e) else None)
      }
    yield kept.flatten


// left = head, right = tail
case class ConsFn(left: Fn[Any], right: Fn[Any]) extends Fn[List[Any]] with OperandBinaryFn[List[Any]]:
  override val methodName: String = "::"
  override def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    copy(kids.head.asInstanceOf[Fn[Any]], kids(1).asInstanceOf[Fn[Any]])
  override val isOptional: Boolean = left.isOptional || right.isOptional
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, List[Any]] =
    for
      h <- left.resolve(ctx)
      t <- right.resolve(ctx)
      list <- t match
        case null | None       => ZIO.succeed(List(h))
        case Some(xs: Seq[?])  => ZIO.succeed(h :: xs.asInstanceOf[Seq[Any]].toList)
        case xs: Seq[?]        => ZIO.succeed(h :: xs.asInstanceOf[Seq[Any]].toList)
        case other             => ZIO.fail(DynaLensError(s":: expects a list tail, got: ${other.getClass.getSimpleName}"))
    yield list

case class SortAscFn(receiver: Fn[Any], keyPath: Option[String]) extends SortFn with ReceiverUnaryFn[Any]:
  val asc: Boolean = true
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(kids.head.asInstanceOf[Fn[Any]], keyPath)
  override val isOptional: Boolean = receiver.isOptional
  override val fnName: String = "sortAsc"

case class SortDescFn(receiver: Fn[Any], keyPath: Option[String]) extends SortFn with ReceiverUnaryFn[Any]:
  val asc: Boolean = false
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(kids.head.asInstanceOf[Fn[Any]], keyPath)
  override val isOptional: Boolean = receiver.isOptional
  override val fnName: String = "sortDesc"

trait SortFn extends Fn[Any] {
  val receiver: Fn[Any]
  val keyPath: Option[String]
  val asc: Boolean
  val fnName: String
  override val recv: Option[Fn[?]] = Some(receiver)
  override val isOptional: Boolean = receiver.isOptional
  private inline def opName: String = if asc then "sortAsc" else "sortDesc"

  implicit private val cmpOrd: Ordering[Comparable[Any]] =
    (a: Comparable[Any], b: Comparable[Any]) => a.compareTo(b)

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      raw <- receiver.resolve(ctx)
      seq <- ZIO.fromEither(asSeq(raw, fnName))
      sorted <- keyPath match {
        case Some(pth) =>
          // sort by key extracted from each element via `this`
          for {
            pairs <- ZIO.foreach(seq) { elem =>
              val keyPathResolved =
                if pth.startsWith("this.") then pth
                else s"this.$pth"
              GetFn(keyPathResolved, isOptional = false).resolve(withElemCtx(elem, ctx)).flatMap {
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

case class DistinctFn(receiver: Fn[Any], fieldPath: Option[String]) extends Fn[Any] with ReceiverUnaryFn[Any]:
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(kids.head.asInstanceOf[Fn[Any]], fieldPath)

  override val isOptional: Boolean = receiver.isOptional

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      raw <- receiver.resolve(ctx)
      items <- ZIO.fromEither(asSeq(raw, "distinct"))

      pairs <- fieldPath match
        case None =>
          // distinct by entire element
          ZIO.succeed(items.map(x => (x, x)))
        case Some(rawKey) =>
          val keyPath =
            if rawKey.startsWith("this.") then rawKey
            else s"this.$rawKey"
          ZIO.foreach(items) { item =>
            val itemCtx = withElemCtx(item, ctx)
            GetFn(keyPath, isOptional = false).resolve(itemCtx).map(k => (k, item))
          }

      deduped = {
        val seen = scala.collection.mutable.HashSet[Any]()
        val buf = scala.collection.mutable.ArrayBuffer[Any]()
        pairs.foreach { case (k, v) =>
          if !seen.contains(k) then {
            seen += k; buf += v
          }
        }
        buf.toList
      }
    } yield deduped

case class LimitFn(receiver: Fn[Any], count: Int) extends Fn[Any] with ReceiverUnaryFn[Any]:
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(kids.head.asInstanceOf[Fn[Any]], count)
  override val isOptional: Boolean = receiver.isOptional

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      raw <- receiver.resolve(ctx)
      seq <- ZIO.fromEither(asSeq(raw, "limit"))
    } yield seq.take(count)

case class ReverseFn(receiver: Fn[Any]) extends Fn[Any] with ReceiverUnaryFn[Any]:
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(kids.head.asInstanceOf[Fn[Any]])
  override val isOptional: Boolean = receiver.isOptional

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      raw <- receiver.resolve(ctx)
      seq <- ZIO.fromEither(asSeq(raw, "reverse"))
    } yield seq.reverse

case class CleanFn(receiver: Fn[Any]) extends Fn[Any] with ReceiverUnaryFn[Any]:
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(kids.head.asInstanceOf[Fn[Any]])
  override val isOptional: Boolean = receiver.isOptional

  private def truthy(v: Any): Boolean = v match
    case null            => false
    case None            => false
    case _: Unit         => false
    case s: CharSequence => s.toString.trim.nonEmpty
    case it: Iterable[?] => it.iterator.hasNext
    case _               => true

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      raw <- receiver.resolve(ctx)
      seq <- ZIO.fromEither(asSeq(raw, "clean"))
    } yield seq.iterator.filter(truthy).toList


// --- Misc Functions ----

case class PolyFn(parts: List[Fn[Any]]) extends NAryFn[Any]:
  override def rebuild(kids: List[Fn[?]]): PolyFn =
    copy(parts = kids.asInstanceOf[List[Fn[Any]]])
  override val isOptional: Boolean = parts.exists(_.isOptional)

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ctx.get("this") match
      case Some((_, lens)) =>
        for {
          _ <- ZIO.foreachDiscard(parts) { fn =>
            fn.resolve(ctx).map(res => ctx.put("this", (res, lens)))
          }
        } yield ctx("this")._1
      case None =>
        ZIO.fail(DynaLensError("'this' not found in context"))


case class IdentityFn(receiver: Fn[?]) extends Fn[Any] {
  override def children: List[Fn[?]] = List(receiver)
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(kids.head)
  override val recv: Option[Fn[?]] = Some(receiver)
  override val isOptional: Boolean = receiver.isOptional

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    receiver.resolve(ctx)
}

case class LenFn(receiver: Fn[Any]) extends Fn[Int] {
  override def children: List[Fn[?]] = List(receiver)
  override def rebuild(kids: List[Fn[?]]): Fn[Int] =
    copy(kids.head.asInstanceOf[Fn[Any]])
  override val recv: Option[Fn[?]] = Some(receiver)
  override val isOptional: Boolean = receiver.isOptional

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Int] =
    receiver.resolve(ctx).map {
      case null                         => 0
      case None                         => 0
      case Some(s: CharSequence)        => s.length
      case Some(arr: Array[?])          => arr.length
      case Some(it: Iterable[?] @unchecked) => it.size
      case Some(other)                  => other.toString.length
      case s: CharSequence              => s.length
      case arr: Array[?]                => arr.length
      case it: Iterable[?] @unchecked   => it.size
      case other                        => other.toString.length
    }
}

case class MapFwdFn(mapName: String, receiver: Fn[Any]) extends Fn[Any] with ReceiverUnaryFn[Any] {
  override val isOptional: Boolean = receiver.isOptional
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(receiver = kids.head.asInstanceOf[Fn[Any]])
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      raw <- receiver.resolve(ctx)
      res <- ZIO.serviceWithZIO[_BiMapRegistry] { registry =>
        registry.get(mapName) match {
          case Some(bimap) =>
            asSeq(raw, s"mapFwd($mapName)") match {
              case Right(seq) =>
                ZIO.foreach(seq) { item =>
                  bimap.getForward(item.toString) match
                    case Some(r) => ZIO.succeed(r)
                    case None => ZIO.fail(DynaLensError(s"Key '$item' not found in forward map '$mapName'"))
                }.map(_.toList)
              case Left(_) =>
                bimap.getForward(raw.toString) match
                  case Some(r) => ZIO.succeed(r)
                  case None => ZIO.fail(DynaLensError(s"Key '$raw' not found in forward map '$mapName'"))
            }
          case None =>
            ZIO.fail(DynaLensError(s"BiMap '$mapName' not found"))
        }
      }
    } yield res
}

case class MapRevFn(mapName: String, receiver: Fn[Any]) extends Fn[Any] with ReceiverUnaryFn[Any] {
  override val isOptional: Boolean = receiver.isOptional
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(receiver = kids.head.asInstanceOf[Fn[Any]])
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      raw <- receiver.resolve(ctx)
      res <- ZIO.serviceWithZIO[_BiMapRegistry] { registry =>
        registry.get(mapName) match {
          case Some(bimap) =>
            asSeq(raw, s"mapRev($mapName)") match {
              case Right(seq) =>
                ZIO.foreach(seq) { item =>
                  bimap.getReverse(item.toString) match
                    case Some(r) => ZIO.succeed(r)
                    case None => ZIO.fail(DynaLensError(s"Key '$item' not found in reverse map '$mapName'"))
                }.map(_.toList)
              case Left(_) =>
                bimap.getReverse(raw.toString) match
                  case Some(r) => ZIO.succeed(r)
                  case None => ZIO.fail(DynaLensError(s"Key '$raw' not found in reverse map '$mapName'"))
            }
          case None =>
            ZIO.fail(DynaLensError(s"BiMap '$mapName' not found"))
        }
      }
    } yield res
}

case class FormatDateFn(receiver: Fn[Any], pattern: Fn[String]) extends Fn[Any] with ReceiverBinaryFn[Any] {
  def other: Fn[Any] = pattern.asInstanceOf[Fn[Any]]
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(
      receiver = kids(0).asInstanceOf[Fn[Any]],
      pattern = kids(1).asInstanceOf[Fn[String]]
    )
  override val isOptional: Boolean = receiver.isOptional
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      d <- receiver.resolve(ctx)
      p <- pattern.resolve(ctx)
      result <- d match
        case date: java.util.Date =>
          ZIO.attempt {
            val sdf = new java.text.SimpleDateFormat(p)
            sdf.format(date)
          }.mapError(e => DynaLensError(s"Error formatting date: ${e.getMessage}"))
        case other =>
          ZIO.fail(DynaLensError(s"Expected java.util.Date but got ${other.getClass.getName}"))
    } yield result
}

case class ParseDateFn(receiver: Fn[Any], pattern: Fn[String]) extends Fn[Any] with ReceiverBinaryFn[Any] {
  def other: Fn[Any] = pattern.asInstanceOf[Fn[Any]]
  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(
      receiver = kids(0).asInstanceOf[Fn[Any]],
      pattern = kids(1).asInstanceOf[Fn[String]]
    )
  override val isOptional: Boolean = receiver.isOptional
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      s <- receiver.resolve(ctx)
      p <- pattern.resolve(ctx)
      result <- s match
        case str: String =>
          ZIO.attempt {
            val sdf = new java.text.SimpleDateFormat(p)
            sdf.parse(str)
          }.mapError(e => DynaLensError(s"Date parse error: ${e.getMessage}"))
        case other =>
          ZIO.fail(DynaLensError(s"Expected String for toDate but got ${other.getClass.getName}"))
    } yield result
}

// --- Stand-Alone Functions ----

case class NowFn() extends Fn[Any] {
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ZIO.succeed(new java.util.Date())
}

case class UUIDFn() extends Fn[Any] {
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ZIO.succeed(java.util.UUID.randomUUID())
}

// --- Case Function ----

case class CaseWhenFn(
                       receiver: Fn[Any],
                       cases: Vector[(Any, Fn[Any])],
                       default: Option[Fn[Any]],
                       permissive: Boolean = false
                     ) extends Fn[Any] {

  override def args: List[Fn[Any]] =
    receiver :: (cases.map(_._2).toList ++ default.toList)

  override def children: List[Fn[?]] = args

  override def rebuild(kids: List[Fn[?]]): Fn[Any] = {
    val rcv      = kids.head.asInstanceOf[Fn[Any]]
    val rhsCount = cases.length
    val newCases =
      cases.indices.map(i => (cases(i)._1, kids(i + 1).asInstanceOf[Fn[Any]])).toVector
    val newDefault =
      if default.isDefined then Some(kids(rhsCount + 1).asInstanceOf[Fn[Any]]) else None
    copy(receiver = rcv, cases = newCases, default = newDefault)
  }

  override val recv: Option[Fn[?]] = Some(receiver)
  override val isOptional: Boolean =
    receiver.isOptional || cases.exists(_._2.isOptional) || default.exists(_.isOptional)

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      v <- receiver.resolve(ctx)
      out <- cases.collectFirst { case (p, rhs) if v == p => rhs.resolve(ctx) }
        .getOrElse {
          default.map(_.resolve(ctx))
            .getOrElse {
              if permissive then ZIO.succeed(v)
              else ZIO.fail(DynaLensError(s"No case matched for value: $v"))
            }
        }
    } yield out
}