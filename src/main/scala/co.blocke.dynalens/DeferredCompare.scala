package co.blocke.dynalens
package parser

import zio.*

/** A generic comparison node whose operator and operands
 * are known but whose type checks are deferred until a receiver is set.
 */
case class DeferredCompare(op: String, left: Fn[Any], right: Fn[Any])
  extends BooleanFn:

  /** After the receiver is bound, run the correct CompileFn and produce
   * the final concrete BooleanFn (GreaterThanFn, EqualFn, etc.).
   */
  def compile()(using ctx: ExprContext): Either[DLCompileError, BooleanFn] =
    op match
      case "==" => CEqualFn.build(List(left, right))
      case "!=" => CNotEqualFn.build(List(left, right))
      case ">"  => CGreaterThanFn.build(List(left, right))
      case "<"  => CLessThanFn.build(List(left, right))
      case ">=" => CGreaterThanOrEqualFn.build(List(left, right))
      case "<=" => CLessThanOrEqualFn.build(List(left, right))
      case o    => Left(DLCompileError(0, s"Unknown comparison operator: $o"))

  override def children: List[Fn[?]] = List(left, right)

  override def rebuild(kids: List[Fn[?]]): BooleanFn =
    copy(left = kids(0), right = kids(1))

  // This should never be invoked at runtime: it must be rewritten
  // to a real comparison (GreaterThanFn, EqualFn, etc.) during compile.
  override def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    ZIO.fail(DynaLensError("DeferredCompare must be replaced at compile time before execution"))