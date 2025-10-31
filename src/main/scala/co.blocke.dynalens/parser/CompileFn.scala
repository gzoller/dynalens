package co.blocke.dynalens
package parser

import co.blocke.dynalens._

/** Base trait for every DSL compile function.
 *
 * Each function:
 *   1. declares its DSL name and built-in status
 *      2. specifies its valid arity and receiver acceptance rules
 *      3. builds an AST node (Fn) and performs semantic validation
 *      4. computes the resulting FieldType for downstream type checks
 */
trait CompileFn {

  /** DSL keyword or symbol (e.g. "filter", "<", "+", "abs") */
  def name: String

  /** True for functions that don't have a receiver, eg now() or uuid() */
  def standalone: Boolean = false

  /** Arity requirement. */
  def minArgs: Int
  def maxArgs: Int = minArgs // default: exact arity

  /** Check if the receiver’s type is acceptable. */
  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean

  /** The resulting type after applying this function. */
  def resultType(receiver: Receiver, args: List[FieldType])
                (using ctx: ExprContext): FieldType

  /** Phase 1: Build the AST node from argument Fns. */
  def build(recv: Receiver, args: List[Fn[Any]])
           (using ctx: ExprContext): Either[DLCompileError, Fn[Any]]

  /** Phase 2: Validate semantics and argument types (may refine error messages). */
  def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit]
}