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
trait CompileFn[R <: Fn[?]] {

  /** DSL keyword or symbol (e.g. "filter", "<", "+", "abs") */
  def name: String

  /** True for core language built-ins (like +, <, ::) */
  def builtIn: Boolean = false

  /** True for functions that don't have a receiver, eg now() or uuid() */
  def standalone: Boolean = false

  /** Arity requirement. */
  def minArgs: Int

  def maxArgs: Int = minArgs // default: exact arity

  /** Check if the receiver’s type is acceptable. */
  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean

  /** The resulting type after applying this function. */
  def resultType(receiver: FieldType, args: List[FieldType])
                (using ctx: ExprContext): FieldType

  /** Phase 1: Build the AST node from argument Fns. */
  def build(recv: Fn[Any], args: List[Fn[Any]])
           (using ctx: ExprContext): Either[DLCompileError, R]

  /** Phase 2: Validate semantics and argument types (may refine error messages). */
  def validate(fn: Fn[?])(using ctx: ExprContext)
  : Either[DLCompileError, Unit] = Right(())
}


object CompileFn:
  def requireNumeric1(arg: Fn[Any], name: String, off: Int)
                     (using ctx: ExprContext): Either[DLCompileError, Unit] =
    Utility.rhsType(arg) match
      case Some(t) if t.isNumeric =>
        Right(())
      case Some(t) =>
        Left(DLCompileError(off,
          s"$name requires a numeric operand, found ${t.typeName}"))
      case None =>
        Left(DLCompileError(off,
          s"$name cannot determine operand type"))

  /** Ensure both args are numeric at compile time. */
//  def requireNumeric2(args: List[Fn[Any]], name: String, off: Int)
//                     (using ctx: ExprContext): Either[DLCompileError, Unit] =
//    (Utility.rhsType(args.head), Utility.rhsType(args(1))) match
//      case (Some(lt), Some(rt)) if lt.isNumeric && rt.isNumeric =>
//        Right(())
//      case (Some(lt), Some(rt)) =>
//        Left(DLCompileError(off,
//          s"$name requires numeric operands, found ${lt.typeName} and ${rt.typeName}"))
//      case _ =>
//        Left(DLCompileError(off,
//          s"$name cannot determine operand types"))

  def requireNumeric2(args: List[Fn[?]], method: String, offset: Int)
                     (using ctx: ExprContext): Either[DLCompileError, Unit] =
    val types = args.map(a => Utility.rhsType(a))
    println(s"[requireNumeric2] method=$method")
    types.zipWithIndex.foreach { case (opt, i) =>
      println(s"  arg$i -> ${opt.map(_.typeName).getOrElse("<?>")}")
      println(s"  arg$i full: ${opt}")
      println(s"  arg$i numeric? ${opt.exists(_.isNumeric)}")
    }

    (types.headOption, types.lift(1)) match
      case (Some(Some(l)), Some(Some(r))) if l.isNumeric && r.isNumeric =>
        println(s"[requireNumeric2] ✅ numeric pair: ${l.typeName}, ${r.typeName}")
        Right(())
      case (Some(Some(l)), Some(Some(r))) =>
        println(s"[requireNumeric2] ❌ non-numeric pair: ${l.typeName}, ${r.typeName}")
        Left(DLCompileError(offset, s"$method requires numeric operands, found ${l.typeName} and ${r.typeName}"))
      case other =>
        println(s"[requireNumeric2] ❌ insufficient args or unresolved types: $other")
        Left(DLCompileError(offset, s"$method requires numeric operands"))

  /** Ensure neither argument is an Option type. */
  def requireNonOptional2(args: List[Fn[Any]], name: String, off: Int)
                         (using ctx: ExprContext): Either[DLCompileError, Unit] =
    (Utility.rhsType(args.head), Utility.rhsType(args(1))) match
      case (Some(lt), Some(rt))
        if !lt.isInstanceOf[OptionType] && !rt.isInstanceOf[OptionType] =>
        Right(())
      case (Some(lt), Some(rt)) =>
        Left(DLCompileError(off,
          s"$name cannot be applied to Option types (use .else() to handle missing values)"))
      case _ =>
        Left(DLCompileError(off,
          s"$name cannot determine operand types"))

  /** Ensure both args are boolean at compile time (for &&, ||). */
  def requireBoolean2(args: List[Fn[Any]], name: String, off: Int)
                     (using ctx: ExprContext): Either[DLCompileError, Unit] =
    (Utility.rhsType(args.head), Utility.rhsType(args(1))) match
      case (Some(lt), Some(rt)) if lt.typeName == "scala.Boolean" && rt.typeName == "scala.Boolean" =>
        Right(())
      case (Some(lt), Some(rt)) =>
        Left(DLCompileError(off,
          s"$name requires boolean operands, found ${lt.typeName} and ${rt.typeName}"))
      case _ =>
        Left(DLCompileError(off,
          s"$name cannot determine operand types"))

  /** Ensure a single arg is boolean (for unary !). */
  def requireBoolean1(arg: Fn[Any], name: String, off: Int)
                     (using ctx: ExprContext): Either[DLCompileError, Unit] =
    Utility.rhsType(arg) match
      case Some(ft) if ft.typeName == "scala.Boolean" => Right(())
      case Some(ft) =>
        Left(DLCompileError(off,
          s"$name requires a boolean operand, found ${ft.typeName}"))
      case None =>
        Left(DLCompileError(off,
          s"$name cannot determine operand type"))

  inline def isNum(ft: FieldType): Boolean = ft match
    case ScalarType(_, t) =>
      t match
        case "scala.Int" | "scala.Long" | "scala.Float" | "scala.Double" |
             "scala.Short" | "scala.Byte" |
             "java.lang.Integer" | "java.lang.Long" | "java.lang.Float" | "java.lang.Double" |
             "java.math.BigDecimal" => true
        case _ => false
    case _ => false

  def expectBoolean1(arg: Fn[Any], name: String, off: Int)
    : Either[DLCompileError, BooleanFn] =
      arg match
        case b: BooleanFn => Right(b)
        case _ =>
          Left(DLCompileError(off, s"$name requires a boolean operand"))