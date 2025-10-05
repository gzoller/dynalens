package co.blocke.dynalens
package parser
package fn


/**
 * Base for all comparison-style Boolean CFns (<, >, <=, >=, ==, !=).
 * Handles the two-phase validation pattern (defer until both operand types known),
 * and standard numeric + non-optional enforcement.
 */
// Base
abstract class ComparisonCFn[T <: OperandBinaryFn[?]](val op: String)
  extends CompileFn[T]:

  override val name: String = op
  override val builtIn: Boolean = true
  val minArgs: Int = 2
  override val maxArgs: Int = 2

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean = true

  def resultType(receiver: FieldType, args: List[FieldType])
                (using ctx: ExprContext): FieldType =
    ScalarType("", "scala.Boolean")

  override def validate(fn: Fn[?])
                       (using ctx: ExprContext): Either[DLCompileError, Unit] =
    fn match
      case cmp: OperandBinaryFn[?] =>
        val lhsOpt = Utility.rhsType(cmp.left)
        val rhsOpt = Utility.rhsType(cmp.right)
        Validation.deferIfUnknown(lhsOpt, rhsOpt, validatePair)
      case _ => Right(())

  /** Shared comparison operand validation once both types are known */
  protected def validatePair(lhs: FieldType, rhs: FieldType)
                            (using ctx: ExprContext): Either[DLCompileError, Unit] =
    for
      // Disallow optional operands
      _ <- (lhs, rhs) match
        case (_: OptionType, _) | (_, _: OptionType) =>
          Left(DLCompileError(0, s"$name operands cannot be optional"))
        case _ => Right(())

      // Require both numeric
      _ <- if lhs.isNumeric && rhs.isNumeric then Right(())
      else Left(DLCompileError(0,
        s"$name operands must be numeric (found ${lhs.typeName}, ${rhs.typeName})"
      ))
    yield ()


abstract class EqualityCFn[T <: OperandBinaryFn[?]](val opName: String)
  extends ComparisonCFn[T](opName):

  override protected def validatePair(lhs: FieldType, rhs: FieldType)
                                     (using ctx: ExprContext): Either[DLCompileError, Unit] =
    // Non-optional only; equality can compare any compatible scalar types
    (lhs, rhs) match
      case (_: OptionType, _) | (_, _: OptionType) =>
        Left(DLCompileError(0, s"$opName operands cannot be optional"))
      case _ =>
        Right(())


//--------------------------------------------------------------------------

object CLessThanFn extends ComparisonCFn[LessThanFn]("<"):
  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left  <- args.headOption.toRight(DLCompileError(0, "< missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "< missing right arg"))
    yield LessThanFn(left, right)


object CLessThanOrEqualFn extends ComparisonCFn[LessThanOrEqualFn]("<="):
  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left <- args.headOption.toRight(DLCompileError(0, "<= missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "<= missing right arg"))
    yield LessThanOrEqualFn(left, right)


object CGreaterThanFn extends ComparisonCFn[GreaterThanFn](">"):
  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left <- args.headOption.toRight(DLCompileError(0, "> missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "> missing right arg"))
    yield GreaterThanFn(left, right)


object CGreaterThanOrEqualFn extends ComparisonCFn[GreaterThanOrEqualFn](">="):
  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left <- args.headOption.toRight(DLCompileError(0, ">= missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, ">= missing right arg"))
    yield GreaterThanOrEqualFn(left, right)


object CEqualFn extends EqualityCFn[EqualFn]("=="):
  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left  <- args.headOption.toRight(DLCompileError(0, "== missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "== missing right arg"))
    yield EqualFn(left, right)


object CNotEqualFn extends EqualityCFn[NotEqualFn]("!="):
  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left  <- args.headOption.toRight(DLCompileError(0, "!= missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "!= missing right arg"))
    yield NotEqualFn(left, right)
