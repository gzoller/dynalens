package co.blocke.dynalens
package parser

/** Minimal compile-time companion for every Fn.
 * Lets us attach compile-time checks without polluting runtime code.
 */
trait CompileFn[R <: Fn[?]]:
  /** Parse/build the runtime Fn.
   * Use ctx for schema/type checks and return a DLCompileError if something is invalid.
   */
  def build(args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, R]

trait CompileChecks:
  /** Ensure both args are numeric at compile time. */
  def requireNumeric2(args: List[Fn[Any]], name: String, off: Int)
                     (using ctx: ExprContext): Either[DLCompileError, Unit] =
    (Utility.rhsType(args.head), Utility.rhsType(args(1))) match
      case (Some(lt), Some(rt)) if lt.isNumeric && rt.isNumeric =>
        Right(())
      case (Some(lt), Some(rt)) =>
        Left(DLCompileError(off,
          s"$name requires numeric operands, found ${lt.typeName} and ${rt.typeName}"))
      case _ =>
        Left(DLCompileError(off,
          s"$name cannot determine operand types"))

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


// ==== Arithmetic Fns ====

object CAddFn extends CompileFn[AddFn], CompileChecks:
  def build(args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, AddFn] =
    for
      left  <- args.headOption.toRight(DLCompileError(0, "+ missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "+ missing right arg"))
      _     <- requireNonOptional2(List(left, right), "+", 0)
      _     <- requireNumeric2(List(left, right), "+", 0)
    yield AddFn(left, right)

object CSubtractFn extends CompileFn[SubtractFn] with CompileChecks:
  def build(args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left <- args.headOption.toRight(DLCompileError(0, "- missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "- missing right arg"))
      _ <- requireNonOptional2(args, "-", 0)
      _ <- requireNumeric2(args, "-", 0)
    yield SubtractFn(left, right)

object CMultiplyFn extends CompileFn[MultiplyFn] with CompileChecks:
  def build(args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left <- args.headOption.toRight(DLCompileError(0, "* missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "* missing right arg"))
      _ <- requireNonOptional2(args, "*", 0)
      _ <- requireNumeric2(args, "*", 0)
    yield MultiplyFn(left, right)

object CDivideFn extends CompileFn[DivideFn] with CompileChecks:
  def build(args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left <- args.headOption.toRight(DLCompileError(0, "/ missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "/ missing right arg"))
      _ <- requireNonOptional2(args, "/", 0)
      _ <- requireNumeric2(args, "/", 0)
    yield DivideFn(left, right)

object CModuloFn extends CompileFn[ModuloFn] with CompileChecks:
  def build(args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left <- args.headOption.toRight(DLCompileError(0, "% missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "% missing right arg"))
      _ <- requireNonOptional2(args, "%", 0)
      _ <- requireNumeric2(args, "%", 0)
    yield ModuloFn(left, right)

// ==== Comparison Fns ====

object CGreaterThanFn extends CompileFn[GreaterThanFn] with CompileChecks:
  def build(args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, GreaterThanFn] =
    for {
      left  <- args.headOption.toRight(DLCompileError(0, "> missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "> missing right arg"))
      _     <- requireNonOptional2(List(left, right), ">", 0)   // add this if you want no Option
      _     <- requireNumeric2(List(left, right), ">", 0)
    } yield GreaterThanFn(left, right)

object CLessThanFn extends CompileFn[LessThanFn] with CompileChecks:
  def build(args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left <- args.headOption.toRight(DLCompileError(0, "< missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "< missing right arg"))
      _ <- requireNonOptional2(args, "<", 0)
      _ <- requireNumeric2(args, "<", 0)
    yield LessThanFn(left, right)

object CGreaterThanOrEqualFn extends CompileFn[GreaterThanOrEqualFn] with CompileChecks:
  def build(args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left <- args.headOption.toRight(DLCompileError(0, ">= missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, ">= missing right arg"))
      _ <- requireNonOptional2(args, ">=", 0)
      _ <- requireNumeric2(args, ">=", 0)
    yield GreaterThanOrEqualFn(left, right)

object CLessThanOrEqualFn extends CompileFn[LessThanOrEqualFn] with CompileChecks:
  def build(args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left <- args.headOption.toRight(DLCompileError(0, "<= missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "<= missing right arg"))
      _ <- requireNonOptional2(args, "<=", 0)
      _ <- requireNumeric2(args, "<=", 0)
    yield LessThanOrEqualFn(left, right)

object CNegateFn extends CompileFn[NegateFn] with CompileChecks:
  def build(args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, NegateFn] =
    for
      arg <- args.headOption.toRight(DLCompileError(0, "unary - missing operand"))
      tpe <- Utility.rhsType(arg).toRight(DLCompileError(0, "unary - cannot determine operand type"))
      _ <- if tpe.isNumeric then Right(())
      else Left(DLCompileError(0, s"unary - requires a numeric operand, found ${tpe.typeName}"))
    yield NegateFn(arg)

object CEqualFn extends CompileFn[EqualFn] with CompileChecks:
  def build(args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left <- args.headOption.toRight(DLCompileError(0, "== missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "== missing right arg"))
    yield EqualFn(left, right)

object CNotEqualFn extends CompileFn[NotEqualFn] with CompileChecks:
  def build(args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left <- args.headOption.toRight(DLCompileError(0, "!= missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "!= missing right arg"))
    yield NotEqualFn(left, right)

// ==== Boolean Fns ====

object CAndFn extends CompileFn[AndFn] with CompileChecks:
  def build(args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, AndFn] =
    for
      left  <- args.headOption.toRight(DLCompileError(0, "&& missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "&& missing right arg"))
      _     <- requireBoolean2(args, "&&", 0)
      lBool <- left match
        case b: BooleanFn => Right(b)
        case _ => Left(DLCompileError(0, "&& left side is not a boolean expression"))
      rBool <- right match
        case b: BooleanFn => Right(b)
        case _ => Left(DLCompileError(0, "&& right side is not a boolean expression"))
    yield AndFn(lBool, rBool)

object COrFn extends CompileFn[OrFn] with CompileChecks:
  def build(args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, OrFn] =
    for
      left  <- args.headOption.toRight(DLCompileError(0, "|| missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "|| missing right arg"))
      _     <- requireBoolean2(args, "||", 0)
      lBool <- left match
        case b: BooleanFn => Right(b)
        case _ => Left(DLCompileError(0, "|| left side is not a boolean expression"))
      rBool <- right match
        case b: BooleanFn => Right(b)
        case _ => Left(DLCompileError(0, "|| right side is not a boolean expression"))
    yield OrFn(lBool, rBool)

object CNotFn extends CompileFn[NotFn] with CompileChecks:
  def build(args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, NotFn] =
    for
      arg <- args.headOption.toRight(DLCompileError(0, "! missing arg"))
      _   <- requireBoolean1(arg, "!", 0)
      b   <- arg match
        case bool: BooleanFn => Right(bool)
        case _ => Left(DLCompileError(0, "! operand is not a boolean expression"))
    yield NotFn(b)