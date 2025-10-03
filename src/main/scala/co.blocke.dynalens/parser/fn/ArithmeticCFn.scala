package co.blocke.dynalens
package parser
package fn


// ---------------------------------
// Addition (+)
// ---------------------------------
object CPlusFn extends CompileFn[Fn[Any]]:
  val name     = "+"
  override val builtIn = true
  val minArgs  = 2
  override val maxArgs = 2

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    true // '+' doesn't depend on the receiver

  def resultType(receiver: FieldType, args: List[FieldType])
                (using ctx: ExprContext): FieldType =
    (args.headOption, args.lift(1)) match
      case (Some(l), Some(r)) if l.isNumeric && r.isNumeric =>
        ScalarType("", Utility.numericPromote(l.typeName, r.typeName))
      case (Some(l), Some(r)) if l.isStringLike || r.isStringLike =>
        ScalarType("", "java.lang.String")
      case _ =>
        ScalarType("", "scala.Any")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left  <- args.headOption.toRight(DLCompileError(0, "+ missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "+ missing right arg"))
    yield {
      (Utility.rhsType(left), Utility.rhsType(right)) match
        case (Some(lt), Some(rt)) if lt.isStringLike || rt.isStringLike =>
          ConcatFn(List(left, right)).asInstanceOf[Fn[Any]]   // string concat
        case _ =>
          AddFn(left, right)      // numeric (default)
    }

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    fn match
      case AddFn(l, r) =>
        println(s"[validate +] AST=($l , $r)")
        (Utility.rhsType(l), Utility.rhsType(r)) match
          case (Some(lt), Some(rt)) if lt.isNumeric && rt.isNumeric =>
            Right(())
          case (Some(lt), Some(rt)) =>
            Left(DLCompileError(0,
              s"+ requires numeric operands, found ${lt.typeName} and ${rt.typeName}"))
          case _ =>
            Left(DLCompileError(0, "+ cannot determine operand types"))

      case ConcatFn(List(l, r)) =>
        (Utility.rhsType(l), Utility.rhsType(r)) match
          case (Some(lt), Some(rt)) if lt.isStringLike || rt.isStringLike =>
            Right(())
          case (Some(lt), Some(rt)) =>
            Left(DLCompileError(0,
              s"+ requires string-like operands for concat, found ${lt.typeName} and ${rt.typeName}"))
          case _ =>
            Left(DLCompileError(0, "+ cannot determine operand types"))

      case _ =>
        Left(DLCompileError(0, "Unexpected Fn type in +"))


// ---------------------------------
// Subtraction (-)
// ---------------------------------
object CMinusFn extends CompileFn[Fn[Any]]:
  val name = "-"
  override val builtIn = true

  val minArgs = 1
  override val maxArgs = 2

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean = true

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    args match
      case a :: Nil if a.isNumeric => a
      case l :: r :: Nil if l.isNumeric && r.isNumeric =>
        ScalarType("", Utility.numericPromote(l.typeName, r.typeName))
      case _ =>
        ScalarType("", "scala.Any")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    args match
      case a :: Nil         => Right(NegateFn(a))
      case l :: r :: Nil    => Right(SubtractFn(l, r))
      case _                => Left(DLCompileError(0, s"- requires 1 or 2 args, found ${args.size}"))

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    fn match
      case NegateFn(inner) =>
        CompileFn.requireNumeric1(inner, "-", 0)

      case SubtractFn(left, right) =>
        CompileFn.requireNumeric2(List(left, right), "-", 0)

      case _ =>
        Left(DLCompileError(0, "Invalid fn passed to CMinusFn"))


// ---------------------------------
// Multiplication (*)
// ---------------------------------
object CMultiplyFn extends CompileFn[MultiplyFn]:
  val name = "*"
  override val builtIn = true

  val minArgs = 2
  override val maxArgs = 2

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean = true

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    (args.headOption, args.lift(1)) match
      case (Some(l), Some(r)) if l.isNumeric && r.isNumeric =>
        ScalarType("", Utility.numericPromote(l.typeName, r.typeName))
      case _ =>
        ScalarType("", "scala.Any")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left  <- args.headOption.toRight(DLCompileError(0, "* missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "* missing right arg"))
    yield MultiplyFn(left, right)


  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    fn match
      case m: MultiplyFn =>
        for
          _ <- CompileFn.requireNonOptional2(List(m.left, m.right), "*", 0)
          _ <- CompileFn.requireNumeric2(List(m.left, m.right), "*", 0)
        yield ()
      case _ =>
        Right(()) // not my responsibility

// ---------------------------------
// Division (/)
// ---------------------------------
object CDivideFn extends CompileFn[DivideFn]:
  val name = "/"
  override val builtIn = true

  val minArgs = 2
  override val maxArgs = 2

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean = true

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    (args.headOption, args.lift(1)) match
      case (Some(l), Some(r)) if l.isNumeric && r.isNumeric =>
        // division always promotes to widest numeric type
        ScalarType("", Utility.numericPromote(l.typeName, r.typeName))
      case _ =>
        ScalarType("", "scala.Any")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left  <- args.headOption.toRight(DLCompileError(0, "/ missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "/ missing right arg"))
    yield DivideFn(left, right)

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case d: DivideFn =>
        for
          _ <- CompileFn.requireNonOptional2(List(d.left, d.right), "/", 0)
          _ <- CompileFn.requireNumeric2(List(d.left, d.right), "/", 0)
        yield ()
      case _ => Right(())

// ---------------------------------
// Modulus (%)
// ---------------------------------
object CModulusFn extends CompileFn[ModuloFn]:
  val name = "%"
  override val builtIn = true

  val minArgs = 2
  override val maxArgs = 2

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean = true

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    (args.headOption, args.lift(1)) match
      case (Some(l), Some(r)) if l.isNumeric && r.isNumeric =>
        // Modulus keeps the "narrowest" numeric type normally,
        // but we can stay consistent with DivideFn and promote.
        ScalarType("", Utility.numericPromote(l.typeName, r.typeName))
      case _ =>
        ScalarType("", "scala.Any")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left <- args.headOption.toRight(DLCompileError(0, "% missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "% missing right arg"))
    yield ModuloFn(left, right)

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case m: ModuloFn =>
        for
          _ <- CompileFn.requireNonOptional2(List(m.left, m.right), "%", 0)
          _ <- CompileFn.requireNumeric2(List(m.left, m.right), "%", 0)
        yield ()
      case _ => Right(())
