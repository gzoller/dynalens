package co.blocke.dynalens
package parser
package fn


object CLessThanFn extends CompileFn[LessThanFn]:
  val name     = "<"
  override val builtIn = true
  val minArgs  = 2
  override val maxArgs = 2

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean = true

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "scala.Boolean")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left  <- args.headOption.toRight(DLCompileError(0, "< missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "< missing right arg"))
    yield LessThanFn(left, right)

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case lt: LessThanFn =>
        for
          _ <- CompileFn.requireNonOptional2(List(lt.left, lt.right), "<", 0)
          _ <- CompileFn.requireNumeric2(List(lt.left, lt.right), "<", 0)
        yield ()
      case _ => Right(())

object CLessThanOrEqualFn extends CompileFn[LessThanOrEqualFn]:
  val name     = "<="
  override val builtIn = true
  val minArgs  = 2

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean = true
  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext) =
    ScalarType("", "scala.Boolean")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left  <- args.headOption.toRight(DLCompileError(0, "<= missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "<= missing right arg"))
    yield LessThanOrEqualFn(left, right)

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case le: LessThanOrEqualFn =>
        for
          _ <- CompileFn.requireNonOptional2(List(le.left, le.right), "<=", 0)
          _ <- CompileFn.requireNumeric2(List(le.left, le.right), "<=", 0)
        yield ()
      case _ => Right(())


object CGreaterThanFn extends CompileFn[GreaterThanFn]:
  val name = ">"
  override val builtIn = true
  val minArgs = 2

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean = true

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext) =
    ScalarType("", "scala.Boolean")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left <- args.headOption.toRight(DLCompileError(0, "> missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "> missing right arg"))
    yield GreaterThanFn(left, right)

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case gt: GreaterThanFn =>
        for
          _ <- CompileFn.requireNonOptional2(List(gt.left, gt.right), ">", 0)
          _ <- CompileFn.requireNumeric2(List(gt.left, gt.right), ">", 0)
        yield ()
      case _ => Right(())


object CGreaterThanOrEqualFn extends CompileFn[GreaterThanOrEqualFn]:
  val name = ">="
  override val builtIn = true
  val minArgs = 2

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean = true

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext) =
    ScalarType("", "scala.Boolean")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left <- args.headOption.toRight(DLCompileError(0, ">= missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, ">= missing right arg"))
    yield GreaterThanOrEqualFn(left, right)

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case ge: GreaterThanOrEqualFn =>
        for
          _ <- CompileFn.requireNonOptional2(List(ge.left, ge.right), ">=", 0)
          _ <- CompileFn.requireNumeric2(List(ge.left, ge.right), ">=", 0)
        yield ()
      case _ => Right(())


object CEqualFn extends CompileFn[EqualFn]:
  val name     = "=="
  override val builtIn = true
  val minArgs  = 2

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean = true
  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext) =
    ScalarType("", "scala.Boolean")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left  <- args.headOption.toRight(DLCompileError(0, "== missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "== missing right arg"))
    yield EqualFn(left, right)

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case eq: EqualFn =>
        for _ <- CompileFn.requireNonOptional2(List(eq.left, eq.right), "==", 0)
          yield ()
      case _ => Right(())


object CNotEqualFn extends CompileFn[NotEqualFn]:
  val name = "!="
  override val builtIn = true
  val minArgs = 2

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean = true

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext) =
    ScalarType("", "scala.Boolean")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      left <- args.headOption.toRight(DLCompileError(0, "!= missing left arg"))
      right <- args.lift(1).toRight(DLCompileError(0, "!= missing right arg"))
    yield NotEqualFn(left, right)

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case ne: NotEqualFn =>
        for _ <- CompileFn.requireNonOptional2(List(ne.left, ne.right), "!=", 0)
          yield ()
      case _ => Right(())

