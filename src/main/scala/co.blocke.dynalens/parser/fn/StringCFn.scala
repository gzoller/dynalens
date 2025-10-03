package co.blocke.dynalens
package parser
package fn

// ---------------------- Trim ----------------------
object CTrimFn extends CompileFn[TrimFn]:
  val name = "trim"
  val minArgs = 0
  override val maxArgs = 0

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver.isStringLike

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.lang.String")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(0, "trim() takes no arguments"))
    else Right(TrimFn(recv))

// ---------------------- ToLower ----------------------
object CToLowerFn extends CompileFn[ToLowerFn]:
  val name = "toLower"
  val minArgs = 0
  override val maxArgs = 0

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver.isStringLike

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.lang.String")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(0, "toLower() takes no arguments"))
    else Right(ToLowerFn(recv))

// ---------------------- ToUpper ----------------------
object CToUpperFn extends CompileFn[ToUpperFn]:
  val name = "toUpper"
  val minArgs = 0
  override val maxArgs = 0

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver.isStringLike

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.lang.String")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(0, "toUpper() takes no arguments"))
    else Right(ToUpperFn(recv))

// ---------------------- Interpolate ----------------------
object CInterpolateFn extends CompileFn[InterpolateFn]:
  val name = "template"
  val minArgs = 0
  override val maxArgs = 0

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver.isStringLike

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.lang.String")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(0, "template() takes no arguments"))
    else
      val varMap = recv match
        case ConstantFn(s: String) =>
          TemplateUtils.extractVariables(s).map(v => v -> GetFn(v, isOptional = false)).toMap
        case _ =>
          Map.empty[String, Fn[Any]] // defer if not constant
      Right(InterpolateFn(recv, varMap))

// ---------------------- Substring ----------------------
object CSubstringFn extends CompileFn[SubstringFn]:
  val name = "substring"
  val minArgs = 2
  override val maxArgs = 2

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver.isStringLike

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.lang.String")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      start <- args.headOption.toRight(DLCompileError(0, "substring missing start arg"))
      end <- args.lift(1).toRight(DLCompileError(0, "substring missing end arg"))
    yield SubstringFn(
      recv,
      start.asInstanceOf[Fn[Int]], // unchecked cast, type-checked later
      Some(end.asInstanceOf[Fn[Int]])
    )

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case s: SubstringFn =>
        CompileFn.requireNumeric2(List(s.start.asInstanceOf[Fn[Any]], s.end.get.asInstanceOf[Fn[Any]]), "substring", 0)
      case _ => Right(())


// ---------------------- Replace ----------------------
object CReplaceFn extends CompileFn[ReplaceFn]:
  val name = "replace"
  val minArgs = 2
  override val maxArgs = 2

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver.isStringLike

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.lang.String")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      target <- args.headOption.toRight(DLCompileError(0, "replace missing target string"))
      repl   <- args.lift(1).toRight(DLCompileError(0, "replace missing replacement string"))
    yield ReplaceFn(recv, target, repl)

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case r: ReplaceFn =>
        for
          _ <- Utility.rhsType(r.target) match
            case Some(ft) if ft.isStringLike => Right(())
            case Some(ft) => Left(DLCompileError(0, s"replace() target must be string, got ${ft.typeName}"))
            case None => Left(DLCompileError(0, "replace() cannot resolve target type"))
          _ <- Utility.rhsType(r.replacement) match
            case Some(ft) if ft.isStringLike => Right(())
            case Some(ft) => Left(DLCompileError(0, s"replace() replacement must be string, got ${ft.typeName}"))
            case None => Left(DLCompileError(0, "replace() cannot resolve replacement type"))
        yield ()
      case _ => Right(())