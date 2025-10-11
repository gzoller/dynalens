package co.blocke.dynalens
package parser
package fn

import co.blocke.dynalens.fn.*


// ---------------------- Trim ----------------------
object CTrimFn extends CompileFn[TrimFn]:
  val name = "trim"
  val minArgs = 0
  override val maxArgs = 0

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    receiver.ftype.isStringLike

  def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.lang.String")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, "trim() takes no arguments"))
    else Right(TrimFn(recv.fn, ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] = Right(())


// ---------------------- ToLower ----------------------
object CToLowerFn extends CompileFn[ToLowerFn]:
  val name = "toLower"
  val minArgs = 0
  override val maxArgs = 0

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    receiver.ftype.isStringLike

  def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.lang.String")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, "toLower() takes no arguments"))
    else Right(ToLowerFn(recv.fn, ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] = Right(())


// ---------------------- ToUpper ----------------------
object CToUpperFn extends CompileFn[ToUpperFn]:
  val name = "toUpper"
  val minArgs = 0
  override val maxArgs = 0

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    receiver.ftype.isStringLike

  def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.lang.String")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, "toUpper() takes no arguments"))
    else Right(ToUpperFn(recv.fn, ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] = Right(())


// ---------------------- Interpolate ----------------------
object CInterpolateFn extends CompileFn[InterpolateFn]:
  val name = "template"
  val minArgs = 0
  override val maxArgs = 0

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    receiver.ftype.isStringLike

  def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.lang.String")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, "template() takes no arguments"))
    else
      val varMap =
        recv.fn match
          case ConstantFn(s: String) =>
            TemplateUtils.extractVariables(s)
              .map(v => v -> GetFn(v, isOptional = false, ConstantFn(""), ctx.posStr))
              .toMap
          case _ =>
            Map.empty[String, Fn[Any]]
      Right(InterpolateFn(recv.fn, varMap, ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] = Right(())


// ---------------------- Substring ----------------------
object CSubstringFn extends CompileFn[SubstringFn]:
  val name = "substring"
  val minArgs = 2
  override val maxArgs = 2

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    receiver.ftype.isStringLike

  def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.lang.String")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      start <- args.headOption.toRight(DLCompileError(ctx.posStr, "substring() missing start arg"))
      end   <- args.lift(1).toRight(DLCompileError(ctx.posStr, "substring() missing end arg"))
    yield SubstringFn(recv.fn, start.asInstanceOf[Fn[Any]], Some(end.asInstanceOf[Fn[Any]]), ctx.posStr)

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case s: SubstringFn =>
        Validation.requireNumeric2(List(s.start.asInstanceOf[Fn[Any]], s.end.get.asInstanceOf[Fn[Any]]), "substring", ctx.posStr)
      case _ => Right(())


// ---------------------- Replace ----------------------
object CReplaceFn extends CompileFn[ReplaceFn]:
  val name = "replace"
  val minArgs = 2
  override val maxArgs = 2

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    receiver.ftype.isStringLike

  def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.lang.String")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    for
      target <- args.headOption.toRight(DLCompileError(ctx.posStr, "replace() missing target string"))
      repl   <- args.lift(1).toRight(DLCompileError(ctx.posStr, "replace() missing replacement string"))
    yield ReplaceFn(recv.fn, target, repl, ctx.posStr)

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case r: ReplaceFn =>
        val tgtType = Utility.rhsType(r.target)
        val repType = Utility.rhsType(r.replacement)
        (tgtType, repType) match
          case (Some(t1), Some(t2)) if t1.isStringLike && t2.isStringLike => Right(())
          case (Some(t1), _) => Left(DLCompileError(ctx.posStr, s"replace() target must be string, got ${t1.typeName}"))
          case (_, Some(t2)) => Left(DLCompileError(ctx.posStr, s"replace() replacement must be string, got ${t2.typeName}"))
          case _             => Left(DLCompileError(ctx.posStr, "replace() could not resolve operand types"))
      case _ => Right(())


// ---------------------- Concat ----------------------
object CConcatFn extends CompileFn[ConcatFn]:
  val name = "+"
  val minArgs = 1
  override val maxArgs = Int.MaxValue

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    receiver.ftype match
      case _: ScalarType => true
      case ft if ft.isStringLike => true
      case _ => false

  def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.lang.String")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    Right(ConcatFn(recv.fn, args, ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    fn match
      case c: ConcatFn =>
        val badArgs = (c.recv :: c.args).flatMap { f =>
          Utility.rhsType(f) match
            case Some(ft) if ft.isStringLike => None
            case Some(ft) => Some(ft.typeName)
            case None     => Some("unknown")
        }
        if badArgs.nonEmpty then
          Left(DLCompileError(ctx.posStr, s"concat (+) requires string-like args, found: ${badArgs.mkString(", ")}"))
        else Right(())
      case _ => Right(())