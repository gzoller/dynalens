package co.blocke.dynalens
package parser
package fn


import co.blocke.dynalens.fn.*


/**
 * Base for all comparison-style Boolean CFns (<, >, <=, >=, ==, !=).
 * Handles the two-phase validation pattern (defer until both operand types known),
 * and standard numeric + non-optional enforcement.
 */
trait ComparisonCFn[R <: Fn[?]] extends CompileFn[R]:

  override val minArgs: Int = 1
  override val maxArgs: Int = 1
  override val standalone = false

  protected def checkOperand(ft: FieldType)(using ctx: ExprContext): Either[DLCompileError, Unit] =
    if !Utility.isComparableType(ft) then
      Left(DLCompileError(ctx.posStr,
        s"Operator '$name' requires comparable operands, found ${ft.typeName}"
      ))
    else if CompileFn.isOptionalType(ft) then
      Left(DLCompileError(ctx.posStr,
        s"Operator '$name' cannot be applied to optional values of type ${ft.typeName}. " +
          "Use `.else(default)` or check `isDefined()` first."
      ))
    else Right(())

  override def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn) match
      case Some(ft) => checkOperand(ft).isRight
      case None => false

  override def resultType(recv: Receiver, argTypes: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "scala.Boolean")

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    val recvFTOpt = Utility.rhsType(fn.recv)
    val argFTOpts = fn.args.map(Utility.rhsType)
    if recvFTOpt.isEmpty || argFTOpts.exists(_.isEmpty) then
      Left(DLCompileError(ctx.posStr, s"Cannot determine operand type(s) for '$name'"))
    else
      for {
        _ <- checkOperand(recvFTOpt.get)
        _ <- argFTOpts.foldLeft[Either[DLCompileError, Unit]](Right(()))((acc, opt) =>
          acc.flatMap(_ => checkOperand(opt.get))
        )
      } yield ()


object CLessThanFn extends ComparisonCFn[LessThanFn]:
  val name = "<"
  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else Right(LessThanFn(recv.fn, args.head, ctx.posStr))


object CLessEqualFn extends ComparisonCFn[LessThanOrEqualFn]:
  val name = "<="
  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else Right(LessThanOrEqualFn(recv.fn, args.head, ctx.posStr))


object CGreaterThanFn extends ComparisonCFn[GreaterThanFn]:
  val name = ">"
  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else Right(GreaterThanFn(recv.fn, args.head, ctx.posStr))


object CGreaterEqualFn extends ComparisonCFn[GreaterThanOrEqualFn]:
  val name = ">="
  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else Right(GreaterThanOrEqualFn(recv.fn, args.head, ctx.posStr))


object CEqualFn extends ComparisonCFn[EqualFn]:
  val name = "=="
  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else Right(EqualFn(recv.fn, args.head, ctx.posStr))


object CNotEqualFn extends ComparisonCFn[NotEqualFn]:
  val name = "!="
  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else Right(NotEqualFn(recv.fn, args.head, ctx.posStr))