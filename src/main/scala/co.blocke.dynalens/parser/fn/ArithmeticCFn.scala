package co.blocke.dynalens
package parser
package fn

import co.blocke.dynalens.fn.*

/** Shared logic for numeric binary / unary operators. */
trait ArithmeticCFn[R <: Fn[?]] extends CompileFn[R]:

  override val minArgs: Int = 1
  override val maxArgs: Int = 1

  /** Strong, user-facing errors for arithmetic operands. */
  protected def checkOperand(ft: FieldType)(using ctx: ExprContext): Either[DLCompileError, Unit] =
    if !Validation.isNumericType(ft) then
      Left(DLCompileError(ctx.posStr, s"Operator '$name' requires numeric operand(s), found ${ft.typeName}"))
    else if ft.isOptional then
      Left(DLCompileError(ctx.posStr, s"Operator '$name' cannot be applied to optional value of type ${ft.typeName}. Use `.else(default)` or check `isDefined()` first."))
    else
      Right(())

  /** Phase 0/1 accept check using Receiver context. */
  override def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Validation.isNumericType(receiver.ftype)

  /** Numeric promotion always via receiver + args. */
  override def resultType(recv: Receiver, argTypes: List[FieldType])(using ctx: ExprContext): FieldType =
    val typeNames = recv.ftype.typeName :: argTypes.map(_.typeName)
    ScalarType("", Utility.numericPromote(typeNames*))

  /** Phase 2 semantic verification. */
  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    val recvFTOpt = Utility.receiverFieldTypeOf(fn.recv)
    val argFTOpts = fn.args.map(Utility.receiverFieldTypeOf(_))
    if recvFTOpt.isEmpty || argFTOpts.exists(_.isEmpty) then
      Left(DLCompileError(ctx.posStr, s"Cannot determine operand type(s) used with operator '$name'"))
    else
      val recvFT = recvFTOpt.get
      val argFTs = argFTOpts.flatten
      argFTs.foldLeft(checkOperand(recvFT))((acc, ft) => acc.flatMap(_ => checkOperand(ft)))


/** ---------------------------------
 *  Addition (+)
 *  ---------------------------------
 */
object CPlusFn extends CompileFn[Fn[?]]:
  val name = "+"
  // builtIn intentionally omitted per API update
  override val minArgs = 1
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    receiver.ftype.isStringLike || Validation.isNumericType(receiver.ftype)

  override def resultType(recv: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    val recvType = recv.ftype
    val argTypeOpt = args.headOption

    if recvType.isNumeric && argTypeOpt.exists(_.isNumeric) then
      val names = recvType.typeName :: args.map(_.typeName)
      ScalarType("", Utility.numericPromote(names *))
    else if recvType.isStringLike || argTypeOpt.exists(_.isStringLike) then
      ScalarType("", "java.lang.String")
    else
      ScalarType("", "java.lang.Object") // fallback for mixed/unknown types

  override def build(receiver: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, Fn[?]] =
    if args.size != 1 then
      Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else
      val recvType  = receiver.ftype
      val argFn     = args.head
      val argTypeOp = Utility.receiverFieldTypeOf(argFn)

      if recvType.isNumeric && argTypeOp.exists(_.isNumeric) then
        Right(AddFn(receiver.fn, args, ctx.posStr))
      else if recvType.isStringLike || argTypeOp.exists(_.isStringLike) then
        Right(ConcatFn(receiver.fn, args, ctx.posStr).asInstanceOf[Fn[?]])
      else
        Left(DLCompileError(ctx.posStr,
          s"Operator '+' not valid between ${recvType.typeName} and ${argTypeOp.map(_.typeName).getOrElse("unknown")}"))

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    fn match
      case a: AddFn =>
        (Utility.rhsType(a.recv), Utility.rhsType(a.args.head)) match
          case (Some(lt), Some(rt)) if lt.isNumeric && rt.isNumeric => Right(())
          case (Some(lt), Some(rt)) =>
            Left(DLCompileError(ctx.posStr, s"+ requires numeric operands, found ${lt.typeName} and ${rt.typeName}"))
          case _ => Left(DLCompileError(ctx.posStr, "+ cannot determine operand types"))
      case c: ConcatFn =>
        (Utility.rhsType(c.recv), Utility.rhsType(c.args.head)) match
          case (Some(lt), Some(rt)) if lt.isStringLike || rt.isStringLike => Right(())
          case (Some(lt), Some(rt)) =>
            Left(DLCompileError(ctx.posStr, s"+ requires string-like operands for concat, found ${lt.typeName} and ${rt.typeName}"))
          case _ => Left(DLCompileError(ctx.posStr, "+ cannot determine operand types"))
      case _ =>
        Left(DLCompileError(ctx.posStr, "Unexpected Fn type in +"))


/** ---------------------------------
 *  Subtraction (-)
 *  ---------------------------------
 */
object CMinusFn extends CompileFn[Fn[?]]:
  val name = "-"
  // builtIn intentionally omitted per API update
  override val minArgs = 0
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Validation.isNumericType(receiver.ftype)

  override def resultType(recv: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    args match
      case Nil =>
        ScalarType("", recv.ftype.typeName)
      case _ =>
        val names = recv.ftype.typeName :: args.map(_.typeName)
        ScalarType("", Utility.numericPromote(names*))

  override def build(receiver: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, Fn[?]] =
    args match
      case Nil      => Right(NegateFn(receiver.fn, ctx.posStr))
      case _ :: Nil => Right(SubtractFn(receiver.fn, args, ctx.posStr))
      case _        => Left(DLCompileError(ctx.posStr, "Operator '-' expects 0 or 1 argument(s)"))

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    fn match
      case n: NegateFn =>
        Utility.receiverFieldTypeOf(n.recv) match
          case Some(ft) if ft.isNumeric && !ft.isOptional =>
            Right(())
          case Some(ft) if !ft.isNumeric =>
            Left(DLCompileError(ctx.posStr, "Unary '-' requires numeric receiver"))
          case Some(ft) =>
            Left(DLCompileError(ctx.posStr, "Unary '-' cannot apply to optional"))
          case None =>
            Left(DLCompileError(ctx.posStr, "Cannot determine receiver type for unary '-'"))

      case s: SubtractFn =>
        (Utility.rhsType(s.recv), Utility.rhsType(s.args.head)) match
          case (Some(l), Some(r))
            if l.isNumeric && r.isNumeric &&
              !l.isOptional && !r.isOptional =>
            Right(())
          case (Some(_), Some(_)) =>
            Left(DLCompileError(ctx.posStr, "Binary '-' requires non-optional numeric operands"))
          case _ =>
            Left(DLCompileError(ctx.posStr, "Binary '-' cannot determine operand types"))

      case _ =>
        Left(DLCompileError(ctx.posStr, "Invalid Fn for '-'"))


/** ---------------------------------
 *  Multiplication (*)
 *  ---------------------------------
 */
object CMultiplyFn extends ArithmeticCFn[MultiplyFn]:
  override def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Validation.isNumericType(receiver.ftype)
  val name = "*"
  override def build(receiver: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, MultiplyFn] =
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else Right(MultiplyFn(receiver.fn, args, ctx.posStr))


/** ---------------------------------
 *  Division (/)
 *  ---------------------------------
 */
object CDivideFn extends ArithmeticCFn[DivideFn]:
  override def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Validation.isNumericType(receiver.ftype)
  val name = "/"
  override def build(receiver: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, DivideFn] =
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else Right(DivideFn(receiver.fn, args, ctx.posStr))


/** ---------------------------------
 *  Modulus (%)
 *  ---------------------------------
 */
object CModulusFn extends ArithmeticCFn[ModuloFn]:
  val name = "%"
  override def build(receiver: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, ModuloFn] =
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else Right(ModuloFn(receiver.fn, args, ctx.posStr))
  override def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Validation.isNumericType(receiver.ftype)