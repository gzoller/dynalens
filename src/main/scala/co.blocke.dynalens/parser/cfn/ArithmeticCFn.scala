package co.blocke.dynalens
package parser
package cfn

import fn.*

/** Shared logic for numeric binary / unary operators. */
trait ArithmeticCFn extends CompileFn:

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

  /** Phase 2 semantic verification. */
  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    val recvFT = Utility.rhsType(fn.recv) match
      case TypeResult.Known(ft) => ft
      case _ => ScalarType("", "scala.Any")

    val argFTs = fn.args.flatMap {
      case a => Utility.rhsType(a) match
        case TypeResult.Known(ft) => Some(ft)
        case _ => None
    }
    if argFTs.isEmpty then
      Left(DLCompileError(ctx.posStr, s"Cannot determine operand type(s) used with operator '$name'"))
    else
      argFTs.foldLeft(checkOperand(recvFT))((acc, ft) => acc.flatMap(_ => checkOperand(ft)))


/** ---------------------------------
 *  Addition (+)
 *  ---------------------------------
 */
object CPlusFn extends CompileFn:
  val name = "+"
  // builtIn intentionally omitted per API update
  override val minArgs = 1
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    receiver.ftype.isStringLike || Validation.isNumericType(receiver.ftype)

  override def build(receiver: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, Fn[Any]] =
    args match
      case arg :: Nil =>
        var recvType = receiver.ftype
        if recvType.typeName == "scala.Any" then
          Utility.rhsType(receiver.fn) match
            case TypeResult.Known(ft) =>
              println("[CPlusFn.build] Re-resolved receiver type via Utility.rhsType: " + ft)
              recvType = ft
            case _ =>
              () // or log a warning if you want visibility
        // prefer rhsType for correct inference on constants/expressions
        val argType: FieldType = Utility.rhsType(arg) match
          case TypeResult.Known(ft) =>
            println(s"[CPlusFn] argType inferred from Utility.rhsType → $ft")
            ft
          case _ =>
            println(s"[CPlusFn] inferring argType from ${arg.getClass.getSimpleName}.resultType → ${arg.resultType}")
            arg.resultType

        if recvType.isNumeric && argType.isNumeric then
          Utility.calcNumericResultType(ctx.posStr, recvType, argType, Utility.ArithOp.Add)
            .map(rt => AddFn(receiver.fn, arg, rt, ctx.posStr))
        else if recvType.isStringLike || argType.isStringLike then
          Right(ConcatFn(receiver.fn, args, ctx.posStr).asInstanceOf[Fn[Any]])
        else
          Left(
            DLCompileError(
              ctx.posStr,
              s"Operator '+' not valid between ${recvType.typeName} and ${argType.typeName}"
            )
          )

      case _ =>
        Left(DLCompileError(ctx.posStr, s"'$name' expects exactly one argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    // Diagnostic instrumentation for operand types
    println(s"CPlusFn.validate called with fn: $fn")
    println(s"Receiver type info: ${Utility.rhsType(fn.recv)}")
    if fn.args.nonEmpty then
      println(s"Argument type info: ${Utility.rhsType(fn.args.head)}")
    else
      println("No arguments present")

    fn match
      case a: AddFn =>
        (Utility.rhsType(a.recv), Utility.rhsType(a.args.head)) match
          case (TypeResult.Known(lt), TypeResult.Known(rt)) if lt.isNumeric && rt.isNumeric => Right(())
          case (TypeResult.Known(lt), TypeResult.Known(rt)) =>
            Left(DLCompileError(ctx.posStr, s"+ requires numeric operands, found ${lt.typeName} and ${rt.typeName}"))
          case (TypeResult.Error(e), _) => Left(e)
          case (_, TypeResult.Error(e)) => Left(e)
          case _ => Left(DLCompileError(ctx.posStr, "+ cannot determine operand types"))
      case c: ConcatFn =>
        (Utility.rhsType(c.recv), Utility.rhsType(c.args.head)) match
          case (TypeResult.Known(lt), TypeResult.Known(rt)) if lt.isStringLike || rt.isStringLike => Right(())
          case (TypeResult.Known(lt), TypeResult.Known(rt)) =>
            Left(DLCompileError(ctx.posStr, s"+ requires string-like operands for concat, found ${lt.typeName} and ${rt.typeName}"))
          case (TypeResult.Error(e), _) => Left(e)
          case (_, TypeResult.Error(e)) => Left(e)
          case _ => Left(DLCompileError(ctx.posStr, "+ cannot determine operand types"))
      case _ =>
        Left(DLCompileError(ctx.posStr, "Unexpected Fn type in +"))


/** ---------------------------------
 *  Subtraction (-)
 *  ---------------------------------
 */
object CMinusFn extends CompileFn:
  val name = "-"
  // builtIn intentionally omitted per API update
  override val minArgs = 0
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Validation.isNumericType(receiver.ftype)

  override def build(receiver: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, Fn[Any]] =
    args match
      case Nil =>
        // Unary minus: just receiver
        println(s"CMinusFn.build (unary): receiver type = ${receiver.ftype}")
        Right(
          NegateFn(
            receiver.fn,
            ctx.posStr
          ).asInstanceOf[Fn[Any]])
      case arg :: Nil =>
        // Infer arg type using rhsType for consistency
        val recvType = receiver.ftype
        val argType: FieldType =
          Utility.rhsType(arg) match
            case TypeResult.Known(ft) => ft
            case _ => ctx.resolveSchemaFor(arg)

        println(s"CMinusFn.build (binary): receiver type = $recvType, arg type = $argType")

        Utility.calcNumericResultType(ctx.posStr, recvType, argType, Utility.ArithOp.Sub)
          .map(rt =>
            SubtractFn(
              receiver.fn,
              arg,
              rt,
              ctx.posStr
            ).asInstanceOf[Fn[Any]]
          )
      case _ =>
        Left(DLCompileError(ctx.posStr, "Operator '-' expects 0 or 1 argument(s)"))

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    fn match
      case n: NegateFn =>
        val ft = ctx.resolveSchemaFor(n.recv)
        if ft.isNumeric && !ft.isOptional then
          Right(())
        else if !ft.isNumeric then
          Left(DLCompileError(ctx.posStr, "Unary '-' requires numeric receiver"))
        else if ft.isOptional then
          Left(DLCompileError(ctx.posStr, "Unary '-' cannot apply to optional"))
        else
          Left(DLCompileError(ctx.posStr, "Cannot determine receiver type for unary '-'"))

      case s: SubtractFn =>
        (Utility.rhsType(s.recv), Utility.rhsType(s.args.head)) match
          case (TypeResult.Known(l), TypeResult.Known(r))
            if l.isNumeric && r.isNumeric &&
              !l.isOptional && !r.isOptional =>
            Right(())
          case (TypeResult.Known(_), TypeResult.Known(_)) =>
            Left(DLCompileError(ctx.posStr, "Binary '-' requires non-optional numeric operands"))
          case (TypeResult.Error(e), _) => Left(e)
          case (_, TypeResult.Error(e)) => Left(e)
          case _ => Left(DLCompileError(ctx.posStr, "Binary '-' cannot determine operand types"))

      case _ =>
        Left(DLCompileError(ctx.posStr, "Invalid Fn for '-'"))


/** ---------------------------------
 *  Multiplication (*)
 *  ---------------------------------
 */
object CMultiplyFn extends ArithmeticCFn:
  override def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Validation.isNumericType(receiver.ftype)
  val name = "*"

  override def build(receiver: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, MultiplyFn] =
    println(s"[CMultiplyFn.build] recvType=${receiver.ftype}, argType=${args.head.resultType}")
    args match
      case arg :: Nil =>
        val recvType = receiver.ftype
        val argType: FieldType =
          Utility.rhsType(args.head) match
            case TypeResult.Known(ft) => ft
            case _ => ctx.resolveSchemaFor(args.head)

        if !recvType.isNumeric || !argType.isNumeric then
          Left(DLCompileError(ctx.posStr, s"Operator '*' requires numeric operands, found ${recvType.typeName} and ${argType.typeName}"))
        else
          Utility.calcNumericResultType(ctx.posStr, recvType, argType, Utility.ArithOp.Mul)
            .map(rt => MultiplyFn(receiver.fn, arg, rt, ctx.posStr))
      case _ =>
        Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))


/** ---------------------------------
 *  Division (/)
 *  ---------------------------------
 */
object CDivideFn extends ArithmeticCFn:
  override def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Validation.isNumericType(receiver.ftype)
  val name = "/"

  override def build(receiver: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, DivideFn] =
    args match
      case arg :: Nil =>
        val recvType = receiver.ftype
        val argType: FieldType =
          Utility.rhsType(args.head) match
            case TypeResult.Known(ft) => ft
            case _ => ctx.resolveSchemaFor(args.head)
        if !recvType.isNumeric || !argType.isNumeric then
          Left(DLCompileError(ctx.posStr, s"Operator '/' requires numeric operands, found ${recvType.typeName} and ${argType.typeName}"))
        else
          Utility.calcNumericResultType(ctx.posStr, recvType, argType, Utility.ArithOp.Div)
            .map(rt => DivideFn(receiver.fn, arg, rt, ctx.posStr))
      case _ =>
        Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))


/** ---------------------------------
 *  Modulus (%)
 *  ---------------------------------
 */
object CModulusFn extends ArithmeticCFn:
  val name = "%"

  override def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Validation.isNumericType(receiver.ftype)

  override def build(receiver: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, ModuloFn] =
    args match
      case arg :: Nil =>
        val recvType = receiver.ftype
        val argType: FieldType =
          Utility.rhsType(args.head) match
            case TypeResult.Known(ft) => ft
            case _ => ctx.resolveSchemaFor(args.head)
        if !recvType.isNumeric || !argType.isNumeric then
          Left(DLCompileError(ctx.posStr, s"Operator '%' requires numeric operands, found ${recvType.typeName} and ${argType.typeName}"))
        else
          Utility.calcNumericResultType(ctx.posStr, recvType, argType, Utility.ArithOp.Mod)
            .map(rt => ModuloFn(receiver.fn, arg, rt, ctx.posStr))
      case _ =>
        Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
