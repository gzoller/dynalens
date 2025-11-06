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

  /** Numeric promotion always via receiver + args. */
  override def resultType(recv: Receiver, argTypes: List[FieldType])(using ctx: ExprContext): FieldType =
    val typeNames = recv.ftype.typeName :: argTypes.map(_.typeName)
    ScalarType("", RuntimeUtil.numericPromote(typeNames*))

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

  override def resultType(recv: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    // Extra normalization: re-resolve receiver type if it's still scala.Any
    var recvType = recv.ftype
    if recvType.typeName == "scala.Any" then
      Utility.rhsType(recv.fn) match
        case TypeResult.Known(ft) =>
          recvType = ft
        case _ =>
          ()
    val argType: FieldType = args.headOption.getOrElse(ScalarType("", "scala.Any"))
    // Use Utility helpers for numeric and string checks
    if Utility.isNumeric(recvType.typeName) && Utility.isNumeric(argType.typeName) then
      if Utility.isFloating(recvType.typeName) || Utility.isFloating(argType.typeName) then
        ScalarType("", Utility.promoteGeneral(recvType.typeName, argType.typeName))
      else
        ScalarType("", Utility.promoteIntegral(recvType.typeName, argType.typeName))
    else if Utility.isStringLike(recvType.typeName) || Utility.isStringLike(argType.typeName) then
      ScalarType("", "java.lang.String")
    else
      ScalarType("", "java.lang.Object")

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
              println("[CPlusFn.build] Receiver still unresolved; using scala.Any")
              ()
        // prefer rhsType for correct inference on constants/expressions
        val argType: FieldType = Utility.rhsType(arg) match
          case TypeResult.Known(ft) =>
            println(s"[CPlusFn] argType inferred from Utility.rhsType → $ft")
            ft
          case _ =>
            arg match
              case f: Fn[?] =>
                println(s"[CPlusFn] inferring argType from ${f.getClass.getSimpleName}.resultType → ${f.resultType}")
                f.resultType
              case _ =>
                val resolved = ctx.resolveSchemaFor(arg)
                println(s"[CPlusFn] fallback to ctx.resolveSchemaFor → $resolved (for $arg)")
                resolved

        if recvType.isNumeric && argType.isNumeric then
          Right(AddFn(receiver.fn, args, ctx.posStr))
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

  override def resultType(recv: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    args match
      case Nil =>
        ScalarType("", recv.ftype.typeName)
      case _ =>
        val recvType = recv.ftype
        val argType = args.head
        if Utility.isNumeric(recvType.typeName) && Utility.isNumeric(argType.typeName) then
          if Utility.isFloating(recvType.typeName) || Utility.isFloating(argType.typeName) then
            ScalarType("", Utility.promoteGeneral(recvType.typeName, argType.typeName))
          else
            ScalarType("", Utility.promoteIntegral(recvType.typeName, argType.typeName))
        else
          val names = recvType.typeName :: args.map(_.typeName)
          ScalarType("", RuntimeUtil.numericPromote(names*))

  override def build(receiver: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, Fn[Any]] =
    args match
      case Nil =>
        // Unary minus: just receiver
        println(s"CMinusFn.build (unary): receiver type = ${receiver.ftype}")
        Right(NegateFn(receiver.fn, ctx.posStr).asInstanceOf[Fn[Any]])
      case arg :: Nil =>
        // Infer arg type using rhsType for consistency
        val recvType = receiver.ftype
        val argType: FieldType =
          Utility.rhsType(arg) match
            case TypeResult.Known(ft) => ft
            case _ => ctx.resolveSchemaFor(arg)
        println(s"CMinusFn.build (binary): receiver type = $recvType, arg type = $argType")
        Right(SubtractFn(receiver.fn, args, ctx.posStr).asInstanceOf[Fn[Any]])
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

  override def resultType(recv: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    val recvType = recv.ftype
    val argType = args.headOption.getOrElse(ScalarType("", "scala.Any"))
    if Utility.isNumeric(recvType.typeName) && Utility.isNumeric(argType.typeName) then
      if Utility.isFloating(recvType.typeName) || Utility.isFloating(argType.typeName) then
        ScalarType("", Utility.promoteGeneral(recvType.typeName, argType.typeName))
      else
        ScalarType("", Utility.promoteIntegral(recvType.typeName, argType.typeName))
    else
      ScalarType("", RuntimeUtil.numericPromote((recvType.typeName :: args.map(_.typeName))*))

  override def build(receiver: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, MultiplyFn] =
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else
      val recvType = receiver.ftype
      val argType: FieldType =
        Utility.rhsType(args.head) match
          case TypeResult.Known(ft) => ft
          case _ => ctx.resolveSchemaFor(args.head)
      if !recvType.isNumeric || !argType.isNumeric then
        Left(DLCompileError(ctx.posStr, s"Operator '*' requires numeric operands, found ${recvType.typeName} and ${argType.typeName}"))
      else
        Right(MultiplyFn(receiver.fn, args, ctx.posStr))


/** ---------------------------------
 *  Division (/)
 *  ---------------------------------
 */
object CDivideFn extends ArithmeticCFn:
  override def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Validation.isNumericType(receiver.ftype)
  val name = "/"

  override def resultType(recv: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    val recvType = recv.ftype
    val argType = args.headOption.getOrElse(ScalarType("", "scala.Any"))
    if Utility.isNumeric(recvType.typeName) && Utility.isNumeric(argType.typeName) then
      val bothIntegral = Utility.isIntegral(recvType.typeName) && Utility.isIntegral(argType.typeName)
      if bothIntegral then
        ScalarType("", Utility.promoteIntegral(recvType.typeName, argType.typeName))
      else
        ScalarType("", Utility.promoteGeneral(recvType.typeName, argType.typeName))
    else
      ScalarType("", "java.lang.Object")

  override def build(receiver: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, DivideFn] =
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else
      val recvType = receiver.ftype
      val argType: FieldType =
        Utility.rhsType(args.head) match
          case TypeResult.Known(ft) => ft
          case _ => ctx.resolveSchemaFor(args.head)
      if !recvType.isNumeric || !argType.isNumeric then
        Left(DLCompileError(ctx.posStr, s"Operator '/' requires numeric operands, found ${recvType.typeName} and ${argType.typeName}"))
      else
        Right(DivideFn(receiver.fn, args, ctx.posStr))


/** ---------------------------------
 *  Modulus (%)
 *  ---------------------------------
 */
object CModulusFn extends ArithmeticCFn:
  val name = "%"
  override def build(receiver: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, ModuloFn] =
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else
      val recvType = receiver.ftype
      val argType: FieldType =
        Utility.rhsType(args.head) match
          case TypeResult.Known(ft) => ft
          case _ => ctx.resolveSchemaFor(args.head)
      if !recvType.isNumeric || !argType.isNumeric then
        Left(DLCompileError(ctx.posStr, s"Operator '%' requires numeric operands, found ${recvType.typeName} and ${argType.typeName}"))
      else
        Right(ModuloFn(receiver.fn, args, ctx.posStr))
  override def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Validation.isNumericType(receiver.ftype)