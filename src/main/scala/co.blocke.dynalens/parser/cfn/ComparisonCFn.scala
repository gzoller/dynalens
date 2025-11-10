package co.blocke.dynalens
package parser
package cfn


import fn.*


/**
 * Base for all comparison-style Boolean CFns (<, >, <=, >=, ==, !=).
 * Handles the two-phase validation pattern (defer until both operand types known),
 * and standard numeric + non-optional enforcement.
 */
trait ComparisonCFn extends CompileFn:

  override val minArgs: Int = 1
  override val maxArgs: Int = 1
  override val standalone = false

  /** Return true if the given FieldType represents a type that supports <, >, <=, >=, ==, != */
  private def isComparableType(ft: FieldType): Boolean =
    ft match
      case ScalarType(_, t, _) =>
        val norm = Utility.normalizeNumeric(t)
        val result = Set(
          "scala.Byte",
          "scala.Short",
          "scala.Int",
          "scala.Long",
          "scala.Float",
          "scala.Double",
          "scala.math.BigInt",
          "scala.math.BigDecimal",
          "scala.Boolean",
          "java.lang.String",
          "java.util.Date"
        ).contains(norm)
        result
      case _ =>
        false

  protected def checkOperand(ft: FieldType)(using ctx: ExprContext): Either[DLCompileError, Unit] =
    if !isComparableType(ft) then
      Left(DLCompileError(ctx.posStr,
        s"Operator '$name' requires comparable operands, found ${ft.typeName}"
      ))
    else if ft.isOptional then
      Left(DLCompileError(ctx.posStr,
        s"Operator '$name' cannot be applied to optional values of type ${ft.typeName}. " +
          "Use `.else(default)` or check `isDefined()` first."
      ))
    else
      Right(())

  /** Ensure two FieldTypes are mutually comparable: either both numeric or exactly the same comparable type */
  protected def ensureComparableTypes(ft1: FieldType, ft2: FieldType)(using ctx: ExprContext): Either[DLCompileError, Unit] =
    val norm1 = Utility.normalizeNumeric(ft1.typeName)
    val norm2 = Utility.normalizeNumeric(ft2.typeName)
    val numericTypes = Set(
      "scala.Byte",
      "scala.Short",
      "scala.Int",
      "scala.Long",
      "scala.Float",
      "scala.Double",
      "scala.math.BigInt",
      "scala.math.BigDecimal"
    )
    val comparableTypes = Set(
      "scala.Boolean",
      "java.lang.String",
      "java.util.Date"
    )
    val bothNumeric = numericTypes.contains(norm1) && numericTypes.contains(norm2)
    val bothSameComparable = (norm1 == norm2) && (numericTypes.contains(norm1) || comparableTypes.contains(norm1))
    if bothNumeric || bothSameComparable then Right(())
    else Left(DLCompileError(ctx.posStr,
      s"Operator '$name' requires operands to be mutually comparable types, found ${ft1.typeName} and ${ft2.typeName}"
    ))

  override def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    val rt = Utility.rhsType(receiver.fn)
    rt match
      case TypeResult.Known(ft) =>
        checkOperand(ft).isRight
      case _ =>
        false

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    val recvFTOpt = Utility.rhsType(fn.recv)
    val argFTOpts = fn.args.map(Utility.rhsType)
    (recvFTOpt, argFTOpts) match
      case (TypeResult.Known(recvFT), args) if args.forall(_.isInstanceOf[TypeResult.Known[?]]) =>
        val argFTs = args.collect { case TypeResult.Known(ft) => ft }
        for {
          _ <-
            val co = checkOperand(recvFT)
            co
          _ <-
            argFTs.foldLeft[Either[DLCompileError, Unit]](Right(()))((acc, ft) =>
              acc.flatMap { _ =>
                val co = checkOperand(ft)
                co
              }
            )
          _ <-
            val ec = ensureComparableTypes(recvFT, argFTs.head)
            ec
        } yield ()
      case _ =>
        Left(DLCompileError(ctx.posStr, s"Cannot determine operand type(s) for '$name'"))


object CLessThanFn extends ComparisonCFn:
  val name = "<"
  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else Right(LessThanFn(recv.fn, args.head, ctx.posStr).asInstanceOf[Fn[Any]])


object CLessThanOrEqualFn extends ComparisonCFn:
  val name = "<="
  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else Right(LessThanOrEqualFn(recv.fn, args.head, ctx.posStr).asInstanceOf[Fn[Any]])


object CGreaterThanFn extends ComparisonCFn:
  val name = ">"
  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext)
  : Either[DLCompileError, Fn[Any]] =
    if args.size != 1 then
      Left(DLCompileError(ctx.posStr, s"'>' expects 1 argument"))
    else
      val lhs = recv.fn
      val rhs = args.head
      Right(GreaterThanFn(lhs, rhs, ctx.posStr).asInstanceOf[Fn[Any]])


object CGreaterThanOrEqualFn extends ComparisonCFn:
  val name = ">="
  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else Right(GreaterThanOrEqualFn(recv.fn, args.head, ctx.posStr).asInstanceOf[Fn[Any]])


object CEqualFn extends ComparisonCFn:
  val name = "=="
  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else Right(EqualFn(recv.fn, args.head, ctx.posStr).asInstanceOf[Fn[Any]])


object CNotEqualFn extends ComparisonCFn:
  val name = "!="
  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else Right(NotEqualFn(recv.fn, args.head, ctx.posStr).asInstanceOf[Fn[Any]])