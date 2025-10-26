package co.blocke.dynalens
package parser
package cfn


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

  /** Return true if the given FieldType represents a type that supports <, >, <=, >=, ==, != */
  private def isComparableType(ft: FieldType): Boolean =
    println(s"[DEBUG] isComparableType called with ft=${ft.typeName}")
    ft match
      case ScalarType(_, t, _) =>
        val norm = Utility.normalizeNumeric(t)
        println(s"[DEBUG] normalized type: $norm")
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
        println(s"[DEBUG] comparable=$result for type=$norm")
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
    println(s"[DEBUG] ensureComparableTypes called with ft1=${ft1.typeName}, ft2=${ft2.typeName}")
    println(s"[DEBUG] normalized to norm1=$norm1, norm2=$norm2")
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
    println(s"[DEBUG] bothNumeric=$bothNumeric, bothSameComparable=$bothSameComparable")
    if bothNumeric || bothSameComparable then Right(())
    else Left(DLCompileError(ctx.posStr,
      s"Operator '$name' requires operands to be mutually comparable types, found ${ft1.typeName} and ${ft2.typeName}"
    ))

  override def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    val rt = Utility.rhsType(receiver.fn)
    println(s"[DEBUG] ${this.getClass.getSimpleName}.accepts recv=${receiver} rhsType=${rt}")
    rt match
      case TypeResult.Known(ft) =>
        val ok = checkOperand(ft).isRight
        println(s"[DEBUG] accepts -> checkOperand ok=$ok for ${ft.typeName}")
        ok
      case _ =>
        println(s"[DEBUG] accepts -> rhsType unknown (cannot resolve)")
        false

  override def resultType(recv: Receiver, argTypes: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "scala.Boolean")

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    val recvFTOpt = Utility.rhsType(fn.recv)
    val argFTOpts = fn.args.map(Utility.rhsType)
    println(s"[DEBUG] ${this.getClass.getSimpleName}.validate recvFT=$recvFTOpt argFTs=$argFTOpts")
    (recvFTOpt, argFTOpts) match
      case (TypeResult.Known(recvFT), args) if args.forall(_.isInstanceOf[TypeResult.Known[?]]) =>
        val argFTs = args.collect { case TypeResult.Known(ft) => ft }
        for {
          _ <-
            val co = checkOperand(recvFT)
            println(s"[DEBUG] validate -> checkOperand(recv) = ${co.isRight} (${recvFT.typeName})")
            co
          _ <-
            argFTs.foldLeft[Either[DLCompileError, Unit]](Right(()))((acc, ft) =>
              acc.flatMap { _ =>
                val co = checkOperand(ft)
                println(s"[DEBUG] validate -> checkOperand(arg) = ${co.isRight} (${ft.typeName})")
                co
              }
            )
          _ <-
            val ec = ensureComparableTypes(recvFT, argFTs.head)
            println(s"[DEBUG] validate -> ensureComparableTypes = ${ec.isRight}")
            ec
        } yield ()
      case _ =>
        println(s"[DEBUG] validate -> EARLY FAIL: missing or unknown types")
        Left(DLCompileError(ctx.posStr, s"Cannot determine operand type(s) for '$name'"))


object CLessThanFn extends ComparisonCFn[LessThanFn]:
  val name = "<"
  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    println(s"[DEBUG] CLessThanFn.build recv=${recv}, args=${args.map(_.getClass.getSimpleName)}")
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else Right(LessThanFn(recv.fn, args.head, ctx.posStr))


object CLessThanOrEqualFn extends ComparisonCFn[LessThanOrEqualFn]:
  val name = "<="
  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else Right(LessThanOrEqualFn(recv.fn, args.head, ctx.posStr))


object CGreaterThanFn extends ComparisonCFn[GreaterThanFn]:
  val name = ">"
  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.size != 1 then Left(DLCompileError(ctx.posStr, s"'$name' expects 1 argument"))
    else Right(GreaterThanFn(recv.fn, args.head, ctx.posStr))


object CGreaterThanOrEqualFn extends ComparisonCFn[GreaterThanOrEqualFn]:
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