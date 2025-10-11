package co.blocke.dynalens
package parser
package fn

import co.blocke.dynalens.fn.*

trait MathCFn[F <: Fn[Any]] extends CompileFn[F]:
  override val minArgs = 0

  /** Determines if the receiver is acceptable (List or Option[List] of numeric) */
  override def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    receiver match
      // Plain List of numeric elements
      case ListType(_, elem: FieldType, _, _) if Validation.isNumericType(elem) => true

      // Optional[List] of numeric elements
      case OptionType(_, ListType(_, elem: FieldType, _, _), _) if Validation.isNumericType(elem) => true

      // Anything else — reject
      case _ => false

  /** Build boilerplate: all math fns take no args */
  override def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, F] =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, s"$name() takes no arguments"))
    else Right(construct(recv.fn, posStr = ctx.posStr))

  /** Each subclass implements this to create its runtime Fn */
  def construct(recv: Fn[Any], posStr: String): F

  /** Determines the resulting scalar type (list element or numeric fallback) */
  private def resultElemType(receiver: Receiver)(using ctx: ExprContext): FieldType =
    receiver match
      case ListType(_, elem: FieldType, _, _) => elem
      case OptionType(_, ListType(_, elem: FieldType, _, _), _) => elem
      case other => Utility.rhsType(other.fn)(using ctx).getOrElse(ScalarType("", "scala.Double"))

  /** Default resultType: subclasses can override if needed */
  override def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    resultElemType(receiver)

  /** Default validate: numeric List or Option[List] only */
  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    fn match
      case f: F =>
        val recvFT = Utility.rhsType(f.recv)(using ctx)
        recvFT match
          case Some(ListType(_, elem: FieldType, _, _)) if Validation.isNumericType(elem) => Right(())
          case Some(OptionType(_, ListType(_, elem: FieldType, _, _), _)) if Validation.isNumericType(elem) => Right(())
          case Some(ft) =>
            Left(DLCompileError(ctx.posStr, s"$name() requires a List of numeric type, got ${ft.typeName}"))
          case None =>
            Left(DLCompileError(ctx.posStr, s"$name() cannot determine receiver type"))
      case _ => Right(())


object CMinFn extends MathCFn[MinFn]:
  val name = "min"
  def construct(recv: Fn[Any], posStr: String): MinFn = MinFn(recv, posStr)


object CMaxFn extends MathCFn[MaxFn]:
  val name = "max"
  def construct(recv: Fn[Any], posStr: String): MaxFn = MaxFn(recv, posStr)


object CMedianFn extends MathCFn[MedianFn]:
  val name = "median"
  def construct(recv: Fn[Any], posStr: String): MedianFn = MedianFn(recv, posStr)
  override def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "scala.Double")


object CSumFn extends MathCFn[SumFn]:
  val name = "sum"
  def construct(recv: Fn[Any], posStr: String): SumFn = SumFn(recv, posStr)
  override def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "scala.Double")


object CAvgFn extends MathCFn[AvgFn]:
  val name = "avg"
  def construct(recv: Fn[Any], posStr: String): AvgFn = AvgFn(recv, posStr)
  override def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "scala.Double")


object CAbsFn extends CompileFn[AbsFn]:
  val name = "abs"
  val minArgs = 0

  override def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    receiver match
      case ft: FieldType if Validation.isNumericType(ft) => true
      case OptionType(_, inner: FieldType, _) if Validation.isNumericType(inner) => true
      case _ => false

  override def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    Utility.rhsType(receiver.fn)(using ctx).getOrElse(ScalarType("", "scala.Double"))

  override def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, AbsFn] =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, "abs() takes no arguments"))
    else Right(AbsFn(recv.fn, ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    fn match
      case a: AbsFn =>
        Utility.rhsType(a.recv)(using ctx) match
          case Some(ft) if Validation.isNumericType(ft) =>
            Right(())
          case Some(OptionType(_, inner: FieldType, _)) if Validation.isNumericType(inner) =>
            Right(())
          case Some(ft) =>
            Left(DLCompileError(ctx.posStr, s"abs() requires a numeric type, got ${ft.typeName}"))
          case None =>
            Left(DLCompileError(ctx.posStr, "abs() cannot determine receiver type"))
      case _ => Right(())