package co.blocke.dynalens
package parser
package cfn

import co.blocke.dynalens.fn.*

trait MathCFn[F <: Fn[Any]] extends CompileFn:
  override val minArgs = 0

  /** Determines if the receiver is acceptable (List of numeric) */
  override def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    receiver.ftype match
      case ListType(_, elem: FieldType, _, _) if Validation.isNumericType(elem) => true
      case _ => false

  /** Build boilerplate: all math fns take no args */
  override def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, F] =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, s"$name() takes no arguments"))
    else Right(construct(recv.fn, posStr = ctx.posStr))

  /** Each subclass implements this to create its runtime Fn */
  def construct(recv: Fn[Any], posStr: String): F

  /** Default validate: numeric List only */
  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    val recvFT = Utility.rhsType(fn.recv)(using ctx)
    recvFT match
      case TypeResult.Known(ListType(_, elem: FieldType, _, _)) if Validation.isNumericType(elem) => Right(())
      case TypeResult.Known(ft) if ft.isOptional && Validation.isNumericType(ft) => Right(())
      case TypeResult.Known(ft) =>
        Left(DLCompileError(ctx.posStr, s"$name() requires a List of numeric type, got ${ft.typeName}"))
      case TypeResult.Error(err) => Left(err)
      case TypeResult.Unknown =>
        Left(DLCompileError(ctx.posStr, s"$name() cannot determine receiver type"))


object CMinFn extends MathCFn[MinFn]:
  val name = "min"
  def construct(recv: Fn[Any], posStr: String): MinFn = MinFn(recv, posStr)


object CMaxFn extends MathCFn[MaxFn]:
  val name = "max"
  def construct(recv: Fn[Any], posStr: String): MaxFn = MaxFn(recv, posStr)


object CMedianFn extends MathCFn[MedianFn]:
  val name = "median"
  def construct(recv: Fn[Any], posStr: String): MedianFn = MedianFn(recv, posStr)


object CSumFn extends MathCFn[SumFn]:
  val name = "sum"
  def construct(recv: Fn[Any], posStr: String): SumFn = SumFn(recv, posStr)


object CAvgFn extends MathCFn[AvgFn]:
  val name = "avg"
  def construct(recv: Fn[Any], posStr: String): AvgFn = AvgFn(recv, posStr)


object CAbsFn extends CompileFn:
  val name = "abs"
  val minArgs = 0

  override def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Validation.isNumericType(receiver.ftype) ||
      (receiver.ftype.isOptional && Validation.isNumericType(receiver.ftype))

  override def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, AbsFn] =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, "abs() takes no arguments"))
    else Right(AbsFn(recv.fn, ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    fn match
      case a: AbsFn =>
        Utility.rhsType(a.recv)(using ctx) match
          case TypeResult.Known(ft) if Validation.isNumericType(ft) || (ft.isOptional && Validation.isNumericType(ft)) =>
            Right(())
          case TypeResult.Known(ft) =>
            Left(DLCompileError(ctx.posStr, s"abs() requires a numeric type, got ${ft.typeName}"))
          case TypeResult.Error(err) => Left(err)
          case TypeResult.Unknown =>
            Left(DLCompileError(ctx.posStr, "abs() cannot determine receiver type"))
      case _ => Right(())
