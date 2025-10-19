package co.blocke.dynalens
package parser
package fn

import co.blocke.dynalens.fn.*

object CFormatDateFn extends CompileFn[FormatDateFn]:
  val name = "formatDate"
  val minArgs = 1
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    val t = receiver.ftype.typeName
    Set("java.time.LocalDate", "java.time.LocalDateTime", "java.time.Instant").contains(t)

  def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.lang.String")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, FormatDateFn] =
    args.headOption match
      case Some(patternFn) =>
        Right(FormatDateFn(recv.fn, patternFn, ctx.posStr))
      case None =>
        Left(DLCompileError(ctx.posStr, "formatDate() requires a pattern argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    fn match
      case f: FormatDateFn =>
        Utility.rhsType(f.pattern) match
          case Some(ScalarType(_, "java.lang.String", _)) => Right(())
          case Some(ft) =>
            Left(DLCompileError(ctx.posStr, s"formatDate pattern must be string, found ${ft.typeName}"))
          case None =>
            Left(DLCompileError(ctx.posStr, "formatDate cannot determine pattern type"))
      case _ => Right(())


object CParseDateFn extends CompileFn[ParseDateFn]:
  val name = "parseDate"
  val minArgs = 1
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    receiver.ftype.typeName == "java.lang.String"

  def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.time.LocalDateTime")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, ParseDateFn] =
    args.headOption match
      case Some(patternFn) =>
        Right(ParseDateFn(recv.fn, patternFn, ctx.posStr))
      case None =>
        Left(DLCompileError(ctx.posStr, "parseDate() requires a pattern argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    fn match
      case p: ParseDateFn =>
        Utility.rhsType(p.recv) match
          case Some(ScalarType(_, "java.lang.String", _)) => Right(())
          case Some(ft) =>
            Left(DLCompileError(ctx.posStr, s"parseDate requires string source, found ${ft.typeName}"))
          case None =>
            Left(DLCompileError(ctx.posStr, "parseDate cannot determine source type"))
      case _ => Right(())


object CNowFn extends CompileFn[NowFn]:
  val name = "now"
  val minArgs = 0
  override val maxArgs = 0
  override val standalone = true

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean = true

  def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.time.LocalDateTime")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, NowFn] =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, "now() takes no arguments"))
    else Right(NowFn(ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    Right(())