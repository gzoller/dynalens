package co.blocke.dynalens
package parser
package fn


object CFormatDateFn extends CompileFn[FormatDateFn]:
  val name = "formatDate"
  val minArgs = 2
  override val maxArgs = 2

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case ScalarType(_, t) if Set("java.time.LocalDate","java.time.LocalDateTime","java.time.Instant").contains(t) =>
        true
      case _ => false

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.lang.String")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption.toRight(DLCompileError(0, "formatDate() requires a pattern argument"))
      .flatMap {
        case pat: Fn[String] => Right(FormatDateFn(recv, pat))
        case other =>
          Left(DLCompileError(0, s"formatDate requires a String pattern, got ${other.getClass.getSimpleName}"))
      }

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case f: FormatDateFn =>
        Utility.rhsType(f.pattern) match
          case Some(ScalarType(_, "java.lang.String")) => Right(())
          case Some(ft) =>
            Left(DLCompileError(0, s"formatDate pattern must be string, found ${ft.typeName}"))
          case None =>
            Left(DLCompileError(0, "formatDate cannot determine pattern type"))
      case _ => Right(())


object CParseDateFn extends CompileFn[ParseDateFn]:
  val name = "parseDate"
  val minArgs = 1
  override val maxArgs = 2

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case ScalarType(_, "java.lang.String") => true
      case _ => false

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.time.LocalDateTime")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption.toRight(DLCompileError(0, "parseDate() requires a pattern argument"))
      .flatMap {
        case pat: Fn[String] => Right(ParseDateFn(recv, pat))
        case other =>
          Left(DLCompileError(0, s"parseDate requires a String pattern, got ${other.getClass.getSimpleName}"))
      }

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case p: ParseDateFn =>
        Utility.rhsType(p.receiver) match
          case Some(ScalarType(_, "java.lang.String")) => Right(())
          case Some(ft) =>
            Left(DLCompileError(0, s"parseDate requires string source, found ${ft.typeName}"))
          case None =>
            Left(DLCompileError(0, "parseDate cannot determine source type"))
      case _ => Right(())


object CNowFn extends CompileFn[NowFn]:
  val name = "now"
  val minArgs = 0
  override val maxArgs = 0
  override val standalone: Boolean = true

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean = true

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.time.LocalDateTime")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(0, "now() takes no arguments"))
    else Right(NowFn())

  override def validate(fn: Fn[?])(using ctx: ExprContext) = Right(())