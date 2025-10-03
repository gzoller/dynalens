package co.blocke.dynalens
package parser
package fn


object CMinFn extends CompileFn[MinFn]:
  val name = "min"
  val minArgs = 0

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case ListType(_, _, _) => true
      case OptionType(_, _: ListType, _) => true
      case _ => false

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    receiver match
      case ListType(_, elem, _) if elem.isNumeric => elem
      case OptionType(_, ListType(_, elem, _), _) if elem.isNumeric => elem
      case _ => ScalarType("", "scala.Any")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(0, "min() takes no arguments"))
    else Right(MinFn(recv))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case m: MinFn =>
        m.recv.flatMap(Utility.rhsType) match
          case Some(ListType(_, elem, _)) if elem.isNumeric => Right(())
          case Some(OptionType(_, ListType(_, elem, _), _)) if elem.isNumeric => Right(())
          case Some(ft) => Left(DLCompileError(0, s"min() requires a List of numeric type, got ${ft.typeName}"))
          case None => Left(DLCompileError(0, "min() cannot determine receiver type"))
      case _ => Right(())


object CMaxFn extends CompileFn[MaxFn]:
  val name = "max"
  val minArgs = 0

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case ListType(_, _, _) => true
      case OptionType(_, _: ListType, _) => true
      case _ => false

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    receiver match
      case ListType(_, elem, _) if elem.isNumeric => elem
      case OptionType(_, ListType(_, elem, _), _) if elem.isNumeric => elem
      case _ => ScalarType("", "scala.Any")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(0, "max() takes no arguments"))
    else Right(MaxFn(recv))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case m: MaxFn =>
        m.recv.flatMap(Utility.rhsType) match
          case Some(ListType(_, elem, _)) if elem.isNumeric => Right(())
          case Some(OptionType(_, ListType(_, elem, _), _)) if elem.isNumeric => Right(())
          case Some(ft) => Left(DLCompileError(0, s"max() requires a List of numeric type, got ${ft.typeName}"))
          case None => Left(DLCompileError(0, "max() cannot determine receiver type"))
      case _ => Right(())


object CMedianFn extends CompileFn[MedianFn]:
  val name    = "median"
  val minArgs = 0

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case ListType(_, _, _)             => true
      case OptionType(_, _: ListType, _) => true
      case _                             => false

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    // Median always produces a Double, since it may average 2 elements
    ScalarType("", "scala.Double")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(0, "median() takes no arguments"))
    else Right(MedianFn(recv))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case m: MedianFn =>
        m.recv.flatMap(Utility.rhsType) match
          case Some(ListType(_, elem, _)) if elem.isNumeric => Right(())
          case Some(OptionType(_, ListType(_, elem, _), _)) if elem.isNumeric => Right(())
          case Some(ft) => Left(DLCompileError(0, s"median() requires a List of numeric type, got ${ft.typeName}"))
          case None     => Left(DLCompileError(0, "median() cannot determine receiver type"))
      case _ => Right(())


object CSumFn extends CompileFn[SumFn]:
  val name    = "sum"
  val minArgs = 0

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case _: ListType                   => true
      case OptionType(_, _: ListType, _) => true
      case _                             => false

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "scala.Double") // always promotes to Double

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(0, "sum() takes no arguments"))
    else Right(SumFn(recv))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case s: SumFn =>
        s.recv.flatMap(Utility.rhsType) match
          case Some(_: ListType) | Some(OptionType(_, _: ListType, _)) => Right(())
          case Some(ft) => Left(DLCompileError(0, s"sum() requires a List receiver, got ${ft.typeName}"))
          case None     => Left(DLCompileError(0, "sum() cannot determine receiver type"))
      case _ => Right(())


object CAvgFn extends CompileFn[AvgFn]:
  val name    = "avg"
  val minArgs = 0

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case ListType(_, _, _)             => true
      case OptionType(_, _: ListType, _) => true
      case _                             => false

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    // average is always floating-point
    ScalarType("", "scala.Double")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(0, "avg() takes no arguments"))
    else Right(AvgFn(recv))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case a: AvgFn =>
        a.recv.flatMap(Utility.rhsType) match
          case Some(ListType(_, elem, _)) if elem.isNumeric => Right(())
          case Some(OptionType(_, ListType(_, elem, _), _)) if elem.isNumeric => Right(())
          case Some(ft) => Left(DLCompileError(0, s"avg() requires a List of numeric type, got ${ft.typeName}"))
          case None     => Left(DLCompileError(0, "avg() cannot determine receiver type"))
      case _ => Right(())


object CAbsFn extends CompileFn[AbsFn]:
  val name    = "abs"
  val minArgs = 0

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case s if s.isNumeric                 => true
      case OptionType(_, inner, _) if inner.isNumeric => true
      case _                                => false

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    // abs() preserves the underlying numeric type (e.g., Int stays Int, Double stays Double)
    receiver

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(0, "abs() takes no arguments"))
    else Right(AbsFn(recv))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case a: AbsFn =>
        a.recv.flatMap(Utility.rhsType) match
          case Some(t) if t.isNumeric => Right(())
          case Some(OptionType(_, inner, _)) if inner.isNumeric => Right(())
          case Some(ft) => Left(DLCompileError(0, s"abs() requires a numeric type, got ${ft.typeName}"))
          case None     => Left(DLCompileError(0, "abs() cannot determine receiver type"))
      case _ => Right(())