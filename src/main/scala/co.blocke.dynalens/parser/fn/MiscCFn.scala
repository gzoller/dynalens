package co.blocke.dynalens
package parser
package fn


object CUuidFn extends CompileFn[UUIDFn]:
  val name = "uuid"
  val minArgs = 0
  override val maxArgs = 0

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean = true

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.lang.String")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    Right(UUIDFn())


  object CElseFn extends CompileFn[ElseFn]:
    val name = "else"
    val minArgs = 1
    override val maxArgs = 1

    def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
      receiver.isInstanceOf[OptionType]

    def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
      receiver match
        case OptionType(_, inner, _) => inner
        case _ => ScalarType("", "scala.Any")

    def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
      args.headOption.toRight(DLCompileError(0, "else() requires a default value"))
        .map(default => ElseFn(recv, default))

    override def validate(fn: Fn[?])(using ctx: ExprContext) =
      fn match
        case e: ElseFn =>
          (e.recv.flatMap(Utility.rhsType), Utility.rhsType(e.right)) match
            case (Some(OptionType(_, inner, _)), Some(d)) if inner.typeName == d.typeName =>
              Right(())
            case (Some(OptionType(_, inner, _)), Some(d)) =>
              Left(DLCompileError(0, s"else() type mismatch: expected ${inner.typeName}, got ${d.typeName}"))
            case (Some(_), _) =>
              Left(DLCompileError(0, "else() requires an Option receiver"))
            case _ =>
              Left(DLCompileError(0, "else() could not resolve types"))
        case _ => Right(())


object CBlockFn extends CompileFn[BlockFn[?]]:
  val name = "{block}"             // synthetic name, not user-facing
  val minArgs = 1
  override val maxArgs = Int.MaxValue

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean = true

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    // last arg is the final expression type, or Unit if empty
    args.lastOption.getOrElse(ScalarType("", "scala.Unit"))

  // NOTE: BlockFn is always constructed in the parser, not here.
  // This stub makes it clear that build() is not part of the normal pipeline.
  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    Left(DLCompileError(0, "BlockFn must be constructed in the parser"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case b: BlockFn[?] =>
        b.finalFn match
          case inner: Fn[?] =>
            // recursively validate the final expression
            validate(inner)
          case _ =>
            Right(())
      case _ => Right(())