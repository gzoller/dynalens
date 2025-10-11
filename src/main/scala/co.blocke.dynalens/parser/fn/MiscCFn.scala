package co.blocke.dynalens
package parser
package fn


import co.blocke.dynalens.fn.*


object CUuidFn extends CompileFn[UUIDFn]:
  val name = "uuid"
  val minArgs = 0
  override val maxArgs = 0
  override val standalone = true

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean = true

  def resultType(receiver: Receiver, argTypes: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "java.lang.String")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, "uuid() takes no arguments"))
    else Right(UUIDFn(ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext) = Right(())



object CElseFn extends CompileFn[ElseFn]:
  val name = "else"
  val minArgs = 1
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    receiver.ftype.isInstanceOf[OptionType]

  def resultType(receiver: Receiver, argTypes: List[FieldType])(using ctx: ExprContext): FieldType =
    receiver.ftype match
      case OptionType(_, inner, _) => inner
      case _                       => ScalarType("", "scala.Any")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption match
      case Some(default) => Right(ElseFn(recv.fn, default, ctx.posStr))
      case None          => Left(DLCompileError(ctx.posStr, "else() requires a default value"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case e: ElseFn =>
        (Utility.rhsType(e.recv), Utility.rhsType(e.default)) match
          case (Some(OptionType(_, inner, _)), Some(d)) if inner.typeName == d.typeName =>
            Right(())
          case (Some(OptionType(_, inner, _)), Some(d)) =>
            Left(DLCompileError(ctx.posStr, s"else() type mismatch: expected ${inner.typeName}, got ${d.typeName}"))
          case (Some(_), _) =>
            Left(DLCompileError(ctx.posStr, "else() requires an Option receiver"))
          case _ =>
            Left(DLCompileError(ctx.posStr, "else() could not resolve types"))
      case _ => Right(())


object CBlockFn extends CompileFn[BlockFn[?]]:
  val name = "{block}" // synthetic placeholder
  val minArgs = 1
  override val maxArgs = Int.MaxValue

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean = true

  def resultType(receiver: Receiver, argTypes: List[FieldType])(using ctx: ExprContext): FieldType =
    argTypes.lastOption.getOrElse(ScalarType("", "scala.Unit"))

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    Left(DLCompileError(ctx.posStr, "BlockFn is parser-constructed only"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case b: BlockFn[?] =>
        b.finalFn match
          case inner: Fn[?] => Right(()) // Parser ensures correctness
          case _             => Right(())
      case _ => Right(())


object CCaseWhenFn extends CompileFn[CaseWhenFn]:
  val name = "case"
  val minArgs = 2 // must have at least one case and one value
  override val maxArgs = Int.MaxValue

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    true // case() works with any receiver type

  def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    // Usually same as RHS type if all RHS are consistent, else fallback to Any
    val rhsTypes = args.tail
    if rhsTypes.nonEmpty && rhsTypes.distinct.size == 1 then rhsTypes.head
    else Utility.rhsType(receiver.fn)(using ctx).getOrElse(ScalarType("", "scala.Any"))

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.isEmpty then
      Left(DLCompileError(ctx.posStr, "case() requires at least one pattern→result pair"))
    else
      val pairs = args.grouped(2).collect {
        case List(ConstantFn(k), v) => (k, v)
      }.toVector

      val hasDefault = args.length % 2 != 0
      val defaultOpt =
        if hasDefault then Some(args.last)
        else None

      Right(CaseWhenFn(recv.fn, pairs, defaultOpt, false, ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case c: CaseWhenFn =>
        val invalidKeys = c.cases.collect {
          case (k, _) if k == null =>
            "null"
        }
        if invalidKeys.nonEmpty then
          Left(DLCompileError(ctx.posStr, s"case() has invalid key(s): ${invalidKeys.mkString(", ")}"))
        else Right(())
      case _ => Right(())


object CIndexFn extends CompileFn[IndexFn]:
  val name: String = "[index]"              // synthetic name for registry/debug
  override val standalone: Boolean = false

  val minArgs: Int = 1
  override val maxArgs: Int = 1

  /** Allow indexing on:
   *   - ListType(_, elem, _)
   *   - OptionType(_, ListType(...), _)
   *   - MapType(_, keyType, valueType, _)
   *   - OptionType(_, MapType(...), _)
   */
  override def accepts(recv: Receiver)(using ctx: ExprContext): Boolean = true

  /** Result type rules:
   *   - List[T]                [i:Int] -> T
   *   - Option[List[T]]        [i:Int] -> Option[T]
   *   - Map[K,V]               [k:K]   -> Option[V]
   *   - Option[Map[K,V]]       [k:K]   -> Option[Option[V]]  (nested)
   */
  override def resultType(recv: Receiver, argTypes: List[FieldType])(using ctx: ExprContext): FieldType = {
    recv.ftype match {
      case ListType(_, elem, _, _) =>
        elem

      case OptionType(_, inner: ListType, opt) =>
        OptionType("", inner.elementType, "scala.Option")

      case MapType(_, _, value, _, _) =>
        OptionType("", value, "scala.Option")

      case OptionType(_, inner: MapType, opt) =>
        OptionType("", OptionType("", inner.valueType, "scala.Option"), "scala.Option")

      case _ =>
        // Defensive; accepts() should have rejected
        ScalarType("", "scala.Any")
    }
  }

  override def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, IndexFn] =
    args.headOption match
      case Some(indexArg) =>
        Right(IndexFn(recv.fn, indexArg, ctx.posStr))
      case None =>
        Left(DLCompileError(ctx.posStr, s"indexing requires an index/key argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] = {
    fn match
      case i: IndexFn =>
        val recvT  = Utility.rhsType(i.recv)
        val argT   = Utility.rhsType(i.index)

        (recvT, argT) match
          // ---- List / Option[List] + numeric index ----
          case (Some(ListType(_, _, _, _)), Some(a)) if Validation.isNumericType(a) =>
            Right(())
          case (Some(OptionType(_, inner: ListType, _)), Some(a)) if Validation.isNumericType(a) =>
            Right(())

          // ---- Map / Option[Map] + key type check ----
          case (Some(MapType(_, keyT, value, _, _)), Some(a)) if keyT.conformsTo(a) || a.conformsTo(keyT) =>
            Right(())
          case (Some(OptionType(_, inner: MapType, _)), Some(a)) if inner.keyType.conformsTo(a) || a.conformsTo(inner.keyType) =>
            Right(())

          // ---- Specific, more helpful error messages ----
          case (Some(ListType(_, _, _, _)), Some(a)) =>
            Left(DLCompileError(ctx.posStr, s"index requires numeric index, found ${a.typeName}"))
          case (Some(OptionType(_, _: ListType, _)), Some(a)) =>
            Left(DLCompileError(ctx.posStr, s"index requires numeric index, found ${a.typeName}"))

          case (Some(MapType(_, keyT, value, _, _)), Some(a)) =>
            Left(DLCompileError(ctx.posStr, s"map index key type mismatch: expected ${keyT.typeName}, got ${a.typeName}"))
          case (Some(OptionType(_, inner: MapType, _)), Some(a)) =>
            Left(DLCompileError(ctx.posStr, s"map index key type mismatch: expected ${inner.keyType.typeName}, got ${a.typeName}"))

          case (Some(other), _) =>
            Left(DLCompileError(ctx.posStr, s"indexing not supported on receiver type ${other.typeName}"))
          case _ =>
            Left(DLCompileError(ctx.posStr, s"cannot resolve receiver or index type for indexing"))

      case _ =>
        Right(())
  }
