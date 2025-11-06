package co.blocke.dynalens
package parser
package cfn


import co.blocke.dynalens.fn.*

object CUuidFn extends CompileFn:
  val name = "uuid"
  val minArgs = 0
  override val maxArgs = 0
  override val standalone = true

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean = true

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, "uuid() takes no arguments"))
    else Right(UUIDFn(ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext) = Right(())


object CElseFn extends CompileFn:
  val name = "else"
  val minArgs = 1
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    receiver.ftype.isOptional

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption match
      case Some(default) => Right(ElseFn(recv.fn, default, ctx.posStr))
      case None          => Left(DLCompileError(ctx.posStr, "else() requires a default value"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case e: ElseFn =>
        (Utility.rhsType(e.recv), Utility.rhsType(e.default)) match
          case (TypeResult.Known(recvType), TypeResult.Known(d)) if recvType.isOptional =>
            val innerType = recvType.cloneWithOptional(false)
            if innerType.typeName == d.typeName then Right(())
            else Left(DLCompileError(ctx.posStr, s"else() type mismatch: expected ${innerType.typeName}, got ${d.typeName}"))
          case (TypeResult.Known(_), _) =>
            Left(DLCompileError(ctx.posStr, "else() requires an Optional receiver"))
          case (TypeResult.Error(err), _) => Left(err)
          case (_, TypeResult.Error(err)) => Left(err)
          case _ =>
            Left(DLCompileError(ctx.posStr, "else() could not resolve types"))
      case _ => Right(())


object CBlockFn extends CompileFn:
  val name = "block"
  val minArgs = 1

  override def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean = true

  override def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, BlockFn[Any]] =
    args match
      case Nil => Left(DLCompileError(ctx.posStr, s"$name() requires at least one argument"))
      case _ =>
        val statements = args.init.collect { case s: Statement => s }
        val finalFn = args.last
        Right(BlockFn(statements, finalFn, ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] =
    fn match
      case b: BlockFn[?] =>
        val allStatementsValid = b.statements.forall(_.isInstanceOf[Statement])
        val finalFnValid = b.finalFn != null
        if allStatementsValid && finalFnValid then Right(())
        else Left(DLCompileError(ctx.posStr, s"$name() contains invalid elements"))
      case _ => Right(())


object CCaseWhenFn extends CompileFn:
  val name = "case"
  val minArgs = 2 // must have at least one case and one value
  override val maxArgs = Int.MaxValue

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    true // case() works with any receiver type

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


object CIndexFn extends CompileFn:
  val name: String = "[index]"              // synthetic name for registry/debug
  override val standalone: Boolean = false

  val minArgs: Int = 1
  override val maxArgs: Int = 1

  override def accepts(recv: Receiver)(using ctx: ExprContext): Boolean =
    recv.ftype match
      case lt: ListType => true
      case mt: MapType => true
      case _ => false

  override def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, IndexFn] =
    args.headOption match
      case Some(indexArg) =>
        Right(IndexFn(recv.fn, indexArg, ctx.posStr))
      case None =>
        Left(DLCompileError(ctx.posStr, s"indexing requires an index/key argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] = {
    fn match
      case i: IndexFn =>
        (Utility.rhsType(i.recv), Utility.rhsType(i.index)) match
          case (TypeResult.Known(rt: ListType), TypeResult.Known(a)) if Validation.isNumericType(a) && !rt.isOptional =>
            Right(())
          case (TypeResult.Known(rt: ListType), TypeResult.Known(a)) if Validation.isNumericType(a) && rt.isOptional =>
            Right(())

          case (TypeResult.Known(rt: MapType), TypeResult.Known(a)) if rt.keyType.canAssignTo(a) || a.canAssignTo(rt.keyType) =>
            Right(())

          case (TypeResult.Known(rt: ListType), TypeResult.Known(a)) =>
            Left(DLCompileError(ctx.posStr, s"index requires numeric index, found ${a.typeName}"))

          case (TypeResult.Known(rt: MapType), TypeResult.Known(a)) =>
            Left(DLCompileError(ctx.posStr, s"map index key type mismatch: expected ${rt.keyType.typeName}, got ${a.typeName}"))

          case (TypeResult.Known(other), _) =>
            Left(DLCompileError(ctx.posStr, s"indexing not supported on receiver type ${other.typeName}"))
          case (TypeResult.Error(err), _) => Left(err)
          case (_, TypeResult.Error(err)) => Left(err)
          case _ =>
            Left(DLCompileError(ctx.posStr, s"cannot resolve receiver or index type for indexing"))

      case _ =>
        Right(())
  }
