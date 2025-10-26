package co.blocke.dynalens
package parser
package cfn

import scala.annotation.tailrec
import co.blocke.dynalens.fn.*


// ---------------------- IF ----------------------
object CIfFn extends CompileFn[IfFn[?]]:
  val name = "if"
  val minArgs = 3

  def accepts(recv: Receiver)(using ctx: ExprContext): Boolean = true

  def resultType(recv: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    (args.lift(1), args.lift(2)) match
      case (Some(t1), Some(t2)) if t1 == t2 => t1
      case _                                => ScalarType("", "scala.Any")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.lengthCompare(3) == 0 then
      Right(IfFn(args(0).asInstanceOf[BooleanFn], args(1), args(2), ctx.posStr))
    else
      Left(DLCompileError(ctx.posStr, "if(cond, then, else) requires 3 arguments"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case i: IfFn[?] =>
        Validation.requireBoolean1(i.condition.asInstanceOf[Fn[Any]], "if", ctx.posStr)
      case _ => Right(())


// ---------------------- AND ----------------------
object CAndFn extends CompileFn[AndFn]:
  val name = "&&"
  val minArgs = 1        // one explicit rhs arg, recv is lhs

  def accepts(recv: Receiver)(using ctx: ExprContext): Boolean =
    recv.ftype match
      case ScalarType(_, "scala.Boolean", _) => true
      case ft if ft.isOptional && ft.isInstanceOf[ScalarType] =>
        ft.asInstanceOf[ScalarType].typeName == "scala.Boolean"
      case _ => false

  def resultType(recv: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "scala.Boolean")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.size == 1 then
      (recv.fn.asInstanceOf[Fn[Any]], args.head) match
        case (l: BooleanFn, r: BooleanFn) =>
          Right(AndFn(l, List(r), ctx.posStr))
        case _ =>
          Left(DLCompileError(ctx.posStr, "&& requires two boolean expressions"))
    else
      Left(DLCompileError(ctx.posStr, "&& requires exactly one rhs argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case a: AndFn =>
        val ops = a.recv :: a.args
        Validation.requireBoolean2(ops, "&&", ctx.posStr)
      case _ => Right(())


// ---------------------- OR ----------------------
object COrFn extends CompileFn[OrFn]:
  val name = "||"
  val minArgs = 1        // one explicit rhs arg, recv is lhs

  def accepts(recv: Receiver)(using ctx: ExprContext): Boolean =
    recv.ftype match
      case ScalarType(_, "scala.Boolean", _) => true
      case ft if ft.isOptional && ft.isInstanceOf[ScalarType] =>
        ft.asInstanceOf[ScalarType].typeName == "scala.Boolean"
      case _ => false

  def resultType(recv: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "scala.Boolean")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.size == 1 then
      (recv.fn.asInstanceOf[Fn[Any]], args.head) match
        case (l: BooleanFn, r: BooleanFn) =>
          Right(OrFn(l, List(r), ctx.posStr))
        case _ =>
          Left(DLCompileError(ctx.posStr, "|| requires two boolean expressions"))
    else
      Left(DLCompileError(ctx.posStr, "|| requires exactly one rhs argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case o: OrFn =>
        val ops = o.recv :: o.args
        Validation.requireBoolean2(ops, "||", ctx.posStr)
      case _ => Right(())


// ---------------------- NOT ----------------------
object CNotFn extends CompileFn[NotFn]:
  val name = "!"
  val minArgs = 1

  def accepts(recv: Receiver)(using ctx: ExprContext): Boolean =
    recv.ftype match
      case ScalarType(_, "scala.Boolean", _) => true
      case ft if ft.isOptional && ft.isInstanceOf[ScalarType] =>
        ft.asInstanceOf[ScalarType].typeName == "scala.Boolean"
      case _ => false

  def resultType(recv: Receiver, args: List[FieldType])(using ctx: ExprContext) =
    ScalarType("", "scala.Boolean")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption match
      case Some(arg) => Validation.expectBoolean1(arg, "!", ctx.posStr).map(b => NotFn(b.asInstanceOf[Fn[Any]], ctx.posStr))
      case None      => Left(DLCompileError(ctx.posStr, "! requires a single argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case n: NotFn =>
        Validation.requireBoolean1(n.recv.asInstanceOf[Fn[Any]], "!", ctx.posStr)
      case _ => Right(())


// ---------------------- ISDEFINED ----------------------
object CIsDefinedFn extends CompileFn[IsDefinedFn]:
  val name = "isDefined"
  val minArgs = 0
  override val maxArgs = 0

  def accepts(recv: Receiver)(using ctx: ExprContext): Boolean =
    recv.ftype match
      case ft if ft.isOptional => true
      case ListType(_, _, _, _)   => true
      case MapType(_, _, _, _, _) => true
      case _                   => false

  def resultType(recv: Receiver, args: List[FieldType])(using ctx: ExprContext) =
    ScalarType("", "scala.Boolean")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    Right(IsDefinedFn(recv.fn.asInstanceOf[Fn[Any]], ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext) = Right(())


// ---------------------- STARTSWITH ----------------------
object CStartsWithFn extends CompileFn[StartsWithFn]:
  val name = "startsWith"
  val minArgs = 1
  override val maxArgs = 1

  def accepts(recv: Receiver)(using ctx: ExprContext): Boolean =
    recv.ftype.isStringLike

  def resultType(recv: Receiver, args: List[FieldType])(using ctx: ExprContext) =
    ScalarType("", "scala.Boolean")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption
      .toRight(DLCompileError(ctx.posStr, "startsWith requires one argument"))
      .map(arg => StartsWithFn(recv.fn.asInstanceOf[Fn[Any]], arg, ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case s: StartsWithFn =>
        Utility.rhsType(s.other) match
          case TypeResult.Known(ft) if ft.isStringLike => Right(())
          case TypeResult.Known(ft)                    => Left(DLCompileError(ctx.posStr, s"startsWith requires String argument, found ${ft.typeName}"))
          case TypeResult.Error(e)                      => Left(e)
          case _                                       => Left(DLCompileError(ctx.posStr, "startsWith cannot determine argument type"))
      case _ => Right(())


// ---------------------- ENDSWITH ----------------------
object CEndsWithFn extends CompileFn[EndsWithFn]:
  val name = "endsWith"
  val minArgs = 1
  override val maxArgs = 1

  def accepts(recv: Receiver)(using ctx: ExprContext): Boolean =
    recv.ftype.isStringLike

  def resultType(recv: Receiver, args: List[FieldType])(using ctx: ExprContext) =
    ScalarType("", "scala.Boolean")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption
      .toRight(DLCompileError(ctx.posStr, "endsWith requires one argument"))
      .map(arg => EndsWithFn(recv.fn.asInstanceOf[Fn[Any]], arg, ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case e: EndsWithFn =>
        Utility.rhsType(e.other) match
          case TypeResult.Known(ft) if ft.isStringLike => Right(())
          case TypeResult.Known(ft)                    => Left(DLCompileError(ctx.posStr, s"endsWith requires String argument, found ${ft.typeName}"))
          case TypeResult.Error(e)                      => Left(e)
          case _                                       => Left(DLCompileError(ctx.posStr, "endsWith cannot determine argument type"))
      case _ => Right(())


// ---------------------- CONTAINS ----------------------
object CContainsFn extends CompileFn[ContainsFn]:
  val name = "contains"
  val minArgs = 1
  override val maxArgs = 1

  @tailrec
  def accepts(recv: co.blocke.dynalens.parser.Receiver)(using ctx: ExprContext): Boolean =
    recv.ftype match
      case ScalarType(_, "java.lang.String", _) => true
      case ListType(_, _, _, _)                 => true
      case MapType(_, _, _, _, _)               => true
      case ft if ft.isOptional =>
        // Recursively check the inner non-optional type by cloning it
        accepts(MethodReceiver(SystemReceiver, ft.cloneWithOptional(false), recv.fn))
      case _                                    => false

  def resultType(recv: Receiver, args: List[FieldType])(using ctx: ExprContext) =
    ScalarType("", "scala.Boolean")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption
      .toRight(DLCompileError(ctx.posStr, "contains requires one argument"))
      .map(arg => ContainsFn(recv.fn.asInstanceOf[Fn[Any]], arg, ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case c: ContainsFn =>
        (Utility.rhsType(c.recv), Utility.rhsType(c.other)) match
          case (TypeResult.Known(ScalarType(_, "java.lang.String", _)), TypeResult.Known(ScalarType(_, "java.lang.String", _))) => Right(())
          case (TypeResult.Known(ListType(_, elem, _, _)), TypeResult.Known(argT)) if elem.canAssignTo(argT) => Right(())
          case (TypeResult.Known(MapType(_, keyType, _, _, _)), TypeResult.Known(argT)) if keyType.canAssignTo(argT) => Right(())
          case (TypeResult.Known(recvT), TypeResult.Known(argT)) =>
            Left(DLCompileError(ctx.posStr, s"contains not applicable: receiver ${recvT.typeName} with argument ${argT.typeName}"))
          case (TypeResult.Error(e), _) => Left(e)
          case (_, TypeResult.Error(e)) => Left(e)
          case _ => Left(DLCompileError(ctx.posStr, "contains cannot determine receiver or argument type"))
      case _ => Right(())


// ---------------------- EQUALSIGNORECASE ----------------------
object CEqualsIgnoreCaseFn extends CompileFn[EqualsIgnoreCaseFn]:
  val name = "equalsIgnoreCase"
  val minArgs = 1
  override val maxArgs = 1

  def accepts(recv: Receiver)(using ctx: ExprContext): Boolean =
    recv.ftype.isStringLike

  def resultType(recv: Receiver, args: List[FieldType])(using ctx: ExprContext) =
    ScalarType("", "scala.Boolean")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption
      .toRight(DLCompileError(ctx.posStr, "equalsIgnoreCase requires one argument"))
      .map(arg => EqualsIgnoreCaseFn(recv.fn.asInstanceOf[Fn[Any]], arg, ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case e: EqualsIgnoreCaseFn =>
        Utility.rhsType(e.other) match
          case TypeResult.Known(ft) if ft.isStringLike => Right(())
          case TypeResult.Known(ft)                    => Left(DLCompileError(ctx.posStr, s"equalsIgnoreCase requires String argument, found ${ft.typeName}"))
          case TypeResult.Error(e)                      => Left(e)
          case _                                       => Left(DLCompileError(ctx.posStr, "equalsIgnoreCase cannot determine argument type"))
      case _ => Right(())


// ---------------------- MATCHESREGEX ----------------------
object CMatchesRegexFn extends CompileFn[MatchesRegexFn]:
  val name = "matchesRegex"
  val minArgs = 1
  override val maxArgs = 1

  def accepts(recv: Receiver)(using ctx: ExprContext): Boolean =
    recv.ftype.isStringLike

  def resultType(recv: Receiver, args: List[FieldType])(using ctx: ExprContext) =
    ScalarType("", "scala.Boolean")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption
      .toRight(DLCompileError(ctx.posStr, "matchesRegex requires one argument"))
      .map(arg => MatchesRegexFn(recv.fn.asInstanceOf[Fn[Any]], arg, ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case m: MatchesRegexFn =>
        Utility.rhsType(m.other) match
          case TypeResult.Known(ft) if ft.isStringLike => Right(())
          case TypeResult.Known(ft)                    => Left(DLCompileError(ctx.posStr, s"matchesRegex requires String argument, found ${ft.typeName}"))
          case TypeResult.Error(e)                      => Left(e)
          case _                                       => Left(DLCompileError(ctx.posStr, "matchesRegex cannot determine argument type"))
      case _ => Right(())