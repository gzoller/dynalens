package co.blocke.dynalens
package parser
package fn

import scala.annotation.tailrec


object CIfFn extends CompileFn[IfFn[?]]:
  def name: String = "if"

  override def builtIn: Boolean = true

  def minArgs: Int = 3

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean = true

  def resultType(receiver: FieldType, args: List[FieldType])
                (using ctx: ExprContext): FieldType =
    // crude join: if both branches are same type, return it,
    // otherwise default to Any (could be refined)
    (args.lift(1), args.lift(2)) match
      case (Some(t1), Some(t2)) if t1 == t2 => t1
      case (Some(t1), Some(t2)) => ScalarType("", "scala.Any")
      case _ => ScalarType("", "scala.Any")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext)
  : Either[DLCompileError, IfFn[?]] =
    if args.lengthCompare(3) == 0 then
      Right(IfFn(args(0).asInstanceOf[BooleanFn], args(1), args(2)))
    else
      Left(DLCompileError(0, "if(cond, then, else) requires 3 arguments"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case i: IfFn[?] =>
        for
          _ <- CompileFn.requireBoolean1(i.condition.asInstanceOf[Fn[Any]], "if", 0)
        yield ()
      case _ => Right(())


object CAndFn extends CompileFn[AndFn]:
  def name: String = "&&"

  override def builtIn: Boolean = true

  def minArgs: Int = 2

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case ScalarType(_, "scala.Boolean") => true
      case OptionType(_, ScalarType(_, "scala.Boolean"), _) => true
      case _ => false

  def resultType(receiver: FieldType, args: List[FieldType])
                (using ctx: ExprContext): FieldType =
    ScalarType("", "scala.Boolean")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext)
  : Either[DLCompileError, AndFn] =
    if args.size == 2 then
      (args(0), args(1)) match
        case (l: BooleanFn, r: BooleanFn) =>
          Right(AndFn(l, r))
        case _ =>
          Left(DLCompileError(0, "&& requires two boolean expressions"))
    else
      Left(DLCompileError(0, "&& requires exactly two arguments"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case a: AndFn =>
        for
          _ <- CompileFn.requireBoolean2(List(a.left.asInstanceOf[Fn[Any]], a.right.asInstanceOf[Fn[Any]]), "&&", 0)
        yield ()
      case _ => Right(())

object COrFn extends CompileFn[OrFn]:
  def name: String = "||"

  override def builtIn: Boolean = true

  def minArgs: Int = 2

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case ScalarType(_, "scala.Boolean") => true
      case OptionType(_, ScalarType(_, "scala.Boolean"), _) => true
      case _ => false

  def resultType(receiver: FieldType, args: List[FieldType])
                (using ctx: ExprContext): FieldType =
    ScalarType("", "scala.Boolean")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext)
  : Either[DLCompileError, OrFn] =
    if args.size == 2 then
      (args(0), args(1)) match
        case (l: BooleanFn, r: BooleanFn) =>
          Right(OrFn(l, r))
        case _ =>
          Left(DLCompileError(0, "|| requires two boolean expressions"))
    else
      Left(DLCompileError(0, "|| requires exactly two arguments"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case o: OrFn =>
        for
          _ <-  CompileFn.requireBoolean2(List(o.left.asInstanceOf[Fn[Any]], o.right.asInstanceOf[Fn[Any]]), "||", 0)
        yield ()
      case _ => Right(())

object CNotFn extends CompileFn[NotFn]:
  def name: String = "!"

  override def builtIn: Boolean = true

  def minArgs: Int = 1

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case ScalarType(_, "scala.Boolean") => true
      case OptionType(_, ScalarType(_, "scala.Boolean"), _) => true
      case _ => false

  def resultType(receiver: FieldType, args: List[FieldType])
                (using ctx: ExprContext): FieldType =
    receiver match
      case o: OptionType => o
      case _ => ScalarType("", "scala.Boolean")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext)
  : Either[DLCompileError, NotFn] =
    args.headOption match
      case Some(arg) => CompileFn.expectBoolean1(arg, "not", 0).map(b => NotFn(b.asInstanceOf[Fn[Any]]))
      case None => Left(DLCompileError(0, "! requires a single argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case n: NotFn =>
        for
          _ <-  CompileFn.requireBoolean1(n.operand.asInstanceOf[Fn[Any]], "!", 0)
        yield ()
      case _ => Right(())


object CIsDefinedFn extends CompileFn[IsDefinedFn]:
  val name = "isDefined"
  override val builtIn = false
  val minArgs = 0
  override val maxArgs = 0

  private def asTarget(recv: Option[Fn[Any]], args: List[Fn[Any]]): Either[DLCompileError, Fn[Any]] =
    recv.orElse(args.headOption)
      .toRight(DLCompileError(0, "isDefined() has no receiver"))

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    println("ZZZ: "+receiver)
    receiver match
      case OptionType(_, inner, _) =>
        // Allow isDefined on Option[List] or Option[Map]
        inner match
          case _: ListType | _: MapType => true
          case _ => true // also allow plain Option
      case _: ListType => true
      case _: MapType => true
      case _ => false

//  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
//    receiver.isInstanceOf[OptionType] // keep this strict; build() will find the real recv

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    ScalarType("", "scala.Boolean")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    asTarget(Option(recv), args).map(IsDefinedFn.apply)

  // in CIsDefinedFn (CompileFn for isDefined)
  override def validate(fn: Fn[?])(using ctx: ExprContext): Either[DLCompileError, Unit] = {
    println("[VALIDATE: isDefined] entering validate() --------------------")
    println(s"[VALIDATE: isDefined] ctx.symbols depth=${ctx.symbols.size}")
    fn.recv match {
      case Some(r) =>
        println(s"[VALIDATE: isDefined] recv class = ${r.getClass.getName}")
        r match {
          case g: GetFn =>
            println(s"[VALIDATE: isDefined] GetFn path = ${g.path}")
            val fromCtx = ctx.symbols.collectFirst {
              case scope if scope.contains(g.path) => scope(g.path)
            }
            println(s"[VALIDATE: isDefined] lookup from ctx = $fromCtx")
            val fromSchema = Utility.elementSchemaFor(g.path, ctx.schema)
            println(s"[VALIDATE: isDefined] lookup from schema = $fromSchema")
          case _ =>
            println(s"[VALIDATE: isDefined] recv not GetFn, class=${r.getClass.getSimpleName}")
        }
      case None =>
        println("[VALIDATE: isDefined] recv is None!")
    }
    // then your existing logic here
    super.validate(fn)
  }


object CStartsWithFn extends CompileFn[StartsWithFn]:
  val name: String = "startsWith"
  override val builtIn: Boolean = false

  val minArgs: Int = 1
  override val maxArgs: Int = 1

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case ScalarType(_, "java.lang.String")        => true
      case OptionType(_, ScalarType(_, "java.lang.String"), "scala.Option") => true
      case _ => false

  def resultType(receiver: FieldType, args: List[FieldType])
                (using ctx: ExprContext): FieldType =
    ScalarType("", "scala.Boolean")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption
      .toRight(DLCompileError(0, "startsWith requires one argument"))
      .map(arg => StartsWithFn(recv, arg))

  override def validate(fn: Fn[?])(using ctx: ExprContext)
  : Either[DLCompileError, Unit] =
    fn match
      case s: StartsWithFn =>
        Utility.rhsType(s.other) match
          case Some(ScalarType(_, "java.lang.String")) => Right(())
          case Some(OptionType(_, ScalarType(_, "java.lang.String"), "scala.Option")) => Right(())
          case Some(ft) =>
            Left(DLCompileError(0, s"startsWith requires a String argument, found ${ft.typeName}"))
          case None =>
            Left(DLCompileError(0, "startsWith cannot determine argument type"))
      case _ => Right(())


object CEndsWithFn extends CompileFn[EndsWithFn]:
  val name: String = "endsWith"
  override val builtIn: Boolean = false

  val minArgs: Int = 1
  override val maxArgs: Int = 1

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case ScalarType(_, "java.lang.String")        => true
      case OptionType(_, ScalarType(_, "java.lang.String"), "scala.Option") => true
      case _ => false

  def resultType(receiver: FieldType, args: List[FieldType])
                (using ctx: ExprContext): FieldType =
    ScalarType("", "scala.Boolean")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption
      .toRight(DLCompileError(0, "endsWith requires one argument"))
      .map(arg => EndsWithFn(recv, arg))

  override def validate(fn: Fn[?])(using ctx: ExprContext)
  : Either[DLCompileError, Unit] =
    fn match
      case e: EndsWithFn =>
        Utility.rhsType(e.other) match
          case Some(ScalarType(_, "java.lang.String")) => Right(())
          case Some(OptionType(_, ScalarType(_, "java.lang.String"), "scala.Option")) => Right(())
          case Some(ft) =>
            Left(DLCompileError(0, s"endsWith requires a String argument, found ${ft.typeName}"))
          case None =>
            Left(DLCompileError(0, "endsWith cannot determine argument type"))
      case _ => Right(())

object CContainsFn extends CompileFn[ContainsFn]:
  val name: String = "contains"
  override val builtIn: Boolean = false

  val minArgs: Int = 1
  override val maxArgs: Int = 1

  @tailrec
  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case ScalarType(_, "java.lang.String") => true
      case ListType(_, _, _)                 => true
      case MapType(_, _, _, _)               => true
      case OptionType(_, inner, _)           => accepts(inner)
      case _                                 => false

  def resultType(receiver: FieldType, args: List[FieldType])
                (using ctx: ExprContext): FieldType =
    ScalarType("", "scala.Boolean")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption
      .toRight(DLCompileError(0, "contains requires one argument"))
      .map(arg => ContainsFn(recv, arg))

  override def validate(fn: Fn[?])(using ctx: ExprContext)
  : Either[DLCompileError, Unit] =
    fn match
      case c: ContainsFn =>
        val recvTypeOpt = Utility.rhsType(c.receiver)
        val argTypeOpt = Utility.rhsType(c.other)

        (recvTypeOpt, argTypeOpt) match
          // --- String.contains(String) ---
          case (Some(ScalarType(_, "java.lang.String")), Some(ScalarType(_, "java.lang.String"))) =>
            Right(())

          // --- List[T].contains(T) ---
          case (Some(ListType(_, elemType, _)), Some(argT)) if elemType.conformsTo(argT) =>
            Right(())

          // --- Option[List[T]].contains(T) ---
          case (Some(OptionType(_, ListType(_, elemType, _), _)), Some(argT)) if elemType.conformsTo(argT) =>
            Right(())

          // --- Map[K,V].contains(K) ---
          case (Some(MapType(_, keyType, _, _)), Some(argT)) if keyType.conformsTo(argT) =>
            Right(())

          // --- Option[Map[K,V]].contains(K) ---
          case (Some(OptionType(_, MapType(_, keyType, _, _), _)), Some(argT)) if keyType.conformsTo(argT) =>
            Right(())

          case (Some(recv), Some(arg)) =>
            Left(DLCompileError(0,
              s"contains not applicable: receiver ${recv.typeName} with argument ${arg.typeName}"))

          case _ =>
            Left(DLCompileError(0, "contains cannot determine receiver or argument type"))

      case _ => Right(())


object CEqualsIgnoreCaseFn extends CompileFn[EqualsIgnoreCaseFn]:
  val name: String = "equalsIgnoreCase"
  override val builtIn: Boolean = false

  val minArgs: Int = 1
  override val maxArgs: Int = 1

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case ScalarType(_, "java.lang.String") => true
      case OptionType(_, ScalarType(_, "java.lang.String"), "scala.Option") => true
      case _ => false

  def resultType(receiver: FieldType, args: List[FieldType])
                (using ctx: ExprContext): FieldType =
    ScalarType("", "scala.Boolean")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption
      .toRight(DLCompileError(0, "equalsIgnoreCase requires one argument"))
      .map(arg => EqualsIgnoreCaseFn(recv, arg))

  override def validate(fn: Fn[?])(using ctx: ExprContext)
  : Either[DLCompileError, Unit] =
    fn match
      case e: EqualsIgnoreCaseFn =>
        Utility.rhsType(e.other) match
          case Some(ScalarType(_, "java.lang.String")) =>
            Right(())
          case Some(OptionType(_, ScalarType(_, "java.lang.String"), _)) =>
            Right(())
          case Some(ft) =>
            Left(DLCompileError(0,
              s"equalsIgnoreCase requires String argument, found ${ft.typeName}"))
          case None =>
            Left(DLCompileError(0, "equalsIgnoreCase cannot determine argument type"))
      case _ => Right(())


object CMatchesRegexFn extends CompileFn[MatchesRegexFn]:
  val name: String = "matchesRegex"
  override val builtIn: Boolean = false

  val minArgs: Int = 1
  override val maxArgs: Int = 1

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case ScalarType(_, "java.lang.String") => true
      case OptionType(_, ScalarType(_, "java.lang.String"), "scala.Option") => true
      case _ => false

  def resultType(receiver: FieldType, args: List[FieldType])
                (using ctx: ExprContext): FieldType =
    ScalarType("", "scala.Boolean")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption
      .toRight(DLCompileError(0, "matchesRegex requires one argument"))
      .map(arg => MatchesRegexFn(recv, arg))

  override def validate(fn: Fn[?])(using ctx: ExprContext)
  : Either[DLCompileError, Unit] =
    fn match
      case m: MatchesRegexFn =>
        Utility.rhsType(m.other) match
          case Some(ScalarType(_, "java.lang.String")) =>
            Right(())
          case Some(OptionType(_, ScalarType(_, "java.lang.String"), _)) =>
            Right(())
          case Some(ft) =>
            Left(DLCompileError(0,
              s"matchesRegex requires String argument, found ${ft.typeName}"))
          case None =>
            Left(DLCompileError(0, "matchesRegex cannot determine argument type"))

      case _ => Right(())