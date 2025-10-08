package co.blocke.dynalens
package parser
package fn

// ---------------------------------
// List Prepend (::)
// ---------------------------------
object CConsFn extends CompileFn[ConsFn]:
  val name = "::"
  val minArgs = 2
  override val builtIn = true

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean = true

  override def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    (args.headOption, args.lift(1)) match
      case (Some(elem: ScalarType), Some(list @ ListType(_, e, container))) =>
        val outElem = if e.typeName == "scala.Any" then elem else e
        ListType("", outElem, container)

      case (Some(list1 @ ListType(_, e1, c1)), Some(list2 @ ListType(_, e2, c2))) =>
        val outElem =
          if e1.typeName == "scala.Any" then e2
          else if e2.typeName == "scala.Any" then e1
          else e2
        ListType("", outElem, c2)

      // 👇 NEW CASE: allow elem :: elem (promote rhs)
      case (Some(_: ScalarType), Some(_: ScalarType)) =>
        ListType("", ScalarType("", "scala.Any"), "scala.collection.immutable.List")

      case _ =>
        ListType("", ScalarType("", "scala.Any"), "scala.collection.immutable.List")

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case c: ConsFn =>
        (Utility.rhsType(c.left), Utility.rhsType(c.right)) match
          case (Some(elem: ScalarType), Some(ListType(_, e, _))) =>
            if e.typeName == "scala.Any" || e.typeName == elem.typeName then Right(())
            else Left(DLCompileError(0, s":: requires matching element/list types, found ${elem.typeName} and List[${e.typeName}]"))

          case (Some(ListType(_, e1, _)), Some(ListType(_, e2, _))) =>
            if e1.typeName == "scala.Any" || e2.typeName == "scala.Any" || e1.typeName == e2.typeName then Right(())
            else Left(DLCompileError(0, s":: requires lists of same element type, found List[${e1.typeName}] and List[${e2.typeName}]"))

          case (Some(_: ScalarType), Some(_: ScalarType)) =>
            Right(())

          case (Some(lt), Some(rt)) =>
            Left(DLCompileError(0, s":: requires element :: list or list :: list, found ${lt.typeName} and ${rt.typeName}"))

          case _ =>
            Left(DLCompileError(0, ":: cannot determine operand types"))
      case _ => Right(())

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext): Either[DLCompileError, ConsFn] =
    (args.headOption, args.lift(1)) match
      // 👇 promote scalar :: scalar → scalar :: List(scalar)
      case (Some(left), Some(right)) =>
        val rightType = Utility.rhsType(right)
        rightType match
          case Some(_: ListType) =>
            Right(ConsFn(left, right))
          case Some(_: ScalarType) =>
            Right(ConsFn(left, ConstantFn(List(right)))) // promote to list
          case _ =>
            Right(ConsFn(left, right))
      case _ =>
        Left(DLCompileError(0, s"$name missing right arg"))

        /*
object CConsFn extends CompileFn[ConsFn]:
  val name = "::"
  val minArgs = 2
  override val builtIn = true

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean = true

  override def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    (args.headOption, args.lift(1)) match
      // elem :: list
      case (Some(elem: ScalarType), Some(list @ ListType(_, e, container))) =>
        val outElem =
          if e.typeName == "scala.Any" then elem else e
        ListType("", outElem, container)

      // list1 :: list2 (concatenate)
      case (Some(list1 @ ListType(_, e1, c1)), Some(list2 @ ListType(_, e2, c2))) =>
        val outElem =
          if e1.typeName == "scala.Any" then e2
          else if e2.typeName == "scala.Any" then e1
          else e2
        ListType("", outElem, c2)

      // fallback
      case _ =>
        ListType("", ScalarType("", "scala.Any"), "scala.collection.immutable.List")

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case c: ConsFn =>
        (Utility.rhsType(c.left), Utility.rhsType(c.right)) match
          // elem :: list
          case (Some(elem: ScalarType), Some(ListType(_, e, _))) =>
            if e.typeName == "scala.Any" || e.typeName == elem.typeName then Right(())
            else Left(DLCompileError(0, s":: requires matching element/list types, found ${elem.typeName} and List[${e.typeName}]"))

          // list :: list
          case (Some(ListType(_, e1, _)), Some(ListType(_, e2, _))) =>
            if e1.typeName == "scala.Any" || e2.typeName == "scala.Any" || e1.typeName == e2.typeName then Right(())
            else Left(DLCompileError(0, s":: requires lists of same element type, found List[${e1.typeName}] and List[${e2.typeName}]"))

          // promote when RHS isn't list
          case (Some(lt), Some(rt)) if !rt.isInstanceOf[ListType] =>
            Right(())

          case _ =>
            Left(DLCompileError(0, ":: cannot determine operand types"))
      case _ => Right(())

  def build(
             recv: Fn[Any],
             args: List[Fn[Any]]
           )(using ctx: ExprContext): Either[DLCompileError, ConsFn] =
    (args.headOption, args.lift(1)) match
      // Normal case: elem :: list
      case (Some(left), Some(right)) =>
        Right(ConsFn(left, right))

      // Single right operand (e.g. "a" :: "b") – promote to singleton list
      case (Some(left), None) =>
        val promoted = promoteToList(left).asInstanceOf[Fn[Any]]
        Right(ConsFn(left, promoted))

      case _ =>
        Left(DLCompileError(0, s"$name missing right arg"))

  private def promoteToList(fn: Fn[Any]): ConstantFn[List[Any]] =
    fn match
      case const: ConstantFn[_] =>
        ConstantFn(List(const.out.asInstanceOf[Any]))
      case other =>
        ConstantFn(List(other.asInstanceOf[Any]))
        */


object CKeysFn extends CompileFn[KeysFn]:
  val name     = "keys"
  override val builtIn = false
  val minArgs  = 0

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case _: MapType                          => true
      case OptionType(_, inner: MapType, _)    => true
      case _                                   => false

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    receiver match
      // plain Map[K,V] → List[K]
      case MapType(_, key, _, container) =>
        ListType("", key, s"scala.collection.immutable.List[${key.typeName}]")

      // Option[Map[K,V]] → Option[List[K]]
      case OptionType(_, inner: MapType, _) =>
        OptionType(
          "",
          ListType("", inner.keyType, s"scala.collection.immutable.List[${inner.keyType.typeName}]"),
          "scala.Option"
        )

      case _ =>
        ScalarType("", "scala.Any")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(0, "keys() takes no arguments"))
    else Right(KeysFn(recv))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case k: KeysFn =>
        k.recv.flatMap(Utility.rhsType) match
          case Some(_: MapType)                      => Right(())
          case Some(OptionType(_, _: MapType, _))    => Right(())
          case _ => Left(DLCompileError(0, "keys() requires a Map or Option[Map] receiver"))
      case _ => Right(())


object CValuesFn extends CompileFn[ValuesFn]:
  val name     = "values"
  override val builtIn = false
  val minArgs  = 0

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case _: MapType                          => true
      case OptionType(_, inner: MapType, _)    => true
      case _                                   => false

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    receiver match
      // plain Map[K,V] → List[V]
      case MapType(_, _, value, _) =>
        ListType("", value, s"scala.collection.immutable.List[${value.typeName}]")

      // Option[Map[K,V]] → Option[List[V]]
      case OptionType(_, inner: MapType, _) =>
        OptionType(
          "",
          ListType("", inner.valueType, s"scala.collection.immutable.List[${inner.valueType.typeName}]"),
          "scala.Option"
        )

      case _ =>
        ScalarType("", "scala.Any")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(0, "values() takes no arguments"))
    else Right(ValuesFn(recv))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case v: ValuesFn =>
        v.recv.flatMap(Utility.rhsType) match
          case Some(_: MapType)                      => Right(())
          case Some(OptionType(_, _: MapType, _))    => Right(())
          case _ => Left(DLCompileError(0, "values() requires a Map or Option[Map] receiver"))
      case _ => Right(())


object CMapGetFn extends CompileFn[MapGetFn]:
  val name     = "get"
  override val builtIn = false
  val minArgs  = 1
  override val maxArgs = 1

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case _: MapType                       => true
      case OptionType(_, _: MapType, _)     => true
      case _                                => false

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    receiver match
      // Map[K,V] → Option[V]
      case MapType(_, _, value, _) =>
        OptionType("", value, "scala.Option")

      // Option[Map[K,V]] → Option[Option[V]] (nested, since get() always returns Option)
      case OptionType(_, inner: MapType, _) =>
        OptionType("", OptionType("", inner.valueType, "scala.Option"), "scala.Option")

      case _ =>
        ScalarType("", "scala.Any")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption
      .toRight(DLCompileError(0, "get() requires a key argument"))
      .map(key => MapGetFn(recv, key))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case m: MapGetFn =>
        for
          recvType <- m.recv.flatMap(Utility.rhsType)
            .toRight(DLCompileError(0, "get() requires a Map receiver"))
          argType <- Utility.rhsType(m.other)
            .toRight(DLCompileError(0, "get() key type cannot be resolved"))
          _ <- recvType match
            case MapType(_, keyType, _, _) if keyType.typeName == argType.typeName =>
              Right(())
            case OptionType(_, inner: MapType, _) if inner.keyType.typeName == argType.typeName =>
              Right(())
            case MapType(_, keyType, _, _) =>
              Left(DLCompileError(0, s"get() key type mismatch: expected ${keyType.typeName}, got ${argType.typeName}"))
            case OptionType(_, inner: MapType, _) =>
              Left(DLCompileError(0, s"get() key type mismatch: expected ${inner.keyType.typeName}, got ${argType.typeName}"))
            case _ =>
              Left(DLCompileError(0, "get() requires a Map or Option[Map] receiver"))
        yield ()
      case _ => Right(())


object CFilterFn extends CompileFn[FilterFn]:
  val name = "filter"
  val minArgs = 1
  override val maxArgs = 1

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    val res = receiver match
      case ListType(_, _, _) => true
      case OptionType(_, l: ListType, _) => true
      case _ => false
    println(s"[DEBUG filter.accepts] receiver=$receiver => $res")
    res

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    receiver

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    println(s"[DEBUG filter.build] receiver= $recv")
    args.headOption.toRight(DLCompileError(0, "filter() requires a predicate")).flatMap { rawPred =>
      Utility.rhsType(recv) match
        case Some(_: ListType) | Some(OptionType(_, _: ListType, _)) =>
          // OK, now (and only now) type-check the predicate in an element context
          val elemType = Utility.elementTypeOf(recv)(using ctx)
          val elemFields = elemType match
            case c: ClassType => c.fields
            case o: OptionType if o.valueType.isInstanceOf[ClassType] =>
              o.valueType.asInstanceOf[ClassType].fields
            case _ => Nil

          val rcvr = Receiver("this", elemFields.map(ft => ft.name -> ft).toMap, elemType, parentFn = Some(recv))
          val ctxEnriched = ctx.withReceiver(rcvr).pushScope(elemFields)

          Utility.rhsType(rawPred)(using ctxEnriched) match
            case Some(ft) if ft.typeName == "scala.Boolean" =>
              Right(FilterFn(recv, rawPred.withReceiver(recv).asInstanceOf[Fn[Any]]))
            case Some(ft) =>
              Left(DLCompileError(0, s"filter() requires boolean predicate, got ${ft.typeName}"))
            case None =>
              Left(DLCompileError(0, "filter() cannot resolve predicate type"))

        case Some(ft) =>
          // Non-list receiver -> fail *without* touching the predicate
          Left(DLCompileError(0, s"Method 'filter' cannot be applied to receiver of type ${ft.typeName}"))

        case None =>
          Left(DLCompileError(0, "filter() cannot resolve receiver type"))
    }

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case f: FilterFn =>
        Utility.rhsType(f.other) match
          case Some(ft) if ft.typeName == "scala.Boolean" => Right(())
          case Some(ft) =>
            Left(DLCompileError(0, s"filter() requires boolean predicate, got ${ft.typeName}"))
          case None =>
            Left(DLCompileError(0, "filter() cannot resolve predicate type"))
      case _ => Right(())


object CSortAscFn extends CompileFn[SortAscFn]:
  val name = "sortAsc"
  val minArgs = 0
  override val maxArgs = 1

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case _: ListType                   => true
      case OptionType(_, _: ListType, _) => true
      case _                             => false

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    // preserves List or Option[List]
    receiver

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    args match
      case Nil                 => Right(SortAscFn(recv, None))
      case List(a1: GetFn)     => Right(SortAscFn(recv, Some(a1.path)))
      case List(_)             => Left(DLCompileError(0, "sortAsc() argument must be a field path"))
      case _                   => Left(DLCompileError(0, s"sortAsc() expected 0 or 1 arg, got ${args.length}"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case s: SortAscFn =>
        s.recv.flatMap(Utility.rhsType) match
          case Some(ListType(_, elem, _)) if elem.isNumeric || elem.isStringLike || elem.isInstanceOf[ClassType] =>
            Right(())
          case Some(ListType(_, elem, _)) =>
            Left(DLCompileError(0, s"sortAsc() requires sortable element type, got ${elem.typeName}"))
          case Some(OptionType(_, inner: ListType, _)) if inner.elementType.isNumeric || inner.elementType.isStringLike || inner.elementType.isInstanceOf[ClassType] =>
            Right(())
          case Some(OptionType(_, inner: ListType, _)) =>
            Left(DLCompileError(0, s"sortAsc() requires sortable element type, got ${inner.elementType.typeName}"))
          case _ =>
            Left(DLCompileError(0, "sortAsc() requires a List or Option[List] receiver"))
      case _ => Right(())


object CSortDescFn extends CompileFn[SortDescFn]:
  val name = "sortDesc"
  val minArgs = 0
  override val maxArgs = 1

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case _: ListType                   => true
      case OptionType(_, _: ListType, _) => true
      case _                             => false

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    receiver

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    args match
      case Nil                 => Right(SortDescFn(recv, None))
      case List(a1: GetFn)     => Right(SortDescFn(recv, Some(a1.path)))
      case List(_)             => Left(DLCompileError(0, "sortDesc() argument must be a field path"))
      case _                   => Left(DLCompileError(0, s"sortDesc() expected 0 or 1 arg, got ${args.length}"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case s: SortDescFn =>
        s.recv.flatMap(Utility.rhsType) match
          case Some(ListType(_, elem, _)) if elem.isNumeric || elem.isStringLike || elem.isInstanceOf[ClassType] =>
            Right(())
          case Some(ListType(_, elem, _)) =>
            Left(DLCompileError(0, s"sortDesc() requires sortable element type, got ${elem.typeName}"))
          case Some(OptionType(_, inner: ListType, _)) if inner.elementType.isNumeric || inner.elementType.isStringLike || inner.elementType.isInstanceOf[ClassType] =>
            Right(())
          case Some(OptionType(_, inner: ListType, _)) =>
            Left(DLCompileError(0, s"sortDesc() requires sortable element type, got ${inner.elementType.typeName}"))
          case _ =>
            Left(DLCompileError(0, "sortDesc() requires a List or Option[List] receiver"))
      case _ => Right(())


object CDistinctFn extends CompileFn[DistinctFn]:
  val name = "distinct"
  val minArgs = 0

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case _: ListType                   => true
      case OptionType(_, _: ListType, _) => true
      case _                             => false

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    receiver // keep the wrapper (List or Option[List])

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    args match
      // no args → distinct by entire element
      case Nil => Right(DistinctFn(recv, None))

      // one constant string arg → distinct by that field
      case List(ConstantFn(s: String)) => Right(DistinctFn(recv, Some(s)))

      case List(GetFn(name, _, _)) =>
        // interpret GetFn(x) as the *field name string* 'x'
        Right(DistinctFn(recv, Some(name)))

      // wrong arity
      case _ => Left(DLCompileError(0, "distinct() takes zero or one constant string argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case d: DistinctFn =>
        d.recv.flatMap(Utility.rhsType) match
          case Some(_: ListType)                        => Right(())
          case Some(OptionType(_, _: ListType, _))      => Right(())
          case _                                        => Left(DLCompileError(0, "distinct() requires a List or Option[List] receiver"))
      case _ => Right(())


object CLimitFn extends CompileFn[LimitFn]:
  val name = "limit"
  val minArgs = 1
  override val maxArgs = 1

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case _: ListType                   => true
      case OptionType(_, _: ListType, _) => true
      case _                             => false

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    receiver

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    args match
      case List(ConstantFn(i: Int))  if i >= 0 => Right(LimitFn(recv, i))
      case List(ConstantFn(l: Long)) if l >= 0 => Right(LimitFn(recv, l.toInt))
      case List(ConstantFn(bad: Int))          => Left(DLCompileError(0, s"limit() requires non-negative integer, got $bad"))
      case Nil                                 => Left(DLCompileError(0, "limit() requires a non-negative integer argument"))
      case _                                   => Left(DLCompileError(0, "limit() only accepts a non-negative integer constant argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case l: LimitFn =>
        l.recv.flatMap(Utility.rhsType) match
          case Some(_: ListType) | Some(OptionType(_, _: ListType, _)) =>
            if l.count >= 0 then Right(())
            else Left(DLCompileError(0, s"limit() requires a non-negative integer, got ${l.count}"))
          case _ =>
            Left(DLCompileError(0, "limit() requires a List or Option[List] receiver"))
      case _ => Right(())


object CReverseFn extends CompileFn[ReverseFn]:
  val name    = "reverse"
  val minArgs = 0

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case _: ListType                   => true
      case OptionType(_, _: ListType, _) => true
      case _                             => false

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    receiver // reverse doesn’t change element type, just order

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(0, "reverse() takes no arguments"))
    else Right(ReverseFn(recv))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case r: ReverseFn =>
        r.recv.flatMap(Utility.rhsType) match
          case Some(_: ListType) | Some(OptionType(_, _: ListType, _)) => Right(())
          case Some(ft) => Left(DLCompileError(0, s"reverse() requires a List receiver, got ${ft.typeName}"))
          case None     => Left(DLCompileError(0, "reverse() cannot determine receiver type"))
      case _ => Right(())


object CCleanFn extends CompileFn[CleanFn]:
  val name    = "clean"
  val minArgs = 0

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case ListType(_, _, _) => true
      case OptionType(_, _: ListType, _) => true
      case _ => false

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    receiver match
      case ListType(_, OptionType(_, inner, _), coll) =>
        ListType("", inner, coll) // remove inner Option
      case OptionType(_, ListType(_, OptionType(_, inner, _), coll), opt) =>
        OptionType("", ListType("", inner, coll), opt) // unwrap both
      case _ =>
        receiver // if no Option nesting, no-op (type stays the same)

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case c: CleanFn =>
        c.recv.flatMap(Utility.rhsType) match
          case Some(ListType(_, _, _)) |
               Some(OptionType(_, _: ListType, _)) =>
            Right(()) // valid — includes lists of Option or plain
          case Some(ft) =>
            Left(DLCompileError(0, s"clean() requires a List[_] receiver, got ${ft.typeName}"))
          case None =>
            Left(DLCompileError(0, "clean() cannot determine receiver type"))
      case _ => Right(())

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(0, "clean() takes no arguments"))
    else Right(CleanFn(recv))


object CLenFn extends CompileFn[LenFn]:
  val name = "len"
  override val builtIn = true
  val minArgs = 0

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case ScalarType(_, "java.lang.String") => true
      case ListType(_, _, _) => true
      case MapType(_, _, _, _) => true
      case OptionType(_, inner, _) => accepts(inner) // unwrap option
      case _ => false

  def resultType(receiver: FieldType, args: List[FieldType])
                (using ctx: ExprContext): FieldType =
    ScalarType("", "scala.Int")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(0, "len() takes no arguments"))
    else Right(LenFn(recv))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case l: LenFn =>
        l.recv.flatMap(Utility.rhsType) match
          case Some(r) if accepts(r) => Right(())
          case Some(r) =>
            Left(DLCompileError(0, s"len() cannot be applied to ${r.typeName}"))
          case None =>
            Left(DLCompileError(0, "len() cannot resolve receiver type"))
      case _ => Right(())


object CMapToFn extends CompileFn[MapToFn]:
  val name = "mapTo"           // DSL keyword
  override val builtIn = false  // user-supplied mapping registry
  val minArgs = 1
  override val maxArgs = 1

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case _: ScalarType => true
      case ListType(_, _: ScalarType, _) => true
      case _ => false

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    receiver match
      case ListType(_, _: ScalarType, coll) =>
        ListType("", ScalarType("", "java.lang.String"), coll)
      case _ =>
        ScalarType("", "java.lang.String")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption match
      case Some(ConstantFn(mapName: String)) =>
        Right(MapToFn(mapName, recv))
      case Some(other) =>
        Left(DLCompileError(0, s"mapTo() expects a string constant map name, got ${other.getClass.getSimpleName}"))
      case None =>
        Left(DLCompileError(0, "mapTo() requires a map name argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case m: MapToFn =>
        // First, try the normal rhsType lookup
        val recvTypeOpt = Utility.rhsType(m.receiver)
          .orElse {
            m.receiver match
              case GetFn(path, _, _) =>
                Some(Utility.getPathType(path))
              case _ => None
          }

        recvTypeOpt match
          case Some(_: ScalarType) => Right(())
          case Some(ListType(_, _: ScalarType, _)) => Right(())
          case Some(ft) =>
            Left(DLCompileError(0, s"mapTo requires a scalar or list of scalars, got ${ft.typeName}"))
          case None =>
            Left(DLCompileError(0, "mapTo cannot determine receiver type"))

      case _ => Right(())


object CMapFromFn extends CompileFn[MapFromFn]:
  val name = "mapFrom"          // DSL keyword
  override val builtIn = false   // user-supplied mapping registry
  val minArgs = 1
  override val maxArgs = 1

  def accepts(receiver: FieldType)(using ctx: ExprContext): Boolean =
    receiver match
      case _: ScalarType => true
      case ListType(_, _: ScalarType, _) => true
      case _ => false

  def resultType(receiver: FieldType, args: List[FieldType])(using ctx: ExprContext): FieldType =
    receiver match
      case ListType(_, _: ScalarType, coll) =>
        ListType("", ScalarType("", "java.lang.String"), coll)
      case _ =>
        ScalarType("", "java.lang.String")

  def build(recv: Fn[Any], args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption match
      case Some(ConstantFn(mapName: String)) =>
        Right(MapFromFn(mapName, recv))
      case Some(other) =>
        Left(DLCompileError(0, s"mapFrom expects a string constant map name, got ${other.getClass.getSimpleName}"))
      case None =>
        Left(DLCompileError(0, "mapFrom requires a map name argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case m: MapFromFn =>
        Utility.rhsType(m.receiver) match
          case Some(_: ScalarType) => Right(())
          case Some(ListType(_, _: ScalarType, _)) => Right(())
          case Some(ft) =>
            Left(DLCompileError(0, s"mapFrom requires a scalar or list of scalars, got ${ft.typeName}"))
          case None =>
            Left(DLCompileError(0, "mapFrom cannot determine receiver type"))
      case _ => Right(())