package co.blocke.dynalens
package parser
package fn

import co.blocke.dynalens.fn.*


object CConsFn extends CompileFn[ConsFn]:
  val name = "::"
  val minArgs = 2

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(ft: ListType) if !ft.isOptional => true
      case Some(ft: ListType) if ft.isOptional => true
      case _ => false

  override def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(listType: ListType) =>
        val isOpt = listType.isOptional
        (args.headOption, args.lift(1)) match
          case (Some(elem: ScalarType), Some(ListType(_, e, container, _))) =>
            val outElem = if e.typeName == "scala.Any" then elem else e
            ListType("", outElem, container, isOpt)
          case (Some(ListType(_, e1, c1, _)), Some(ListType(_, e2, c2, _))) =>
            val outElem =
              if e1.typeName == "scala.Any" then e2
              else if e2.typeName == "scala.Any" then e1
              else e2
            ListType("", outElem, c2, isOpt)
          case (Some(_: ScalarType), Some(_: ScalarType)) =>
            ListType("", ScalarType("", "scala.Any"), "scala.collection.immutable.List", isOpt)
          case _ =>
            ListType("", ScalarType("", "scala.Any"), "scala.collection.immutable.List", isOpt)
      case _ =>
        ListType("", ScalarType("", "scala.Any"), "scala.collection.immutable.List", false)

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    (args.headOption, args.lift(1)) match
      case (Some(left), Some(right)) =>
        Utility.rhsType(right)(using ctx) match
          case Some(_: ListType) =>
            Right(ConsFn(left, right, ctx.posStr))
          case Some(_: ScalarType) =>
            Right(ConsFn(left, ConstantFn(List(right)), ctx.posStr))
          case _ =>
            Right(ConsFn(left, right, ctx.posStr))
      case _ =>
        Left(DLCompileError(ctx.posStr, s"$name missing right arg"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case c: ConsFn =>
        (Utility.rhsType(c.left)(using ctx), Utility.rhsType(c.right)(using ctx)) match
          case (Some(elem: ScalarType), Some(ListType(_, e, _, _))) =>
            if e.typeName == "scala.Any" || e.typeName == elem.typeName then Right(())
            else Left(DLCompileError(ctx.posStr, s":: requires matching element/list types, found ${elem.typeName} and List[${e.typeName}]"))
          case (Some(ListType(_, e1, _, _)), Some(ListType(_, e2, _, _))) =>
            if e1.typeName == "scala.Any" || e2.typeName == "scala.Any" || e1.typeName == e2.typeName then Right(())
            else Left(DLCompileError(ctx.posStr, s":: requires lists of same element type, found List[${e1.typeName}] and List[${e2.typeName}]"))
          case (Some(_: ScalarType), Some(_: ScalarType)) =>
            Right(())
          case (Some(lt), Some(rt)) =>
            Left(DLCompileError(ctx.posStr, s":: requires element :: list or list :: list, found ${lt.typeName} and ${rt.typeName}"))
          case _ =>
            Left(DLCompileError(ctx.posStr, ":: cannot determine operand types"))
      case _ => Right(())


// --------------------------------------------------
// keys()
// --------------------------------------------------
object CKeysFn extends CompileFn[KeysFn]:
  val name = "keys"
  val minArgs = 0

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(ft: MapType) => true
      case Some(ft: FieldType) if ft.isOptional && ft.isInstanceOf[MapType] => true
      case _ => false

  override def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(mapType: MapType) =>
        ListType("", mapType.keyType, "scala.collection.immutable.List", mapType.isOptional)
      case _ => ScalarType("", "scala.Any")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, "keys() takes no arguments"))
    else Right(KeysFn(recv.fn, ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case k: KeysFn =>
        Utility.rhsType(k.recv)(using ctx) match
          case Some(_: MapType) => Right(())
          case Some(ft: FieldType) if ft.isOptional && ft.isInstanceOf[MapType] => Right(())
          case _ => Left(DLCompileError(ctx.posStr, "keys() requires a Map or Option[Map] receiver"))
      case _ => Right(())


// --------------------------------------------------
// values()
// --------------------------------------------------
object CValuesFn extends CompileFn[ValuesFn]:
  val name = "values"
  val minArgs = 0

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(ft: MapType) => true
      case Some(ft: FieldType) if ft.isOptional && ft.isInstanceOf[MapType] => true
      case _ => false

  override def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(mapType: MapType) =>
        ListType("", mapType.valueType, "scala.collection.immutable.List", mapType.isOptional)
      case _ =>
        ScalarType("", "scala.Any")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, "values() takes no arguments"))
    else Right(ValuesFn(recv.fn, ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case v: ValuesFn =>
        Utility.rhsType(v.recv)(using ctx) match
          case Some(_: MapType) => Right(())
          case Some(ft: FieldType) if ft.isOptional && ft.isInstanceOf[MapType] => Right(())
          case _ => Left(DLCompileError(ctx.posStr, "values() requires a Map or Option[Map] receiver"))
      case _ => Right(())


// --------------------------------------------------
// filter()
// --------------------------------------------------
object CFilterFn extends CompileFn[FilterFn]:
  val name = "filter"
  val minArgs = 1
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(ft: ListType) => true
      case _ => false

  override def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(listType: ListType) =>
        ListType("", listType.elementType, listType.typeName, listType.isOptional)
      case _ =>
        ScalarType("", "scala.Any")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption.toRight(DLCompileError(ctx.posStr, "filter() requires a predicate")).flatMap { rawPred =>
      val elemType = Utility.rhsType(recv.fn)(using ctx) match
        case Some(ListType(_, e, _, _)) => e
        case Some(ft: FieldType) if ft.isOptional && ft.isInstanceOf[ListType] =>
          ft.asInstanceOf[ListType].elementType
        case _ => ScalarType("", "scala.Any")
      val elemFields = elemType match
        case c: ClassType => c.fields
        case _ => Nil
      val rcvr = NamedReceiver("this", elemType, recv.fn)
      val ctxEnriched = ctx.withReceiver(rcvr).pushScope(elemFields)
      Utility.rhsType(recv.fn)(using ctx) match
        case Some(_: ListType) =>
          Utility.rhsType(rawPred)(using ctxEnriched) match
            case Some(ft) if ft.typeName == "scala.Boolean" =>
              Right(FilterFn(recv.fn, rawPred.withReceiver(recv.fn).asInstanceOf[Fn[Any]], ctx.posStr))
            case Some(ft) =>
              Left(DLCompileError(ctx.posStr, s"filter() requires boolean predicate, got ${ft.typeName}"))
            case None =>
              Left(DLCompileError(ctx.posStr, "filter() cannot resolve predicate type"))
        case Some(ft: FieldType) if ft.isOptional && ft.isInstanceOf[ListType] =>
          Utility.rhsType(rawPred)(using ctxEnriched) match
            case Some(ft) if ft.typeName == "scala.Boolean" =>
              Right(FilterFn(recv.fn, rawPred.withReceiver(recv.fn).asInstanceOf[Fn[Any]], ctx.posStr))
            case Some(ft) =>
              Left(DLCompileError(ctx.posStr, s"filter() requires boolean predicate, got ${ft.typeName}"))
            case None =>
              Left(DLCompileError(ctx.posStr, "filter() cannot resolve predicate type"))
        case Some(ft) =>
          Left(DLCompileError(ctx.posStr, s"filter() cannot be applied to receiver of type ${ft.typeName}"))
        case None =>
          Left(DLCompileError(ctx.posStr, "filter() cannot resolve receiver type"))
    }

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case f: FilterFn =>
        Utility.rhsType(f.other)(using ctx) match
          case Some(ft) if ft.typeName == "scala.Boolean" => Right(())
          case Some(ft) => Left(DLCompileError(ctx.posStr, s"filter() requires boolean predicate, got ${ft.typeName}"))
          case None => Left(DLCompileError(ctx.posStr, "filter() cannot resolve predicate type"))
      case _ => Right(())


// --------------------------------------------------
// sortAsc()
// --------------------------------------------------
object CSortAscFn extends CompileFn[SortAscFn]:
  val name = "sortAsc"
  val minArgs = 0
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(ft: ListType) => true
      case _ => false

  override def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(listType: ListType) =>
        ListType("", listType.elementType, listType.typeName, listType.isOptional)
      case _ =>
        ScalarType("", "scala.Any")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args match
      case Nil => Right(SortAscFn(recv.fn, None, ctx.posStr))
      case List(a1: GetFn) => Right(SortAscFn(recv.fn, Some(a1.path), ctx.posStr))
      case List(_) => Left(DLCompileError(ctx.posStr, "sortAsc() argument must be a field path"))
      case _ => Left(DLCompileError(ctx.posStr, s"sortAsc() expected 0 or 1 arg, got ${args.length}"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case s: SortAscFn =>
        Utility.rhsType(s.recv) match
          case Some(ListType(_, elem, _, _)) if elem.isNumeric || elem.isStringLike || elem.isInstanceOf[ClassType] => Right(())
          case Some(ListType(_, elem, _, _)) =>
            Left(DLCompileError(ctx.posStr, s"sortAsc() requires sortable element type, got ${elem.typeName}"))
          case Some(ft: FieldType) if ft.isOptional && ft.isInstanceOf[ListType] =>
            val inner = ft.asInstanceOf[ListType]
            if inner.elementType.isNumeric || inner.elementType.isStringLike || inner.elementType.isInstanceOf[ClassType] then Right(())
            else Left(DLCompileError(ctx.posStr, s"sortAsc() requires sortable element type, got ${inner.elementType.typeName}"))
          case _ => Left(DLCompileError(ctx.posStr, "sortAsc() requires a List or Option[List] receiver"))
      case _ => Right(())


// --------------------------------------------------
// sortDesc()
// --------------------------------------------------
object CSortDescFn extends CompileFn[SortDescFn]:
  val name = "sortDesc"
  val minArgs = 0
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(ft: ListType) => true
      case _ => false

  override def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(listType: ListType) =>
        ListType("", listType.elementType, listType.typeName, listType.isOptional)
      case _ =>
        ScalarType("", "scala.Any")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args match
      case Nil => Right(SortDescFn(recv.fn, None, ctx.posStr))
      case List(a1: GetFn) => Right(SortDescFn(recv.fn, Some(a1.path), ctx.posStr))
      case List(_) => Left(DLCompileError(ctx.posStr, "sortDesc() argument must be a field path"))
      case _ => Left(DLCompileError(ctx.posStr, s"sortDesc() expected 0 or 1 arg, got ${args.length}"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case s: SortDescFn =>
        Utility.rhsType(s.recv) match
          case Some(ListType(_, elem, _, _)) if elem.isNumeric || elem.isStringLike || elem.isInstanceOf[ClassType] => Right(())
          case Some(ListType(_, elem, _, _)) =>
            Left(DLCompileError(ctx.posStr, s"sortDesc() requires sortable element type, got ${elem.typeName}"))
          case Some(ft: FieldType) if ft.isOptional && ft.isInstanceOf[ListType] =>
            val inner = ft.asInstanceOf[ListType]
            if inner.elementType.isNumeric || inner.elementType.isStringLike || inner.elementType.isInstanceOf[ClassType] then Right(())
            else Left(DLCompileError(ctx.posStr, s"sortDesc() requires sortable element type, got ${inner.elementType.typeName}"))
          case _ => Left(DLCompileError(ctx.posStr, "sortDesc() requires a List or Option[List] receiver"))
      case _ => Right(())


// --------------------------------------------------
// distinct()
// --------------------------------------------------
object CDistinctFn extends CompileFn[DistinctFn]:
  val name = "distinct"
  val minArgs = 0

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(ft: ListType) => true
      case _ => false

  override def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(listType: ListType) =>
        ListType("", listType.elementType, listType.typeName, listType.isOptional)
      case _ =>
        ScalarType("", "scala.Any")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args match
      case Nil => Right(DistinctFn(recv.fn, None, ctx.posStr))
      case List(ConstantFn(s: String)) => Right(DistinctFn(recv.fn, Some(s), ctx.posStr))
      case List(GetFn(name, _, _,_)) => Right(DistinctFn(recv.fn, Some(name), ctx.posStr))
      case _ => Left(DLCompileError(ctx.posStr, "distinct() takes zero or one constant string argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case d: DistinctFn =>
        Utility.rhsType(d.recv) match
          case Some(_: ListType) => Right(())
          case Some(ft: FieldType) if ft.isOptional && ft.isInstanceOf[ListType] => Right(())
          case _ => Left(DLCompileError(ctx.posStr, "distinct() requires a List or Option[List] receiver"))
      case _ => Right(())


// --------------------------------------------------
// limit()
// --------------------------------------------------
object CLimitFn extends CompileFn[LimitFn]:
  val name = "limit"
  val minArgs = 1
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(ft: ListType) => true
      case _ => false

  override def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(listType: ListType) =>
        ListType("", listType.elementType, listType.typeName, listType.isOptional)
      case _ =>
        ScalarType("", "scala.Any")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args match
      case List(ConstantFn(i: Int))  if i >= 0 => Right(LimitFn(recv.fn, i, ctx.posStr))
      case List(ConstantFn(l: Long)) if l >= 0 => Right(LimitFn(recv.fn, l.toInt, ctx.posStr))
      case List(ConstantFn(bad: Int)) => Left(DLCompileError(ctx.posStr, s"limit() requires non-negative integer, got $bad"))
      case Nil => Left(DLCompileError(ctx.posStr, "limit() requires a non-negative integer argument"))
      case _ => Left(DLCompileError(ctx.posStr, "limit() only accepts a non-negative integer constant argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case l: LimitFn =>
        Utility.rhsType(l.recv) match
          case Some(_: ListType) =>
            if l.count >= 0 then Right(())
            else Left(DLCompileError(ctx.posStr, s"limit() requires a non-negative integer, got ${l.count}"))
          case Some(ft: FieldType) if ft.isOptional && ft.isInstanceOf[ListType] =>
            if l.count >= 0 then Right(())
            else Left(DLCompileError(ctx.posStr, s"limit() requires a non-negative integer, got ${l.count}"))
          case _ => Left(DLCompileError(ctx.posStr, "limit() requires a List or Option[List] receiver"))
      case _ => Right(())


// --------------------------------------------------
// reverse()
// --------------------------------------------------
object CReverseFn extends CompileFn[ReverseFn]:
  val name = "reverse"
  val minArgs = 0

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(ft: ListType) => true
      case _ => false

  override def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(listType: ListType) =>
        ListType("", listType.elementType, listType.typeName, listType.isOptional)
      case _ =>
        ScalarType("", "scala.Any")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, "reverse() takes no arguments"))
    else Right(ReverseFn(recv.fn, ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case r: ReverseFn =>
        Utility.rhsType(r.recv) match
          case Some(_: ListType) => Right(())
          case Some(ft: FieldType) if ft.isOptional && ft.isInstanceOf[ListType] => Right(())
          case Some(ft) => Left(DLCompileError(ctx.posStr, s"reverse() requires a List receiver, got ${ft.typeName}"))
          case None => Left(DLCompileError(ctx.posStr, "reverse() cannot determine receiver type"))
      case _ => Right(())


// --------------------------------------------------
// clean()
// --------------------------------------------------
object CCleanFn extends CompileFn[CleanFn]:
  val name = "clean"
  val minArgs = 0

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(ft: ListType) => true
      case _ => false

  override def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(listType: ListType) if listType.elementType.isOptional =>
        val elem = listType.elementType
        val cleanedElem = elem match
          case ft: FieldType if ft.isOptional => ft match
            case lt: ListType => lt
            case mt: MapType => mt
            case st: ScalarType => st
            case ct: ClassType => ct
            case _ => ScalarType("", "scala.Any")
          case _ => elem
        ListType("", cleanedElem, listType.typeName, listType.isOptional)
      case Some(ft) => ft
      case None => ScalarType("", "scala.Any")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, "clean() takes no arguments"))
    else Right(CleanFn(recv.fn, ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case c: CleanFn =>
        Utility.rhsType(c.recv) match
          case Some(_: ListType) => Right(())
          case Some(ft: FieldType) if ft.isOptional && ft.isInstanceOf[ListType] => Right(())
          case Some(ft) => Left(DLCompileError(ctx.posStr, s"clean() requires a List[_] receiver, got ${ft.typeName}"))
          case None => Left(DLCompileError(ctx.posStr, "clean() cannot determine receiver type"))
      case _ => Right(())


// --------------------------------------------------
// len()
// --------------------------------------------------
object CLenFn extends CompileFn[LenFn]:
  val name = "len"
  val minArgs = 0

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    def acceptsField(ft: FieldType): Boolean = ft match
      case ScalarType(_, "java.lang.String", _) => true
      case lt: ListType => true
      case mt: MapType => true
      case _ => false
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(ft) => acceptsField(ft)
      case None => false

  override def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(ft: FieldType) if ft.isOptional =>
        ScalarType("", "scala.Int").copy(isOptional = true)
      case _ =>
        ScalarType("", "scala.Int")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, "len() takes no arguments"))
    else Right(LenFn(recv.fn, ctx.posStr))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case l: LenFn =>
        Utility.rhsType(l.recv) match
          case Some(r) if accepts(NamedReceiver("this", r, co.blocke.dynalens.fn.RootFn)) => Right(())
          case Some(r) => Left(DLCompileError(ctx.posStr, s"len() cannot be applied to ${r.typeName}"))
          case None => Left(DLCompileError(ctx.posStr, "len() cannot resolve receiver type"))
      case _ => Right(())


// --------------------------------------------------
// mapTo()
// --------------------------------------------------
object CMapToFn extends CompileFn[MapToFn]:
  val name = "mapTo"
  val minArgs = 1
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(_: ScalarType) | Some(ListType(_, _: ScalarType, _, _)) => true
      case _ => false

  override def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(ListType(_, _: ScalarType, coll, isOpt)) =>
        ListType("", ScalarType("", "java.lang.String"), coll, isOpt)
      case _ => ScalarType("", "java.lang.String")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption match
      case Some(ConstantFn(mapName: String)) => Right(MapToFn(mapName, recv.fn, ctx.posStr))
      case Some(other) => Left(DLCompileError(ctx.posStr, s"mapTo() expects a string constant map name, got ${other.getClass.getSimpleName}"))
      case None => Left(DLCompileError(ctx.posStr, "mapTo() requires a map name argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case m: MapToFn =>
        Utility.rhsType(m.recv)(using ctx) match
          case Some(_: ScalarType) | Some(ListType(_, _: ScalarType, _, _)) => Right(())
          case Some(recvType) => Left(DLCompileError(ctx.posStr, s"mapTo() requires a scalar or list of scalars, got ${recvType.typeName}"))
          case None => Left(DLCompileError(ctx.posStr, "mapTo() cannot determine receiver type"))
      case _ => Right(())


// --------------------------------------------------
// mapFrom()
// --------------------------------------------------
object CMapFromFn extends CompileFn[MapFromFn]:
  val name = "mapFrom"
  val minArgs = 1
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(_: ScalarType) | Some(ListType(_, _: ScalarType, _, _)) => true
      case _ => false

  override def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(ListType(_, _: ScalarType, coll, isOpt)) =>
        ListType("", ScalarType("", "java.lang.String"), coll, isOpt)
      case _ => ScalarType("", "java.lang.String")

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption match
      case Some(ConstantFn(mapName: String)) => Right(MapFromFn(mapName, recv.fn, ctx.posStr))
      case Some(other) => Left(DLCompileError(ctx.posStr, s"mapFrom() expects a string constant map name, got ${other.getClass.getSimpleName}"))
      case None => Left(DLCompileError(ctx.posStr, "mapFrom() requires a map name argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case m: MapFromFn =>
        Utility.rhsType(m.recv)(using ctx) match
          case Some(_: ScalarType) | Some(ListType(_, _: ScalarType, _, _)) => Right(())
          case Some(recvType) =>
            Left(DLCompileError(ctx.posStr, s"mapFrom() requires a scalar or list of scalars, got ${recvType.typeName}"))
          case None =>
            Left(DLCompileError(ctx.posStr, "mapFrom() cannot determine receiver type"))
      case _ => Right(())


// --------------------------------------------------
// => (map)
// --------------------------------------------------
object CMapFn extends CompileFn[MapFn]:
  val name = "map"
  val minArgs = 1

  override def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case Some(_: ListType) | Some(_: MapType) => true
      case _ => false

  override def build(receiver: Receiver, args: List[Fn[Any]])(using ctx: ExprContext)
  : Either[DLCompileError, MapFn] =
    args match
      case transform :: Nil => Right(MapFn(receiver.fn, transform, ctx.posStr))
      case _ => Left(DLCompileError(ctx.posStr, "map() requires exactly one transform argument"))

  override def resultType(receiver: Receiver, args: List[FieldType])(using ctx: ExprContext): FieldType =
    (Utility.rhsType(receiver.fn)(using ctx), args.headOption) match
      // --- Map receiver ---
      case (Some(mapType: MapType), Some(c: ClassType)) if c.typeName == "scala.Tuple2" && c.fields.size == 2 =>
        val leftT  = c.fields.head
        val rightT = c.fields(1)
        MapType("", leftT, rightT, mapType.typeName, mapType.isOptional)

      case (Some(mapType: MapType), Some(elem)) =>
        ListType("", elem, "scala.collection.immutable.List", mapType.isOptional)

      // --- List receiver ---
      case (Some(listType: ListType), Some(elem)) =>
        ListType("", elem, listType.typeName, listType.isOptional)

      // --- Fallback ---
      case _ =>
        ScalarType("", "scala.Any")

  override def validate(fn: Fn[?])(using ctx: ExprContext) = Right(())