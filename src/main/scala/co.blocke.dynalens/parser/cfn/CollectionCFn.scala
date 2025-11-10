package co.blocke.dynalens
package parser
package cfn

import co.blocke.dynalens.fn.*
import co.blocke.dynalens.fn.IterThisFn


object CConsFn extends CompileFn:
  val name = "::"
  val minArgs = 2

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case TypeResult.Known(ft: ListType) if !ft.isOptional => true
      case TypeResult.Known(ft: ListType) if ft.isOptional => true
      case _ => false

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    (args.headOption, args.lift(1)) match
      case (Some(left), Some(right)) =>
        Utility.rhsType(right)(using ctx) match
          case TypeResult.Known(_: ListType) =>
            Right(ConsFn(left, right, ctx.posStr).asInstanceOf[Fn[Any]])
          case TypeResult.Known(_: ScalarType) =>
            Right(ConsFn(left, ConstantFn(List(right)), ctx.posStr).asInstanceOf[Fn[Any]])
          case _ =>
            Right(ConsFn(left, right, ctx.posStr).asInstanceOf[Fn[Any]])
      case _ =>
        Left(DLCompileError(ctx.posStr, s"$name missing right arg"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case c: ConsFn =>
        (Utility.rhsType(c.recv)(using ctx), Utility.rhsType(c.arg)(using ctx)) match
          case (TypeResult.Known(elem: ScalarType), TypeResult.Known(ListType(_, e, _, _))) =>
            if e.typeName == "scala.Any" || e.typeName == elem.typeName then Right(())
            else Left(DLCompileError(ctx.posStr, s":: requires matching element/list types, found ${elem.typeName} and List[${e.typeName}]"))
          case (TypeResult.Known(ListType(_, e1, _, _)), TypeResult.Known(ListType(_, e2, _, _))) =>
            if e1.typeName == "scala.Any" || e2.typeName == "scala.Any" || e1.typeName == e2.typeName then Right(())
            else Left(DLCompileError(ctx.posStr, s":: requires lists of same element type, found List[${e1.typeName}] and List[${e2.typeName}]"))
          case (TypeResult.Known(_: ScalarType), TypeResult.Known(_: ScalarType)) =>
            Right(())
          case (TypeResult.Known(lt), TypeResult.Known(rt)) =>
            Left(DLCompileError(ctx.posStr, s":: requires element :: list or list :: list, found ${lt.typeName} and ${rt.typeName}"))
          case (TypeResult.Error(e), _) => Left(e)
          case (_, TypeResult.Error(e)) => Left(e)
          case _ =>
            Left(DLCompileError(ctx.posStr, ":: cannot determine operand types"))
      case _ => Right(())


// --------------------------------------------------
// keys()
// --------------------------------------------------
object CKeysFn extends CompileFn:
  val name = "keys"
  val minArgs = 0

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case TypeResult.Known(ft: MapType) => true
      case TypeResult.Known(ft: FieldType) if ft.isOptional && ft.isInstanceOf[MapType] => true
      case _ => false

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, "keys() takes no arguments"))
    else Right(KeysFn(recv.fn, ctx.posStr).asInstanceOf[Fn[Any]])

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case k: KeysFn =>
        Utility.rhsType(k.recv)(using ctx) match
          case TypeResult.Known(_: MapType) => Right(())
          case TypeResult.Known(ft: FieldType) if ft.isOptional && ft.isInstanceOf[MapType] => Right(())
          case _ => Left(DLCompileError(ctx.posStr, "keys() requires a Map or Option[Map] receiver"))
      case _ => Right(())


// --------------------------------------------------
// values()
// --------------------------------------------------
object CValuesFn extends CompileFn:
  val name = "values"
  val minArgs = 0

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case TypeResult.Known(ft: MapType) => true
      case TypeResult.Known(ft: FieldType) if ft.isOptional && ft.isInstanceOf[MapType] => true
      case _ => false

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, "values() takes no arguments"))
    else Right(ValuesFn(recv.fn, ctx.posStr).asInstanceOf[Fn[Any]])

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case v: ValuesFn =>
        Utility.rhsType(v.recv)(using ctx) match
          case TypeResult.Known(_: MapType) => Right(())
          case TypeResult.Known(ft: FieldType) if ft.isOptional && ft.isInstanceOf[MapType] => Right(())
          case _ => Left(DLCompileError(ctx.posStr, "values() requires a Map or Option[Map] receiver"))
      case _ => Right(())


// --------------------------------------------------
// filter()
// --------------------------------------------------
object CFilterFn extends CompileFn:
  val name = "filter"
  val minArgs = 1
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case TypeResult.Known(ft: ListType) => true
      case _ => false

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption match
      case None =>
        Left(DLCompileError(ctx.posStr, "filter() requires a predicate"))

      case Some(pred) =>
        // Get element type from the receiver list
        val elemType = Utility.rhsType(recv.fn)(using ctx) match
          case TypeResult.Known(listT: ListType) => listT.elementType
          case TypeResult.Known(ft: FieldType) if ft.isOptional && ft.isInstanceOf[ListType] =>
            ft.asInstanceOf[ListType].elementType
          case _ => ScalarType("", "scala.Any", false)
        val elemFields = elemType match
          case c: ClassType => c.fields
          case _ => Nil

        // Insert IterThisFn for 'this' or receiver field in predicate (recursive)
        println("--z-- BEFORE"+pred)
        def replaceIterThis(fn: Fn[Any], depth: Int = 0): Fn[Any] = {
          println("[TRACE] " + "  " * depth + s"Visiting ${fn.getClass.getSimpleName}: $fn")
          fn match
            case g: GetFn if g.path == "this" =>
              println("[TRACE] " + "  " * depth + s"Matched GetFn('this'): replacing with IterThisFn")
              println("[TRACE] " + "  " * depth + s"Returning: $IterThisFn")
              IterThisFn
            case f =>
              // Check and maybe replace recv
              val recvReplaced = f.recv match
                case fn1: GetFn if fn1.path == "this" =>
                  println("[TRACE] " + "  " * (depth+1) + s"Matched recv GetFn('this') in ${f.getClass.getSimpleName}: replacing with IterThisFn")
                  f.replaceRecv(IterThisFn)
                case recvFn =>
                  // Only recurse if not null and not already IterThisFn
                  if recvFn != null && recvFn != f && recvFn != IterThisFn then
                    println("[TRACE] " + "  " * (depth+1) + s"Recursing into recv of ${f.getClass.getSimpleName}")
                  f
              // Scan arguments too
              val result = f.args.zipWithIndex.foldLeft(recvReplaced) { case (currFn, (a, i)) =>
                val patched = a match
                  case fn1: GetFn if fn1.path == "this" =>
                    println("[TRACE] " + "  " * (depth+1) + s"Matched arg[$i] GetFn('this') in ${f.getClass.getSimpleName}: replacing with IterThisFn")
                    IterThisFn
                  case _ =>
                    println("[TRACE] " + "  " * (depth+1) + s"Recursing into arg[$i] of ${f.getClass.getSimpleName}")
                    replaceIterThis(a, depth + 2)
                val replaced = currFn.replaceArg(i, patched)
                if (patched ne a)
                  println("[TRACE] " + "  " * (depth+1) + s"Replaced arg[$i] in ${f.getClass.getSimpleName}")
                replaced
              }
              println("[TRACE] " + "  " * depth + s"Returning: $result")
              result
        }

        val fixedPred = replaceIterThis(pred)
        println("------> Fixed: " + fixedPred)

        // Validate predicate returns Boolean
        Utility.rhsType(fixedPred)(using ctx.withReceiver(NamedReceiver("this", elemType, recv.fn)).pushScope(elemFields)) match
          case TypeResult.Known(ft) if ft.typeName == "scala.Boolean" =>
            Right(FilterFn(recv.fn, fixedPred, ctx.posStr).asInstanceOf[Fn[Any]])
          case TypeResult.Known(ft) =>
            Left(DLCompileError(ctx.posStr, s"filter() requires boolean predicate, got ${ft.typeName}"))
          case TypeResult.Error(e) => Left(e)
          case _ =>
            Left(DLCompileError(ctx.posStr, "filter() cannot resolve predicate type"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case f: FilterFn =>
        Utility.rhsType(f.arg)(using ctx) match
          case TypeResult.Known(ft) if ft.typeName == "scala.Boolean" => Right(())
          case TypeResult.Known(ft) => Left(DLCompileError(ctx.posStr, s"filter() requires boolean predicate, got ${ft.typeName}"))
          case TypeResult.Error(e) => Left(e)
          case _ => Left(DLCompileError(ctx.posStr, "filter() cannot resolve predicate type"))
      case _ => Right(())


// --------------------------------------------------
// sortAsc()
// --------------------------------------------------
object CSortAscFn extends CompileFn:
  val name = "sortAsc"
  val minArgs = 0
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case TypeResult.Known(ft: ListType) => true
      case _ => false

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args match
      case Nil => Right(SortAscFn(recv.fn, None, ctx.posStr).asInstanceOf[Fn[Any]])
      case List(a1: GetFn) => Right(SortAscFn(recv.fn, Some(a1.path), ctx.posStr).asInstanceOf[Fn[Any]])
      case List(_) => Left(DLCompileError(ctx.posStr, "sortAsc() argument must be a field path"))
      case _ => Left(DLCompileError(ctx.posStr, s"sortAsc() expected 0 or 1 arg, got ${args.length}"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case s: SortAscFn =>
        Utility.rhsType(s.recv) match
          case TypeResult.Known(ListType(_, elem, _, _)) if elem.isNumeric || elem.isStringLike || elem.isInstanceOf[ClassType] => Right(())
          case TypeResult.Known(ListType(_, elem, _, _)) =>
            Left(DLCompileError(ctx.posStr, s"sortAsc() requires sortable element type, got ${elem.typeName}"))
          case TypeResult.Known(ft: FieldType) if ft.isOptional && ft.isInstanceOf[ListType] =>
            val inner = ft.asInstanceOf[ListType]
            if inner.elementType.isNumeric || inner.elementType.isStringLike || inner.elementType.isInstanceOf[ClassType] then Right(())
            else Left(DLCompileError(ctx.posStr, s"sortAsc() requires sortable element type, got ${inner.elementType.typeName}"))
          case _ => Left(DLCompileError(ctx.posStr, "sortAsc() requires a List or Option[List] receiver"))
      case _ => Right(())


// --------------------------------------------------
// sortDesc()
// --------------------------------------------------
object CSortDescFn extends CompileFn:
  val name = "sortDesc"
  val minArgs = 0
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case TypeResult.Known(ft: ListType) => true
      case _ => false

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args match
      case Nil => Right(SortDescFn(recv.fn, None, ctx.posStr).asInstanceOf[Fn[Any]])
      case List(a1: GetFn) => Right(SortDescFn(recv.fn, Some(a1.path), ctx.posStr).asInstanceOf[Fn[Any]])
      case List(_) => Left(DLCompileError(ctx.posStr, "sortDesc() argument must be a field path"))
      case _ => Left(DLCompileError(ctx.posStr, s"sortDesc() expected 0 or 1 arg, got ${args.length}"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case s: SortDescFn =>
        Utility.rhsType(s.recv) match
          case TypeResult.Known(ListType(_, elem, _, _)) if elem.isNumeric || elem.isStringLike || elem.isInstanceOf[ClassType] => Right(())
          case TypeResult.Known(ListType(_, elem, _, _)) =>
            Left(DLCompileError(ctx.posStr, s"sortDesc() requires sortable element type, got ${elem.typeName}"))
          case TypeResult.Known(ft: FieldType) if ft.isOptional && ft.isInstanceOf[ListType] =>
            val inner = ft.asInstanceOf[ListType]
            if inner.elementType.isNumeric || inner.elementType.isStringLike || inner.elementType.isInstanceOf[ClassType] then Right(())
            else Left(DLCompileError(ctx.posStr, s"sortDesc() requires sortable element type, got ${inner.elementType.typeName}"))
          case _ => Left(DLCompileError(ctx.posStr, "sortDesc() requires a List or Option[List] receiver"))
      case _ => Right(())


// --------------------------------------------------
// distinct()
// --------------------------------------------------
object CDistinctFn extends CompileFn:
  val name = "distinct"
  val minArgs = 0

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case TypeResult.Known(ft: ListType) => true
      case _ => false

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args match
      case Nil => Right(DistinctFn(recv.fn, None, ctx.posStr).asInstanceOf[Fn[Any]])
      case List(ConstantFn(s: String)) => Right(DistinctFn(recv.fn, Some(s), ctx.posStr).asInstanceOf[Fn[Any]])
      case List(GetFn(name, _, _,_, _)) => Right(DistinctFn(recv.fn, Some(name), ctx.posStr).asInstanceOf[Fn[Any]])
      case _ => Left(DLCompileError(ctx.posStr, "distinct() takes zero or one constant string argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case d: DistinctFn =>
        Utility.rhsType(d.recv) match
          case TypeResult.Known(_: ListType) => Right(())
          case TypeResult.Known(ft: FieldType) if ft.isOptional && ft.isInstanceOf[ListType] => Right(())
          case _ => Left(DLCompileError(ctx.posStr, "distinct() requires a List or Option[List] receiver"))
      case _ => Right(())


// --------------------------------------------------
// limit()
// --------------------------------------------------
object CLimitFn extends CompileFn:
  val name = "limit"
  val minArgs = 1
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case TypeResult.Known(ft: ListType) => true
      case _ => false

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args match
      case List(ConstantFn(i: Int))  if i >= 0 => Right(LimitFn(recv.fn, i, ctx.posStr).asInstanceOf[Fn[Any]])
      case List(ConstantFn(l: Long)) if l >= 0 => Right(LimitFn(recv.fn, l.toInt, ctx.posStr).asInstanceOf[Fn[Any]])
      case List(ConstantFn(bad: Int)) => Left(DLCompileError(ctx.posStr, s"limit() requires non-negative integer, got $bad"))
      case Nil => Left(DLCompileError(ctx.posStr, "limit() requires a non-negative integer argument"))
      case _ => Left(DLCompileError(ctx.posStr, "limit() only accepts a non-negative integer constant argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case l: LimitFn =>
        Utility.rhsType(l.recv) match
          case TypeResult.Known(_: ListType) =>
            if l.count >= 0 then Right(())
            else Left(DLCompileError(ctx.posStr, s"limit() requires a non-negative integer, got ${l.count}"))
          case TypeResult.Known(ft: FieldType) if ft.isOptional && ft.isInstanceOf[ListType] =>
            if l.count >= 0 then Right(())
            else Left(DLCompileError(ctx.posStr, s"limit() requires a non-negative integer, got ${l.count}"))
          case _ => Left(DLCompileError(ctx.posStr, "limit() requires a List or Option[List] receiver"))
      case _ => Right(())


// --------------------------------------------------
// reverse()
// --------------------------------------------------
object CReverseFn extends CompileFn:
  val name = "reverse"
  val minArgs = 0

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case TypeResult.Known(ft: ListType) => true
      case _ => false

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, "reverse() takes no arguments"))
    else Right(ReverseFn(recv.fn, ctx.posStr).asInstanceOf[Fn[Any]])

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case r: ReverseFn =>
        Utility.rhsType(r.recv) match
          case TypeResult.Known(_: ListType) => Right(())
          case TypeResult.Known(ft: FieldType) if ft.isOptional && ft.isInstanceOf[ListType] => Right(())
          case TypeResult.Known(ft) => Left(DLCompileError(ctx.posStr, s"reverse() requires a List receiver, got ${ft.typeName}"))
          case TypeResult.Error(e) => Left(e)
          case _ => Left(DLCompileError(ctx.posStr, "reverse() cannot determine receiver type"))
      case _ => Right(())


// --------------------------------------------------
// clean()
// --------------------------------------------------
object CCleanFn extends CompileFn:
  val name = "clean"
  val minArgs = 0

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case TypeResult.Known(ft: ListType) => true
      case _ => false

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, "clean() takes no arguments"))
    else
      // Instrumentation: log the raw recv.fieldType
      val recvType = Utility.rhsType(recv.fn)(using ctx)

      val resultType = recvType match
        case TypeResult.Known(listT: ListType) =>
          // Preserve the element type accurately
          ListType(listT.name, listT.elementType, listT.typeName, false)
        case TypeResult.Known(ft) =>
          ListType("", ScalarType("", "scala.Any", false), "scala.List[Any]", false)
        case _ =>
          ListType("", ScalarType("", "scala.Any", false), "scala.List[Any]", false)
      Right(CleanFn(recv.fn, resultType, ctx.posStr).asInstanceOf[Fn[Any]])

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case c: CleanFn =>
        Utility.rhsType(c.recv) match
          case TypeResult.Known(_: ListType) => Right(())
          case TypeResult.Known(ft: FieldType) if ft.isOptional && ft.isInstanceOf[ListType] => Right(())
          case TypeResult.Known(ft) => Left(DLCompileError(ctx.posStr, s"clean() requires a List[_] receiver, got ${ft.typeName}"))
          case TypeResult.Error(e) => Left(e)
          case _ => Left(DLCompileError(ctx.posStr, "clean() cannot determine receiver type"))
      case _ => Right(())


// --------------------------------------------------
// len()
// --------------------------------------------------
object CLenFn extends CompileFn:
  val name = "len"
  val minArgs = 0

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    def acceptsField(ft: FieldType): Boolean = ft match
      case ScalarType(_, "java.lang.String", _) => true
      case lt: ListType => true
      case mt: MapType => true
      case _ => false
    Utility.rhsType(receiver.fn)(using ctx) match
      case TypeResult.Known(ft) => acceptsField(ft)
      case _ => false

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    if args.nonEmpty then Left(DLCompileError(ctx.posStr, "len() takes no arguments"))
    else Right(LenFn(recv.fn, ctx.posStr).asInstanceOf[Fn[Any]])

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case l: LenFn =>
        Utility.rhsType(l.recv) match
          case TypeResult.Known(r) if accepts(NamedReceiver("this", r, co.blocke.dynalens.fn.RootFn)) => Right(())
          case TypeResult.Known(r) => Left(DLCompileError(ctx.posStr, s"len() cannot be applied to ${r.typeName}"))
          case TypeResult.Error(e) => Left(e)
          case _ => Left(DLCompileError(ctx.posStr, "len() cannot resolve receiver type"))
      case _ => Right(())


/*
// --------------------------------------------------
// mapTo()
// --------------------------------------------------
object CMapToFn extends CompileFn:
  val name = "mapTo"
  val minArgs = 1
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case TypeResult.Known(_: ScalarType) | TypeResult.Known(ListType(_, _: ScalarType, _, _)) => true
      case _ => false

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption match
      case Some(ConstantFn(mapName: String)) => Right(MapToFn(mapName, recv.fn, ctx.posStr))
      case Some(other) => Left(DLCompileError(ctx.posStr, s"mapTo() expects a string constant map name, got ${other.getClass.getSimpleName}"))
      case None => Left(DLCompileError(ctx.posStr, "mapTo() requires a map name argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case m: MapToFn =>
        Utility.rhsType(m.recv)(using ctx) match
          case TypeResult.Known(_: ScalarType) | TypeResult.Known(ListType(_, _: ScalarType, _, _)) => Right(())
          case TypeResult.Known(recvType) => Left(DLCompileError(ctx.posStr, s"mapTo() requires a scalar or list of scalars, got ${recvType.typeName}"))
          case TypeResult.Error(e) => Left(e)
          case _ => Left(DLCompileError(ctx.posStr, "mapTo() cannot determine receiver type"))
      case _ => Right(())


// --------------------------------------------------
// mapFrom()
// --------------------------------------------------
object CMapFromFn extends CompileFn:
  val name = "mapFrom"
  val minArgs = 1
  override val maxArgs = 1

  def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case TypeResult.Known(_: ScalarType) | TypeResult.Known(ListType(_, _: ScalarType, _, _)) => true
      case _ => false

  def build(recv: Receiver, args: List[Fn[Any]])(using ctx: ExprContext) =
    args.headOption match
      case Some(ConstantFn(mapName: String)) => Right(MapFromFn(mapName, recv.fn, ctx.posStr))
      case Some(other) => Left(DLCompileError(ctx.posStr, s"mapFrom() expects a string constant map name, got ${other.getClass.getSimpleName}"))
      case None => Left(DLCompileError(ctx.posStr, "mapFrom() requires a map name argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) =
    fn match
      case m: MapFromFn =>
        Utility.rhsType(m.recv)(using ctx) match
          case TypeResult.Known(_: ScalarType) | TypeResult.Known(ListType(_, _: ScalarType, _, _)) => Right(())
          case TypeResult.Known(recvType) => Left(DLCompileError(ctx.posStr, s"mapFrom() requires a scalar or list of scalars, got ${recvType.typeName}"))
          case TypeResult.Error(e) => Left(e)
          case _ => Left(DLCompileError(ctx.posStr, "mapFrom() cannot determine receiver type"))
      case _ => Right(())
      */


// --------------------------------------------------
// => (map)
// --------------------------------------------------
object CMapFn extends CompileFn:
  val name = "map"
  val minArgs = 1

  override def accepts(receiver: Receiver)(using ctx: ExprContext): Boolean =
    Utility.rhsType(receiver.fn)(using ctx) match
      case TypeResult.Known(_: ListType) | TypeResult.Known(_: MapType) => true
      case _ => false

  override def build(receiver: Receiver, args: List[Fn[Any]])(using ctx: ExprContext)
  : Either[DLCompileError, MapFn] =
    args match
      case transform :: Nil => Right(MapFn(receiver.fn, transform, ctx.posStr))
      case _ => Left(DLCompileError(ctx.posStr, "map() requires exactly one transform argument"))

  override def validate(fn: Fn[?])(using ctx: ExprContext) = Right(())