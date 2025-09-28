/*
 * Copyright (c) 2025 Greg Zoller
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package co.blocke.dynalens
package parser

import Path.*

import scala.annotation.tailrec

object Utility:

  def getPathType(path: String)(using ctx: ExprContext): FieldType = {
    val segments = path.split("\\.").toList
    if segments.isEmpty then return ScalarType("", "scala.Any")

    //  1 check user-defined vals first (top of symbol stack wins)
    ctx.symbols.collectFirst { case m if m.contains(segments.head) => m(segments.head) } match {
      case Some(v: ValType) =>
        // if there's more path after a val, walk inside its valueType
        val base = v.valueType
        return if segments.tail.nonEmpty then descend(base, segments.tail).getOrElse(ScalarType("", "scala.Any"))
        else base
      case Some(ft) =>
        return if segments.tail.nonEmpty then descend(ft, segments.tail).getOrElse(ScalarType("", "scala.Any"))
        else ft
      case None => // keep going
    }

    // 2 fall back to schema-based resolution
    @tailrec
    def stepIntoForHead(ft: FieldType, headSeg: String): FieldType = {
      val hasIndex = headSeg.matches(""".*\[\d+\]""")
      ft match
        case o: OptionType => stepIntoForHead(o.valueType, headSeg)
        case l: ListType if hasIndex => l.elementType
        case other => other
    }

    segments match
      case head :: tail =>
        val headNorm = head.replaceAll("\\[\\d+\\]", "")
        ctx.schema.fields.find(_.name == headNorm) match
          case None => ScalarType("", "scala.Any")
          case Some(ft0) =>
            val ft1 = stepIntoForHead(ft0, head)
            descend(ft1, tail).getOrElse(ScalarType("", "scala.Any"))
  }


  /**
   * Recursively resolve a dot path into its FieldType.
   * Normalizes indexed segments like "items[3]" → "items".
   * Prints detailed debug at every step.
   */
  // Put this once (top-level in Utility or wherever you keep helpers)
  private def descend(ft: FieldType, rest: List[String]): Option[FieldType] =
    rest match
      case Nil =>
        // done: return whatever type we've landed on
        Some(ft)

      case seg :: tail =>
        // strip any [123] index sugar off this segment
        val norm = seg.replaceAll("\\[\\d+\\]", "")

        ft match
          case c: ClassType =>
            // consume this segment by finding the matching field on the class
            c.fields.find(_.name == norm).flatMap(descend(_, tail))

          case o: OptionType =>
            // unwrap Option to continue, but DO NOT consume the segment here
            descend(o.valueType, rest)

          case l: ListType =>
            // step into element type, but DO NOT consume the segment here.
            // We still need to match `norm` against the element’s fields next.
            descend(l.elementType, rest)

          case _ =>
            None

  def rhsType(fn: Fn[?])(using ctx: ExprContext): Option[FieldType] =

    // All these to handle figuring out rhs type of arithmetic functions, which can have different argument types (all numeric, tho)
    def normNum(tn: String): Option[String] = tn match
      case "scala.Int" | "java.lang.Integer" => Some("scala.Int")
      case "scala.Long" | "java.lang.Long" => Some("scala.Long")
      case "scala.Float" | "java.lang.Float" => Some("scala.Float")
      case "scala.Double" | "java.lang.Double" => Some("scala.Double")
      case _ => None

    val rank = Map("scala.Int" -> 1, "scala.Long" -> 2, "scala.Float" -> 3, "scala.Double" -> 4)

    def promote(a: String, b: String, atLeastDouble: Boolean): String =
      if atLeastDouble then "scala.Double"
      else if rank(a) >= rank(b) then a else b

    def numericResultType(l: Fn[?], r: Fn[?], atLeastDouble: Boolean): Option[FieldType] =
      for
        lt <- rhsType(l)
        rt <- rhsType(r)
        ln <- normNum(lt.typeName)
        rn <- normNum(rt.typeName)
        res = promote(ln, rn, atLeastDouble)
      yield ScalarType("", res)

    fn match

      // special-cases where we must inspect inner structure
      case IndexFn(inner, idx) =>
        rhsType(inner).flatMap {
          case l: ListType => Some(l.elementType)
          case o: OptionType if o.valueType.isInstanceOf[ListType] =>
            Some(o.valueType.asInstanceOf[ListType].elementType)
          case _ => None
        }

      case IdentityFn(real) =>
        println(s"[rhsType] unwrapping IdentityFn to $real")
        rhsType(real)

      case f: GetFn =>
        val cleanPath =
          f.path.replaceFirst("^_\\.", "")   // keep indices

        println(s"[DEBUG rhsType] cleanPath=$cleanPath, f.recv=${f.recv}")

        // --- paths that explicitly use `this` ---
        if cleanPath == "this" || cleanPath.startsWith("this.") then
          val base: Option[FieldType] =
            ctx.receiver.map(_.fieldType)
              .orElse(f.recv.flatMap(Utility.rhsType)) // fallback to attached recv if present

          if cleanPath == "this" then
            base.flatMap {
              case ListType(_, elem, _)                         => Some(elem)
              case OptionType(_, ListType(_, elem, _), _)       => Some(elem)
              case other                                        => Some(other)
            }
          else {
            val segs = cleanPath.stripPrefix("this.").split("\\.").toList
            base.flatMap(descend(_, segs))
          }

        // --- NEW: path without "this" but a receiver is attached -> resolve inside receiver ---
        else if f.recv.nonEmpty then
          // Prefer compile-time receiver from context (set by Filter/collection scope),
          // otherwise try to infer from the attached recv fn.
          val base0: Option[FieldType] =
            ctx.receiver.map(_.fieldType)
              .orElse(f.recv.flatMap(Utility.rhsType))

          // Unwrap Option/List so we can resolve a field like "qty" on element types.
          def unwrapToElement(ft: FieldType): FieldType = ft match
            case OptionType(_, inner, _)         => unwrapToElement(inner)
            case ListType(_, elem, _)            => unwrapToElement(elem)
            case other                           => other

          base0
            .map(unwrapToElement)
            .flatMap { baseElemType =>
              // allow dotted relative paths too (e.g., "addr.zip")
              val segs = cleanPath.split("\\.").toList
              descend(baseElemType, segs)
            }

        // --- no receiver involved: symbols-first, then schema walk ---
        else {
          val symHit: Option[FieldType] =
            ctx.symbols.collectFirst { case scope if scope.contains(cleanPath) => scope(cleanPath) }

          symHit match
            case Some(vt: ValType) =>
              println(s"[rhsType] symbol hit (ValType): $vt")
              Some(vt)

            case Some(ft) =>
              println(s"[rhsType] symbol hit: $ft")
              Some(ft)

            case None =>
              println(s"[rhsType] no symbol hit, falling back to schema walk for $cleanPath")
              rhsTypeFromSchemaPath(cleanPath, ctx.schema)
        }

      case f: ElseFn => rhsType(f.fallback)
      case f: IfFn[?] => rhsType(f.thenFn)
      case f: BlockFn[?] => rhsType(f.finalFn)

      case MapGetFn(recv, _) => recv match
        case GetFn(p, _, _) => Utility.mapGetValueType(p)
        case _ => None

      case CaseWhenFn(_, cases, default, _) =>
        val ts: List[FieldType] =
          (cases.iterator.flatMap { case (_, fn) => rhsType(fn) } ++ default.iterator.flatMap(rhsType)).toList
        ts.distinct match
          case single :: Nil => Some(single)
          case _             => None

      case b: BooleanConstantFn =>
        Some(ScalarType("", "scala.Boolean"))

      case c: ConstantFn[?] =>
        // If ConstantFn already carries a FieldType, prefer it; otherwise derive from value
        c match
          case _ =>
            c.out match
              case _: Int     => Some(ScalarType("", "scala.Int"))
              case _: Long    => Some(ScalarType("", "scala.Long"))
              case _: Float   => Some(ScalarType("", "scala.Float"))
              case _: Double  => Some(ScalarType("", "scala.Double"))
              case _: Boolean => Some(ScalarType("", "scala.Boolean"))
              case _: String  => Some(ScalarType("", "java.lang.String"))
              case _          => Some(ScalarType("", "scala.Any"))

      case v: ValType =>
        println(s"[rhsType valType] $v")
        Some(v.valueType)

      case AddFn(l, r)      => numericResultType(l, r, atLeastDouble = false)
      case SubtractFn(l, r) => numericResultType(l, r, atLeastDouble = false)
      case MultiplyFn(l, r) => numericResultType(l, r, atLeastDouble = false)
      case DivideFn(l, r)   => numericResultType(l, r, atLeastDouble = true)

      // generic fallback: look up MethodSig
      case other =>
        // --- DEBUG instrumentation ---
        val recvDebug =
          other.recv
            .flatMap(rhsType)
            .map(ft => s"${ft.getClass.getSimpleName}(${ft.typeName})")
            .getOrElse("None")

        MethodSig.lookup(other.methodName).flatMap { sig =>
          val recvType: FieldType =
            other.recv.flatMap(rhsType).getOrElse(ScalarType("", "java.lang.String"))

          if sig.accepts(recvType) then Some(sig.result(recvType))
          else None
        }

  /** For a list or map at `basePath`, return the FieldType of the element/value.
   * - For `List` or `Option[List]` → its element type.
   * - For `Map` or `Option[Map]`   → its value type.
   * - For plain classes → just return their own fields as a synthetic ClassType.
   * - For scalars → None.
   */
  def elementSchemaFor(basePath: String, schema: ClassType): Option[FieldType] =
    Schema.resolvePath(schema, basePath).map(_.fieldType) match
      case Some(ListType(_, elem, _))                 => Some(elem)
      case Some(OptionType(_, inner: ListType, _))    => Some(inner.elementType)
      case Some(MapType(_, _, valueType, _))          => Some(valueType)
      case Some(OptionType(_, inner: MapType, _))     => Some(inner.valueType)
      case Some(ct: ClassType)                        => Some(ct)          // whole class itself
      case Some(OptionType(_, inner: ClassType, _))   => Some(inner)
      case _                                          => None

  // Small parser for "seg" -> (nameWithoutIndex, hasIndex)

  /** Split a segment like "items[3]" into ("items", Some(3)). */

  /** Split a segment like "items[3]" into ("items", Some(3)). */
  private def splitIndex(seg: String): (String, Option[Int]) =
    val m = """^([^\[]+)(?:\[(\d+)\])?$""".r
    seg match
      case m(base, idx) => (base, Option(idx).map(_.toInt))
      case _ => (seg, None)

  /** Determine FieldType for a dotted path that may contain [index] segments.
   * Handles items[1].num by drilling through the list element type.
   */
  def rhsTypeFromSchemaPath(path: String, schema: ClassType): Option[FieldType] = {
    println(s"[rhsTypeFromSchemaPath] start: path=$path  schema=${schema.name}")

    def walk(ft: FieldType, segs: List[String]): Option[FieldType] = {
      println(s"[rhsTypeFromSchemaPath] walk: ft=${ft.name}:${ft.getClass.getSimpleName}, segs=$segs")

      segs match
        case Nil =>
          println(s"[rhsTypeFromSchemaPath] ✓ found final type: $ft")
          Some(ft)

        case rawHead :: tail =>
          // Parse the current segment for an optional index (e.g. items[1])
          val m = """^([^\[]+)(?:\[(\d+)\])?$""".r
          val (name, idxOpt) = rawHead match
            case m(base, idx) => (base, Option(idx))
            case _            => (rawHead, None)
          println(s"[rhsTypeFromSchemaPath]   head=$rawHead name=$name idx=$idxOpt")

          ft match
            case o: OptionType =>
              println(s"[rhsTypeFromSchemaPath]   unwrapping OptionType")
              // unwrap Option but **keep current segment list**
              walk(o.valueType, segs)

            case l: ListType if idxOpt.nonEmpty || (tail.nonEmpty && l.elementType.isInstanceOf[ClassType]) =>
              // We either have an explicit index OR we're implicitly accessing a field inside each element.
              println(s"[rhsTypeFromSchemaPath]   ListType unwrap -> elemType=${l.elementType}, remaining=$tail")
              walk(l.elementType, tail)

            case l: ListType =>
              println(s"[rhsTypeFromSchemaPath]   ListType hit, idxOpt=$idxOpt, head=$name, tail=$tail")
              idxOpt match
                case Some(_) =>
                  // Explicit index like items[1] -> unwrap and continue with the remaining tail
                  walk(l.elementType, tail)
                case None =>
                  // Implicit element access (e.g., items.num):
                  // unwrap but re-check the SAME head segment (`name`) against the element type
                  // i.e., don't drop `head`; keep `segs` so `name` is matched on the element.
                  walk(l.elementType, segs)

            case c: ClassType =>
              println(s"[rhsTypeFromSchemaPath]   ClassType fields=${c.fields.map(_.name)}")
              c.fields.find(_.name == name)
                .flatMap(f => walk(f, tail))

            case other =>
              println(s"[rhsTypeFromSchemaPath]   no match (type=${other.getClass.getSimpleName}), giving up")
              None
    }

    val segList = path.split("\\.").toList
    println(s"[rhsTypeFromSchemaPath] segments = $segList")
    walk(schema, segList)
  }

  def elementTypeOf(fn: Fn[Any])(using ctx: ExprContext): FieldType =
    val r = rhsType(fn)
    // Unwrap IdentityFn if present so we get to the true list source
    val source: Fn[Any] = fn match
      case id: IdentityFn.type if id.recv.nonEmpty => id.recv.get.asInstanceOf[Fn[Any]]
      case _ => fn

    println(s"[elementTypeOf] incoming fn = $fn, fn.recv = ${fn.recv}")
    val rt = rhsType(fn)
    println(s"[elementTypeOf] rhsType($fn) = $rt")

    rhsType(source) match
      case Some(ListType(_, elem, _)) => elem
      case Some(OptionType(_, ListType(_, elem, _), _)) => elem
      case Some(ft) => ft
      case None => ScalarType("this", "scala.Any")

  //  def elementTypeOf(fn: Fn[Any])(using ctx: ExprContext): FieldType = {
//    val maybeType = rhsType(fn)
//    println(s"[elementTypeOf] rhsType($fn) = $maybeType")
//
//    maybeType match {
//      // scalar list, e.g. List[Int]
//      case Some(ListType(_, e: ScalarType, _)) =>
//        println(s"[elementTypeOf] matched ListType with element: $e")
//        ScalarType("this", e.typeName)
//
//      // optional list, e.g. Option[List[Int]]
//      case Some(OptionType(_, l: ListType, _)) =>
//        l.elementType match {
//          case s: ScalarType =>
//            println(s"[elementTypeOf] matched Option[ListType] with element: $s")
//            ScalarType("this", s.typeName)
//          case other =>
//            println(s"[elementTypeOf] Option[ListType] but element not scalar: $other")
//            other
//        }
//
//      // class list, e.g. List[Item]
//      case Some(ListType(_, c: ClassType, _)) =>
//        println(s"[elementTypeOf] matched ListType with ClassType element: $c")
//        c
//
//      // option of class list
//      case Some(OptionType(_, l: ListType, _)) if l.elementType.isInstanceOf[ClassType] =>
//        val c = l.elementType.asInstanceOf[ClassType]
//        println(s"[elementTypeOf] matched Option[ListType] with ClassType element: $c")
//        c
//
//      // anything else falls back
//      case other =>
//        println(s"[elementTypeOf] no special match, defaulting to ScalarType(this, scala.Any)")
//        ScalarType("this", "scala.Any")
//    }
//  }

  def addThisType(cleanPath: String, ctx: ExprContext): ExprContext = {
    val parts = Path.parsePath(cleanPath)

    // collect every collection segment along the path
    val loops: List[(String, String)] = {
      val buf = scala.collection.mutable.ListBuffer.empty[(String, String)]
      val path = new StringBuilder
      parts.foreach {
        case Path.Field(name, _) =>
          if path.nonEmpty then path.append('.')
          path.append(name)

        case Path.IndexedField(name, _, _) =>
          if path.nonEmpty then path.append('.')
          path.append(name)
          buf += ((path.result(), name))
      }
      buf.toList
    }

    val loopSymbols: Map[String, FieldType] =
      loops.flatMap { case (absPath, loopName) =>
        Schema.resolvePath(ctx.schema, absPath) match {
          case Some(resolved) =>
            resolved.fieldType match {
              case ListType(_, elementType, _) =>
                Some(loopName -> elementType)
              case OptionType(_, inner: ListType, _) =>
                Some(loopName -> inner.elementType)
              case _ =>
                None
            }
          case None => None
        }
      }.toMap

    val withRecv = ctx.withReceiverFromPath(cleanPath)
    if loopSymbols.nonEmpty then withRecv.copy(symbols = loopSymbols :: withRecv.symbols)
    else withRecv
  }

  /** Convert a GetFn or IndexFn back into a dotted path string, including indices. */
  def pathString(fn: Fn[Any]): String = fn match
    case g: GetFn   => g.path
    case i: IndexFn => s"${pathString(i.receiver)}[${i.index}]"
    case other      => other.toString // fallback if some unexpected Fn sneaks in

  def areTypesCompatible(lhs: FieldType, rhs: FieldType): Boolean = {

    def isNumeric(t: String): Boolean =
      t == "scala.Int" || t == "scala.Long" || t == "scala.Float" || t == "scala.Double"

    def numericCompatible(l: String, r: String): Boolean =
      isNumeric(l) && isNumeric(r)

    (lhs, rhs) match {
      // ----- identical types quickly short-circuit -----
      case _ if lhs == rhs => true

      // ----- simple scalars -----
      case (ScalarType(_, lt), ScalarType(_, rt)) =>
        numericCompatible(lt, rt) || lt == rt

      // ----- options -----
      case (OptionType(_, lElem, _), OptionType(_, rElem, _)) =>
        areTypesCompatible(lElem, rElem)
      // allow assigning plain T to Option[T]
      case (OptionType(_, lElem, _), other) =>
        areTypesCompatible(lElem, other)
      // allow assigning Option[T] to plain T
      case (other, OptionType(_, rElem, _)) =>
        areTypesCompatible(other, rElem)

      // ----- lists -----
      case (ListType(_, lElem, _), ListType(_, rElem, _)) =>
        areTypesCompatible(lElem, rElem)

      // ----- maps -----
      case (MapType(_, lk, lv, _), MapType(_, rk, rv, _)) =>
        areTypesCompatible(lk, rk) && areTypesCompatible(lv, rv)

      // ----- classes -----
      case (ClassType(_, ln, lf), ClassType(_, rn, rf)) if ln == rn =>
        // compare by field name, not tuple order
        val leftMap = lf.map(f => f.name -> f).toMap
        val rightMap = rf.map(f => f.name -> f).toMap
        rightMap.forall { case (n, rft) =>
          leftMap.get(n).exists(lft => areTypesCompatible(lft, rft))
        }

      // ----- sealed traits -----
      // same trait
      case (SealedTraitType(_, ln, _, lSubs), SealedTraitType(_, rn, _, rSubs)) if ln == rn =>
        true
      // trait on left, concrete on right
      case (SealedTraitType(_, _, _, lSubs), ClassType(_, rc, _)) =>
        lSubs.contains(rc)
      // trait on right, concrete on left
      case (ClassType(_, lc, _), SealedTraitType(_, _, _, rSubs)) =>
        rSubs.contains(lc)

      // ----- otherwise -----
      case _ => false
    }
  }


  /** Enforce that option-map (LHS is OptionType) doesn’t change its container “kind”.
   * Allowed RHS for each LHS:
   *   - Option[Scalar]  : RHS must be Scalar or another Option[Scalar]
   *   - Option[List[_]] : RHS can be anything (each element mapped individually)
   *   - Option[Map[_,_]]: RHS can be anything (each entry mapped individually)
   *
   * If we cannot infer the RHS type, we do not block compilation.
   */
  def checkRhsShapeForOptionMap(lhs: FieldType, rhs: Fn[Any], off: Int)
                               (using ctx: ExprContext): Either[DLCompileError, Unit] =
    lhs match
      // ----- Option[T] -----
      case OptionType(_, inner, _) =>
        inner match
          // ----- Option[Scalar] -----
          case s: ScalarType =>
            // Infer RHS type
            Utility.rhsType(rhs) match
              case None =>
                Left(DLCompileError(off, s"Unable to infer type of RHS for option map"))
              case Some(r: FieldType) =>
                r match
                  case ScalarType(_, _) => Right(())
                  case OptionType(_, v: ScalarType, _) => Right(())
                  case _ =>
                    Left(DLCompileError(
                      off,
                      s"Option map shape mismatch: LHS expects Scalar or Option[Scalar] but RHS is ${r.typeName}"
                    ))

          // ----- Option[List[_]] -----
          case _: ListType =>
            // Element mapping is handled inside the list itself; no shape restriction
            Right(())

          // ----- Option[Map[_,_]] -----
          case _: MapType =>
            // Similar logic: each map entry is mapped individually
            Right(())

          // ----- Option[ClassType] or other -----
          case c: ClassType =>
            // For plain Option[Class], require RHS to be same class or another Option[Class]
            Utility.rhsType(rhs) match
              case None => Left(DLCompileError(off, s"Unable to infer type of RHS for option map"))
              case Some(r: FieldType) =>
                r match
                  case ClassType(_, tName, _) if tName == c.typeName => Right(())
                  case OptionType(_, ClassType(_, tName, _), _) if tName == c.typeName => Right(())
                  case _ =>
                    Left(DLCompileError(
                      off,
                      s"Option map shape mismatch: LHS expects ${c.typeName} or Option[${c.typeName}] but RHS is ${r.typeName}"
                    ))

      // ----- Not an Option[T] -----
      case _ =>
        // Non-option LHS: no special restriction
        Right(())

  private val EndsWithIndexed = """.*\[\d+\]\??$""".r

  private def hasExplicitIndex(path: String): Boolean =
    EndsWithIndexed.pattern.matcher(path).matches()

  /** For element-wise writes like foo[2] = …, treat Option[List[T]] as plain List[T]. */
  def effectiveLhsForAssignment(lhs: FieldType, path: String): FieldType =
    if hasExplicitIndex(path) then
      lhs match
        case OptionType(_, inner: ListType, _) => inner // unwrap Option[List] when indexing into elements
        case l: ListType                        => l.elementType // bare List[...] with explicit index
        case other                               => other
    else
      // Whole-field assignment: keep the full declared type (List, Map, Option, etc.)
      lhs

  /** If `inner` is a `GetFn(path)`, return the element type when that path
   * refers to a List or Option[List].  Otherwise `None`.
   */
  private def indexResultType(inner: Fn[Any])(using ctx: ExprContext): Option[FieldType] =
    inner match
      case GetFn(path, _, _) =>
        Schema.resolvePath(ctx.schema, path).map(_.fieldType) match
          case Some(ListType(_, elemType, _)) =>
            // e.g. List[T] → T
            Some(elemType)

          case Some(OptionType(_, ListType(_, elemType, _), _)) =>
            // e.g. Option[List[T]] → Option[T]
            Some(OptionType(name = "", valueType = elemType, typeName = "scala.Option"))

          case _ =>
            // Not a list (or option of list)
            None

      case _ =>
        None

  /** Return the non-optional value FieldType of a Map at the given receiver path. */
  private def mapGetValueType(recvPath: String)(using ctx: ExprContext): Option[FieldType] =
    // Locate the map field along the given path
    Schema.resolvePath(ctx.schema, recvPath).map(_.fieldType) collect {
      // For a Map[K,V] simply return V.  If it's Option[Map[K,V]], unwrap both Option layers.
      case MapType(_, _, valueType, _) => stripOption(valueType)
      case OptionType(_, m: MapType, _) => stripOption(m.valueType)
    }

  /** Helper: if t is OptionType, return its inner value type, otherwise t itself. */
  private def stripOption(t: FieldType): FieldType = t match
    case OptionType(_, inner, _) => inner
    case other => other

  def isPathOptional(path: String, ctx: ExprContext): Boolean =
    // simple first cut: look for an OptionType in the schema
    Schema.resolvePath(ctx.schema, path).exists {
      case ResolvedType(ft, _) =>
        ft.isInstanceOf[OptionType]
    }

  /** Find the FieldType node at the end of the given path, if any. */
  private def findNodeAtEnd(path: String)(using ctx: ExprContext): Option[FieldType] =
    Schema.resolvePath(ctx.schema, path).map(_.fieldType)

  /** When inside a `map()` call, push a synthetic `this`
   * receiver representing a single map entry.
   *
   * - `this.key`   → key type (usually String)
   * - `this.value` → actual value type (may be scalar or class)
   */
  def addThisForMap(path: String, ctx: ExprContext): ExprContext =
    Schema.resolvePath(ctx.schema, path) match
      case Some(resolved) =>
        resolved.fieldType match
          case m: MapType =>
            // synthetic entry schema for a single map entry
            val entrySchema = ClassType(
              name = s"${path}.Entry",
              typeName = s"MapEntry[${m.keyType.typeName},${m.valueType.typeName}]",
              fields = List(
                ScalarType("key", m.keyType.typeName),
                m.valueType match
                  case s: ScalarType => s.copy(name = "value")
                  case o: OptionType => o.copy(name = "value")
                  case c: ClassType => c.copy(name = "value")
                  case other => ScalarType("value", other.typeName)
              )
            )

            val recv = Receiver(
              name = "this",
              fields = entrySchema.fields.map(ft => ft.name -> ft).toMap,
              fieldType = entrySchema,
              parentFn = Some(GetFn(path, isOptional = false))
            )

            ctx.copy(
              receiver = Some(recv),
              // push a new scope so bare `key` / `value` inside the block can resolve
              symbols = Map("this" -> entrySchema) :: ctx.symbols
            )

          case _ =>
            ctx // path exists but is not a Map
      case None =>
        ctx // path not found

  /** Build a Receiver for mapping over a Map entry: exposes `this.key` and `this.value`. */
  def mapEntryReceiverFor(recvPath: String)(using ctx: ExprContext): Receiver =
    // Locate the MapType at recvPath if present
    val mapField: MapType =
      Schema.resolvePath(ctx.schema, recvPath)
        .collect { case ResolvedType(m: MapType, _) => m }
        .getOrElse {
          // fallback if no map is found – a generic Map[Any,Any]
          MapType(
            name      = recvPath,
            keyType   = ScalarType("key", "scala.Any"),
            valueType = ScalarType("value", "scala.Any"),
            typeName  = "scala.collection.immutable.Map"
          )
        }

    // Build a synthetic ClassType representing one map entry (key + value)
    val entrySchema = ClassType(
      name     = s"$recvPath.Entry",
      typeName = s"${mapField.typeName}.Entry",
      fields   = List(
        ScalarType("key", mapField.keyType.typeName),
        mapField.valueType match
          case ct: ClassType  => ClassType("value", ct.typeName, ct.fields)
          case o: OptionType  => o.copy(name = "value")
          case s: ScalarType  => s.copy(name = "value")
          case other          => ScalarType("value", other.typeName)
      )
    )

    // Return a Receiver whose schema is this synthetic map-entry class
    Receiver(
      name      = "this",
      fields    = entrySchema.fields.map(ft => ft.name -> ft).toMap,
      fieldType = entrySchema,
      parentFn = Some(GetFn(recvPath, isOptional = false))
    )

  /** Ensure the final segment of a list-like LHS has an explicit [] (or []? for OptionalList). */
  def addWildcardToListLike(path: String)(using ctx: ExprContext): String =
    val alreadyIndexed = path.matches(""".*\[(\d+)?\]\??$""")
    if alreadyIndexed then
      path
    else
      def appendSuffix(suffix: String): String =
        val i = path.lastIndexOf('.')
        if i >= 0 then
          path.substring(0, i + 1) + path.substring(i + 1) + suffix
        else
          path + suffix

      getPathType(path) match
        case OptionType(_, inner: ListType, _) => appendSuffix("[]?")
        case _: ListType                       => appendSuffix("[]")
        case _                                  => path

  def hasListSegment(path: String): Boolean =
    path.split("\\.").exists(_.matches(""".*\[(\d+)?\]\??$"""))

  def listSegments(schema: ClassType, path: String): List[String] = {
    val parts = path.split('.').toList
    val buf = scala.collection.mutable.ListBuffer.empty[String]

    def go(node: FieldType, segs: List[String]): Unit =
      segs match
        case Nil => ()
        case head :: tail =>
          node match
            case ct: ClassType =>
              ct.fields.find(_.name == head).foreach { f =>
                f match
                  case lt: ListType =>
                    buf += head
                    go(lt.elementType, tail)
                  case ot: OptionType =>
                    go(ot.valueType, tail)
                  case inner =>
                    go(inner, tail)
              }
            case _ => ()

    go(schema, parts)
    buf.toList
  }

  /** Return the FieldType for the **nearest list element** on the path. */
  private def nearestListElement(path: String, schema: ClassType): Option[FieldType] = {

    @annotation.tailrec
    def walk(cur: ClassType,
             segs: List[String],
             accElem: Option[FieldType]): Option[FieldType] =
      segs match
        case Nil =>
          accElem

        case seg :: tail =>
          // Find the field that matches this segment
          cur.fields.find(_.name == seg) match
            case Some(listField: ListType) =>
              listField.elementType match
                case ct: ClassType =>
                  // descend into the element class and remember it
                  walk(ct, tail, Some(listField.elementType))
                case other =>
                  // scalar elements: we can’t go deeper
                  Some(listField.elementType)

            case Some(optField: OptionType) =>
              optField.valueType match
                case listField: ListType =>
                  listField.elementType match
                    case ct: ClassType =>
                      walk(ct, tail, Some(listField.elementType))
                    case other =>
                      Some(listField.elementType)
                case ct: ClassType =>
                  walk(ct, tail, accElem)
                case _ =>
                  accElem

            case Some(ct: ClassType) =>
              walk(ct, tail, accElem)

            case _ =>
              accElem

    walk(schema, path.split('.').toList, None)
  }

  // Returns the *fields* map of the nearest container class before the first list segment in `path`.
  // If none, returns Map.empty.
  def containerFieldsFor(schema: ClassType, path: String): List[FieldType] =
    val segments = path.split("\\.").toList

    @annotation.tailrec
    def walk(node: ClassType, segs: List[String]): List[FieldType] =
      segs match
        case Nil => Nil
        case seg :: tail =>
          if seg.contains('[') then
            // we've hit a list index — return the *current* container's fields
            node.fields
          else
            node.fields.find(_.name == seg) match
              case Some(c: ClassType)        => walk(c, tail)
              case Some(OptionType(_, v, _)) =>
                v match
                  case c: ClassType => walk(c, tail)
                  case _            => Nil
              case Some(ListType(_, e, _)) =>
                e match
                  case c: ClassType => walk(c, tail)
                  case _            => Nil
              case _ => Nil

    walk(schema, segments)

  def prettyFieldType(ft: FieldType): String = ft match {
    case ScalarType(_, t) => t
    case OptionType(_, v, _) => s"Option[${prettyFieldType(v)}]"
    case ListType(_, e, _) => s"List[${prettyFieldType(e)}]"
    case MapType(_, k, v, _) => s"Map[${prettyFieldType(k)}, ${prettyFieldType(v)}]"
    case ClassType(n, _, _) => n
    case ValType(_, v, _) => prettyFieldType(v)
  }