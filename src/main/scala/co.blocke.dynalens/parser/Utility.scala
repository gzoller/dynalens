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

object Utility:

  // We can't tell Boolean from path alone
  // Final-target typing using BOTH the path spelling (for [i]/[])
  // and the schema in ctx.typeInfo (__type for list/map, leaf "" / "?" for scalars).
  def getPathType(path: String)(using ctx: ExprContext): SymbolType = {
    val segs = path.split("\\.").toList
    val rawLast = segs.lastOption.getOrElse("")
    val hasFixedIdx = rawLast.matches(""".*\[\d+\]$""")
    val hasWildcard = rawLast.endsWith("[]")
    val hasOptQ = rawLast.endsWith("?")

    inline def normalize(seg: String): String =
      seg.replaceAll("""\[\d*\]""", "").stripSuffix("?")

    // Inline version of the old targetNodeFor: walk by exact field names,
    // and for the *last* segment return the node itself (map or string),
    // without descending into __elemType/__valType.
    def findNodeAtEnd(path: String): Option[Any] = {
      @annotation.tailrec
      def walk(cur: Map[String, Any], rest: List[String]): Option[Any] = rest match {
        case Nil => Some(cur)
        case segStr :: tail =>
          cur.get(normalize(segStr)) match {
            case Some(m: Map[?, ?] @unchecked) =>
              val mm = m.asInstanceOf[Map[String, Any]]
              if tail.isEmpty then Some(mm) else walk(mm, tail)
            case Some(s: String) =>
              if tail.isEmpty then Some(s) else None
            case _ => None
          }
      }
      walk(ctx.typeInfo, path.split("\\.").toList)
    }

    def shapeOfNode(node: Any): SymbolType = node match
      case m: Map[?, ?] @unchecked =>
        val mm = m.asInstanceOf[Map[String, Any]]
        mm.get("__type") match
          case Some("[]")  => SymbolType.List
          case Some("[]?") => SymbolType.OptionalList
          case Some("{}")  => SymbolType.Map
          case Some("{}?") => SymbolType.OptionalMap
          case Some("?")   => SymbolType.OptionalScalar
          case _           => SymbolType.Scalar // class node (fields beneath)
      case s: String =>
        if s == "?" then SymbolType.OptionalScalar else SymbolType.Scalar
      case _ => SymbolType.Scalar

    if hasFixedIdx then {
      // Normalize …[i] → …[] and consult the *list* node’s __elemType
      val parentListPath = path.replaceAll("""\[\d+\]$""", "[]")
      findNodeAtEnd(parentListPath) match
        case Some(m: Map[String @unchecked, Any @unchecked]) =>
          m.asInstanceOf[Map[String, Any]].get("__elemType") match
            case Some(elemNode) => shapeOfNode(elemNode)
            case None           => SymbolType.Scalar
        case _ => SymbolType.Scalar
    } else {
      // Normal: ask schema for the last node’s shape
      findNodeAtEnd(path) match
        case Some(node) => shapeOfNode(node)
        case None       =>
          // Fallback to spelling if schema missing
          if hasWildcard then {
            if hasOptQ then SymbolType.OptionalList else SymbolType.List
          } else if rawLast.matches(""".*\[\d+\]$""") then {
            if hasOptQ then SymbolType.OptionalScalar else SymbolType.Scalar
          } else if hasOptQ then SymbolType.OptionalScalar
          else SymbolType.Scalar
    }
  }

  def rhsType(fn: Fn[?])(using ctx: ExprContext): Option[SymbolType] =
    val fnName = fn.getClass.getSimpleName
    fn match {
      case f: GetFn =>
        // See if this is a val sym (if so look it up), else examine the path to get type
        ctx.sym.get(f.path).orElse(Some(getPathType(f.path)))
      case f: ElseFn     => rhsType(f.fallback)
      case f: IfFn[?]    => rhsType(f.thenFn)
      case f: BlockFn[?] => rhsType(f.finalFn)
      case MapGetFn(recv, _) =>
        recv match {
          case GetFn(p) => Utility.mapGetValueType(p) // non-optional now
          case _        => None // don’t guess
        }
      case IndexFn(inner, _) =>
        // Prefer precise inference via typeInfo when possible
        Utility.indexResultType(inner) orElse {
          // Fall back to old behavior only if you must, or better: return None
          rhsType(inner).flatMap {
            case SymbolType.List         => Some(SymbolType.Scalar)
            case SymbolType.OptionalList => Some(SymbolType.OptionalScalar)
            case _                       => None // don't guess
          }
        }
      case CaseWhenFn(_, cases, default, _) =>
        // try to unify RHS types; if mixed, return None (don’t over-constrain)
        val ts = cases.flatMap { case (_, fn) => Utility.rhsType(fn) } ++ default.toList.flatMap(Utility.rhsType)
        if ts.isEmpty then None
        else {
          val uniq = ts.toSet
          if uniq.size == 1 then uniq.headOption
          else None
        }
      case _ => fnReturnTypes.get(fnName)
    }
  private val fnReturnTypes = Map(
    "ConstantFn" -> SymbolType.Scalar,
    "BooleanConstantFn" -> SymbolType.Boolean,
    "EqualFn" -> SymbolType.Boolean,
    "NotEqualFn" -> SymbolType.Boolean,
    "GreaterThanFn" -> SymbolType.Boolean,
    "LessThanFn" -> SymbolType.Boolean,
    "GreaterThanOrEqualFn" -> SymbolType.Boolean,
    "LessThanOrEqualFn" -> SymbolType.Boolean,
    "AndFn" -> SymbolType.Boolean,
    "OrFn" -> SymbolType.Boolean,
    "NotFn" -> SymbolType.Boolean,
    "IsDefinedFn" -> SymbolType.Boolean,
    "toBooleanFn" -> SymbolType.Boolean,
    "StartsWithFn" -> SymbolType.Boolean,
    "EndsWithFn" -> SymbolType.Boolean,
    "ContainsFn" -> SymbolType.Boolean,
    "EqualsIgnoreCaseFn" -> SymbolType.Boolean,
    "MatchesRegexFn" -> SymbolType.Boolean,
    "NegateFn" -> SymbolType.Scalar,
    "ModuloFn" -> SymbolType.Scalar,
    "AddFn" -> SymbolType.Scalar,
    "SubtractFn" -> SymbolType.Scalar,
    "MultiplyFn" -> SymbolType.Scalar,
    "DivideFn" -> SymbolType.Scalar,
    "TrimFn" -> SymbolType.Scalar,
    "ToLowerFn" -> SymbolType.Scalar,
    "ToUpperFn" -> SymbolType.Scalar,
    "ConcatFn" -> SymbolType.Scalar,
    "InterpolateFn" -> SymbolType.Scalar,
    "SubstringFn" -> SymbolType.Scalar,
    "ReplaceFn" -> SymbolType.Scalar,
    "FilterFn" -> SymbolType.List,
    "SortFn" -> SymbolType.List,
    "DistinctFn" -> SymbolType.List,
    "LimitFn" -> SymbolType.List,
    "ReverseFn" -> SymbolType.List,
    "CleanFn" -> SymbolType.List,
    "NoneFn" -> SymbolType.None,
    "KeysFn" -> SymbolType.List,
    "ValuesFn" -> SymbolType.List,
    // TODO: Fix MapGetFn -- use typeInfo to figure return type
//    "MapGetFn" -> SymbolType.Scalar, // <-- this may be a lie, eg Map[String,List[Int]] should be List not Scalar
//    "PolyFn" -> SymbolType.Scalar, <-- Only used as an expression
    "LengthFn" -> SymbolType.Scalar,
//    "MapFwdFn" -> SymbolType.???,  <-- Only used as an expression
//    "MapRevFn" -> SymbolType.???,  <-- Only used as an expression
    "FormatDateFn" -> SymbolType.Scalar,
    "ParseDateFn" -> SymbolType.Scalar,
    "NowFn" -> SymbolType.Scalar,
    "UUIDFn" -> SymbolType.Scalar,
    "AbsFn" -> SymbolType.Scalar,
    "MinFn" -> SymbolType.Scalar,
    "MaxFn" -> SymbolType.Scalar,
    "SumFn" -> SymbolType.Scalar,
    "AvgFn" -> SymbolType.Scalar,
    "MedianFn" -> SymbolType.Scalar
  )

  /** Return the *element/value* schema for a collection node at `basePath`.
    * - For List/Option[List], looks into "__elemType".
    * - For Map/Option[Map],   looks into "__valType".
    * - For plain class nodes, returns its fields (minus "__type").
    * - For scalar leaves, returns Map.empty.
    */
  def elementSchemaFor(basePath: String, ti: Map[String, Any]): Map[String, Any] = {
    def asSchemaMap(a: Any): Map[String, Any] = a match
      case m: Map[?, ?] @unchecked => m.asInstanceOf[Map[String, Any]]
      case _                       => Map.empty

    nodeAtPath(basePath, ti) match
      case Some(m: Map[String @unchecked, Any @unchecked]) =>
        // List/Opt[List] element type?
        m.get("__elemType") match
          case Some(elemNode) =>
            val em = asSchemaMap(elemNode)
            if em.nonEmpty then em - "__type" else Map.empty
          case None =>
            // Map/Opt[Map] value type?
            m.get("__valType") match
              case Some(valNode) =>
                val vm = asSchemaMap(valNode)
                if vm.nonEmpty then vm - "__type" else Map.empty
              case None =>
                // Class schema
                val fieldsOnly = m - "__type"
                if fieldsOnly.nonEmpty then fieldsOnly else Map.empty
      case Some(_: String) => Map.empty
      case _               => Map.empty
  }

  def addThisType(cleanPath: String, ctx: ExprContext): ExprContext = {

    // Parse the LHS and collect every collection segment's name in order
    val parts = Path.parsePath(cleanPath)

    // Build a list of (name, absolutePathString) for each collection segment
    // e.g. for "pack.shipments[1].items[].number":
    //   loops = List(("pack.shipments", "shipments"), ("pack.shipments.items", "items"))
    val loops: List[(String, String)] = {
      val b = scala.collection.mutable.ListBuffer.empty[(String, String)]
      val segs = new StringBuilder
      parts.foreach {
        case Path.Field(n, _) =>
          if segs.nonEmpty then segs.append('.'); segs.append(n)
        case Path.IndexedField(n, _, _) =>
          if segs.nonEmpty then segs.append('.');
          segs.append(n)
          // absolute path up to this collection
          b += ((segs.result(), n))
      }
      b.toList
    }

    // Build a scope map: "shipments" -> <schema of Shipment element>, "items" -> <schema of Item element>
    val loopScope: Map[String, Any] =
      loops.flatMap { case (absPath, symName) =>
        val elemSchema = Utility.elementSchemaFor(absPath, ctx.typeInfo)
        if elemSchema.nonEmpty then Some(symName -> elemSchema) else None
      }.toMap

    // Install the receiver (`this`) and push the loop-scope map to the *top* of scopes
    val withRecv = ctx.withReceiverFromPath(cleanPath)
    if loopScope.nonEmpty then withRecv.copy(scopes = loopScope :: withRecv.scopes)
    else withRecv
  }

  def areTypesCompatible(lhs: SymbolType, rhs: SymbolType): Boolean =
    (lhs, rhs) match {
      case (SymbolType.Scalar, SymbolType.Scalar | SymbolType.Boolean)                                  => true
      case (SymbolType.Boolean, SymbolType.Boolean)                                                     => true
      case (SymbolType.Map, SymbolType.Map)                                                             => true
      case (SymbolType.List, SymbolType.List | SymbolType.OptionalList)                                 => true
      case (SymbolType.OptionalScalar, SymbolType.Scalar | SymbolType.OptionalScalar | SymbolType.None) => true
      case (SymbolType.OptionalList, SymbolType.List | SymbolType.OptionalList | SymbolType.None)       => true
      case (SymbolType.OptionalMap, SymbolType.Map | SymbolType.OptionalMap | SymbolType.None)          => true
      case (SymbolType.None, SymbolType.None)                                                           => true
      case _                                                                                            => false
    }

  /** Enforce that option-map (LHS is Optional*) doesn’t change container “kind”.
    * Allowed RHS for each LHS:
    *   - OptionalScalar: Scalar | Boolean | OptionalScalar | None
    *   - OptionalList  : List   | OptionalList            | None
    *   - OptionalMap   : Map    | OptionalMap             | None
    *
    * If rhsType is unknown (None), we don’t block the compile.
    */
  def checkRhsShapeForOptionMap(lhsSym: SymbolType, rhs: Fn[Any], off: Int)(using ctx: ExprContext): Either[DLCompileError, Unit] =
    lhsSym match {
      // For OptionalScalar, RHS must be element-like (Scalar) or None
      case SymbolType.OptionalScalar =>
        Utility.rhsType(rhs) match {
          case None =>
            Left(DLCompileError(off, s"Unable to infer type of RHS for option map"))
          case Some(SymbolType.Scalar | SymbolType.None) =>
            Right(())
          case Some(other) =>
            Left(DLCompileError(off, s"Option map shape mismatch: LHS expects ScalarLike but RHS is $other"))
        }

      // For OptionalList we’re doing element mapping, so don’t force RHS to be List.
      case SymbolType.OptionalList =>
        Right(())

      // If you want to special-case OptionalMap, decide your rule here. For now, allow.
      case SymbolType.OptionalMap =>
        Right(())

      // Non-option LHS or anything else: no special check.
      case _ =>
        Right(())
    }

  private val EndsWithIndexed = """.*\[\d+\]\??$""".r

  private def hasExplicitIndex(path: String): Boolean =
    EndsWithIndexed.pattern.matcher(path).matches()

  /** For '=' element writes like foo[2] = ..., treat OptionalList as List. */
  def effectiveLhsForAssignment(lhs: SymbolType, path: String): SymbolType =
    if hasExplicitIndex(path) && lhs == SymbolType.OptionalList then SymbolType.List
    else lhs

  // Map your typeInfo subtree to a SymbolType
  private def symbolTypeOfNode(node: Any): Option[SymbolType] = node match {
    case m: Map[?, ?] @unchecked =>
      val mm = m.asInstanceOf[Map[String, Any]]
      mm.get("__type") match {
        case Some("[]")  => Some(SymbolType.List)
        case Some("[]?") => Some(SymbolType.OptionalList)
        case Some("{}")  => Some(SymbolType.Map)
        case Some("{}?") => Some(SymbolType.OptionalMap)
        case _           => Some(SymbolType.Scalar) // class/leaf treated as scalar at this level
      }
    case s: String =>
      s match {
        case "[]"  => Some(SymbolType.List)
        case "[]?" => Some(SymbolType.OptionalList)
        case "{}"  => Some(SymbolType.Map)
        case "{}?" => Some(SymbolType.OptionalMap)
        case "?"   => Some(SymbolType.OptionalScalar)
        case ""    => Some(SymbolType.Scalar)
        case _     => None
      }
    case _ => None
  }

  // Navigate ctx.typeInfo by path and return the node for that path
  private def nodeAtPath(path: String, ti: Map[String, Any]): Option[Any] = {
    import scala.annotation.tailrec

    // parse "seg", capturing optional [i] or []? suffixes; reuse your parseSeg if you want.
    val segRx = """^([A-Za-z0-9_]+)(?:\[(\d*)\])?(\?)?$""".r

    def baseOf(seg: String) = seg match {
      case segRx(base, _, _) => base
      case _                 => seg
    }

    @tailrec
    def go(cur: Any, parts: List[String]): Option[Any] = (cur, parts) match {
      case (node, Nil) => Some(node)

      case (m: Map[?, ?] @unchecked, seg :: tail) =>
        val mm = m.asInstanceOf[Map[String, Any]]
        val base = baseOf(seg)

        mm.get(base) match {
          case None       => None
          case Some(next) =>
            // descend if the current node implies a nested schema
            // lists: go via __elemType
            // maps:  go via __valType
            // classes: descend into the map itself
            val down: Any = next match {
              case sub: Map[?, ?] @unchecked =>
                val s = sub.asInstanceOf[Map[String, Any]]
                s.get("__type") match {
                  case Some("[]") | Some("[]?") => s.getOrElse("__elemType", Map.empty[String, Any])
                  case Some("{}") | Some("{}?") => s.getOrElse("__valType", Map.empty[String, Any])
                  case _                        => s // class fields live in the map itself
                }
              case leaf => leaf
            }
            go(down, tail)
        }

      case _ => None
    }

    go(ti, path.split("\\.").toList)
  }

  // For an IndexFn(inner,_), infer the *element* SymbolType if possible.
  private def indexResultType(inner: Fn[Any])(using ctx: ExprContext): Option[SymbolType] = inner match {
    case GetFn(p) =>
      nodeAtPath(p, ctx.typeInfo) match {
        case Some(m: Map[?, ?]) =>
          val mm = m.asInstanceOf[Map[String, Any]]
          mm.get("__type") match {
            case Some("[]") =>
              // element could be a class schema (mm("__elemType")) or a scalar
              mm.get("__elemType") match {
                case Some(em: Map[?, ?]) => Some(SymbolType.Map) // class-valued element → treat as Map (object)
                case _                   => Some(SymbolType.Scalar) // scalar element
              }
            case Some("[]?") =>
              mm.get("__elemType") match {
                case Some(em: Map[?, ?]) => Some(SymbolType.OptionalMap)
                case _                   => Some(SymbolType.OptionalScalar)
              }
            case other =>
              // Not a list – indexing doesn’t make sense; be conservative:
              None
          }
        case Some(s: String) =>
          // leaf encodings (unlikely to be list), be conservative
          None
        case _ =>
          None
      }

    // If inner is a method chain without declared type metadata, don’t guess
    case _ => None
  }

  private def mapValueSym(node: Any): Option[SymbolType] = node match {
    case mm: Map[?, ?] @unchecked =>
      val m = mm.asInstanceOf[Map[String, Any]]
      m.get("__type") match {
        case Some("{}") | Some("{}?") =>
          m.get("__valType") match {
            case Some(s: String)    => symbolTypeOfNode(s).orElse(Some(SymbolType.Scalar))
            case Some(_: Map[?, ?]) => Some(SymbolType.Scalar) // class-valued → Scalar in coarse system
            case Some(_)            => Some(SymbolType.Scalar) // unexpected shape → be conservative
            case None               => Some(SymbolType.Scalar) // default/fallback
          }
        case _ =>
          None
      }
    case _ =>
      None
  }

  /** Non-optional value SymbolType returned by map.get() for a map at recvPath. */
  private def mapGetValueType(recvPath: String)(using ctx: ExprContext): Option[SymbolType] =
    nodeAtPath(recvPath, ctx.typeInfo).flatMap {
      case mm: Map[?, ?] @unchecked =>
        // nodeAtPath returned the map node itself (older behavior)
        mapValueSym(mm)
      case s: String =>
        // nodeAtPath has already drilled into __valType and gave us the leaf token like "" / "[]"
        symbolTypeOfNode(s)
      case _ =>
        None
    }

  // Build a receiver for mapping a Map: expose `this.key` and `this.value`,
  // and also push these into a new relative scope so bare `key`/`value` can be used if you allow it.
  def addThisForMap(path: String, ctx: ExprContext): ExprContext = {
    // Find the node for the map at `path`
    val nodeOpt = nodeAtPath(path, ctx.typeInfo)

    // Determine the value schema (from __valType), default to scalar ("") if unknown
    val valueSchema: Any = nodeOpt match {
      case Some(m: Map[?, ?] @unchecked) =>
        val mm = m.asInstanceOf[Map[String, Any]]
        mm.get("__valType").getOrElse("")
      case _ => ""
    }

    // Synthetic receiver fields
    val mapThis: Map[String, Any] = Map(
      "key" -> "", // keys are scalar in our DSL
      "value" -> valueSchema // may be scalar ("") or a nested class map
    )

    val recv = Receiver(fields = mapThis, sym = SymbolType.Map)
    ctx.copy(
      receiver = Some(recv),
      sym = ctx.sym + ("this" -> SymbolType.Map)
      // If you want bare `key`/`value` (without `this.`), also do:
      // , scopes   = mapThis :: ctx.scopes
    )
  }

  /** Return the schema node that represents the *value* of a Map at `recvPath`.
    * For a map node, we expect a subtree like: Map("__type" -> "{}", "__valType" -> <node>)
    */
  private def mapValueNodeAt(recvPath: String)(using ctx: ExprContext): Option[Any] =
    nodeAtPath(recvPath, ctx.typeInfo).flatMap {
      case m: Map[?, ?] @unchecked =>
        val mm = m.asInstanceOf[Map[String, Any]]
        mm.get("__valType")
      case _ => None
    }

  /** Build a Receiver for mapping over a Map entry: exposes `this.key` and `this.value`. */
  def mapEntryReceiverFor(recvPath: String)(using ctx: ExprContext): Receiver = {
    val valueNode: Any = mapValueNodeAt(recvPath).getOrElse("")
    val fields: Map[String, Any] =
      Map(
        "key" -> "", // treat key as scalar (String/Int/etc.)
        "value" -> valueNode // carry whatever schema the value has
      )
    // Treat the receiver "this" as Map (entry) for symbol kind
    Receiver(fields = fields, sym = SymbolType.Map)
  }

  /** Ensure the final segment of a list-like LHS has an explicit [] (or []? for OptionalList). */
  def addWildcardToListLike(path: String)(using ctx: ExprContext): String = {
    val alreadyIndexed = path.matches(""".*\[(\d+)?\]\??$""")
    if alreadyIndexed then path
    else {
      val isOptList = getPathType(path) == SymbolType.OptionalList
      val suffix = if isOptList then "[]?" else "[]"
      val i = path.lastIndexOf('.')
      if i >= 0 then path.substring(0, i + 1) + path.substring(i + 1) + suffix
      else path + suffix
    }
  }

  def hasListSegment(path: String): Boolean =
    path.split("\\.").exists(_.matches(""".*\[(\d+)?\]\??$"""))

  // Bind each ancestor *collection* segment name to its node so RHS can resolve
  // e.g. "pack.shipments.items.number" => Map("shipments" -> <node>, "items" -> <node>)
  def loopScopesFor(path: String, ti: Map[String, Any]): Map[String, Any] = {
    val segRx = """^([A-Za-z0-9_]+)(?:\[(?:\d*)\])?(\?)?$""".r
    inline def baseOf(seg: String): String = seg match {
      case segRx(base, _) => base
      case _              => seg
    }

    inline def nodeType(node: Any): Option[String] = node match {
      case m: Map[?, ?] @unchecked =>
        m.asInstanceOf[Map[String, Any]].get("__type") match {
          case Some(s: String) => Some(s)
          case _               => None
        }
      case s: String => Some(s)
      case _         => None
    }
    inline def isListNode(node: Any): Boolean =
      nodeType(node).exists(t => t == "[]" || t == "[]?")

    @annotation.tailrec
    def go(cur: Map[String, Any], parts: List[String], acc: Map[String, Any]): Map[String, Any] =
      parts match {
        case Nil => acc
        case segStr :: tail =>
          val base = baseOf(segStr)
          cur.get(base) match {
            case None => acc
            case Some(node) =>
              val acc2 = if isListNode(node) then acc + (base -> node) else acc
              go(descend(node), tail, acc2)
          }
      }

    val parts = path.split("\\.").toList
    val res1 = go(ti, parts, Map.empty)

    // If the very first segment isn’t a top-level field, try again without it.
    if res1.nonEmpty || parts.isEmpty || ti.contains(baseOf(parts.head)) then res1
    else go(ti, parts.tail, Map.empty)
  }

  private val Seg = """^([A-Za-z0-9_]+)(?:\[(?:\d*)\])?(\?)?$""".r

  // Treat true maps only if `__type` is "{}" or "{}?" *and* there is a __valType.
  // Otherwise, consider it a class schema (fields live directly in the map).
  private def nodeType(node: Any): Option[String] = node match {
    case m: Map[?, ?] @unchecked =>
      val mm = m.asInstanceOf[Map[String, Any]]
      mm.get("__type") match {
        case Some(t @ ("[]" | "[]?")) => Some(t)
        case Some(t @ ("{}" | "{}?")) =>
          if mm.contains("__valType") then Some(t) else None // class schema masquerading as "{}"
        case _ => None
      }
    case s: String => Some(s) // leaf encodings "", "?", etc.
    case _         => None
  }

  /** Return the schema map of the **nearest list element** on the path, and a reasonable SymbolType for `this`. */
  def nearestListElement(path: String, ti: Map[String, Any]): Option[(Map[String, Any], SymbolType)] = {
    val parts = path.split("\\.").toList

    @annotation.tailrec
    def walk(
        cur: Map[String, Any],
        segs: List[String],
        accElem: Option[(Map[String, Any], SymbolType)]
    ): Option[(Map[String, Any], SymbolType)] =
      segs match {
        case Nil =>
          accElem

        case seg :: tail =>
          val base = seg match {
            case Seg(b, _) => b
            case _         => seg
          }
          cur.get(base) match {
            case None =>
              accElem

            case Some(node) =>
              val tpe = nodeType(node)
              val acc2 =
                tpe match {
                  case Some("[]") =>
                    Some((descend(node), SymbolType.Scalar))
                  case Some("[]?") =>
                    Some((descend(node), SymbolType.OptionalScalar))
                  case other =>
                    accElem
                }
              walk(descend(node), tail, acc2)
          }
      }

    val fullTry = walk(ti, parts, None)
    if fullTry.isEmpty && parts.nonEmpty && !ti.contains(parts.head) then {
      walk(ti, parts.tail, None)
    } else fullTry
  }

  /** True if any segment on the path is a list in typeInfo. */
  def hasListInTypeInfo(path: String, ti: Map[String, Any]): Boolean =
    nearestListElement(path, ti).isDefined

  // Returns the *fields* map of the nearest container class before the first list segment in `path`.
  // If none, returns Map.empty.
  def containerFieldsFor(path: String, ti: Map[String, Any]): Map[String, Any] = {
    val parts = path.split("\\.").toList

    // seg base: strip [i] / [] / ? suffixes
    val segRx = """^([A-Za-z0-9_]+)(?:\[(\d*)\])?(\?)?$""".r
    inline def baseOf(seg: String): String = seg match {
      case segRx(b, _, _) => b
      case _              => seg
    }

    inline def isListSeg(seg: String): Boolean =
      seg.indexOf('[') >= 0

    @annotation.tailrec
    def go(cur: Map[String, Any], segs: List[String]): Map[String, Any] = segs match {
      case Nil => Map.empty
      case seg :: tail =>
        if isListSeg(seg) then {
          // We stop *before* the first list segment: `cur` is the container node
          descend(cur)
        } else {
          cur.get(baseOf(seg)) match {
            case Some(next) => go(descend(next), tail)
            case None       => Map.empty
          }
        }
    }

    // Try full path; if head isn't at root (e.g. paths prefixed with a root like "pack"),
    // allow dropping the first segment and try again.
    val m0 = go(ti, parts)
    if m0.nonEmpty then m0
    else if parts.nonEmpty && !ti.contains(baseOf(parts.head)) then go(ti, parts.tail)
    else Map.empty
  }

  // Where to descend for the next segment.
  // - List      → __elemType (if class schema) else no deeper fields
  // - Map       → __valType  (if class schema) else no deeper fields
  // - Class map → the map itself (its fields)
  private def descend(node: Any): Map[String, Any] = node match {
    case m: Map[?, ?] @unchecked =>
      val mm = m.asInstanceOf[Map[String, Any]]
      mm.get("__type") match {
        case Some("[]") | Some("[]?") =>
          mm.get("__elemType")
            .collect { case em: Map[?, ?] @unchecked => em.asInstanceOf[Map[String, Any]] }
            .getOrElse(Map.empty)
        case Some("{}") | Some("{}?") =>
          mm.get("__valType")
            .collect { case vm: Map[?, ?] @unchecked => vm.asInstanceOf[Map[String, Any]] }
            // If __valType is absent, treat as class schema (fields live in this map)
            .getOrElse(mm)
        case _ =>
          // No __type → class schema: fields live here
          mm
      }
    case _ => Map.empty
  }
