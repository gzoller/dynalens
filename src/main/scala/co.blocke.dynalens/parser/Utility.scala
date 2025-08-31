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

  // We can't tell Boolean from path alone
  // Final-target typing using BOTH the path spelling (for [i]/[])
  // and the schema in ctx.typeInfo (__type for list/map, leaf "" / "?" for scalars).
  def getPathType(path: String)(using ctx: ExprContext): SymbolType = {
    val segs = path.split("\\.").toList
    val rawLast = segs.lastOption.getOrElse("")
    val hasFixedIdx = rawLast.matches(""".*\[\d+\]$""")
    val hasWildcard = rawLast.endsWith("[]")
    val hasOptQ = rawLast.endsWith("?")

    def shapeOfNode(node: Any): SymbolType = node match
      case m: Map[?, ?] @unchecked =>
        val mm = m.asInstanceOf[Map[String, Any]]
        mm.get("__type") match
          case Some("[]") => SymbolType.List
          case Some("[]?") => SymbolType.OptionalList
          case Some("{}") => SymbolType.Map
          case Some("{}?") => SymbolType.OptionalMap
          case Some("?") => SymbolType.OptionalScalar
          case _ => SymbolType.Scalar // class node (fields beneath)
      case s: String =>
        if s == "?" then SymbolType.OptionalScalar else SymbolType.Scalar
      case _ => SymbolType.Scalar

    // If the last segment is a fixed index: consult the list’s __elemType
    if (hasFixedIdx) {
      val parentListPath = path.replaceAll("""\[\d+\]$""", "[]") // normalize to wildcard
      Utility.targetNodeFor(parentListPath, ctx.typeInfo) match
        case Some(m: Map[String @unchecked, Any @unchecked]) =>
          m.asInstanceOf[Map[String, Any]].get("__elemType") match
            case Some(elemNode) => shapeOfNode(elemNode)
            case None => SymbolType.Scalar
        case _ => SymbolType.Scalar
    } else {
      // Normal: ask typeInfo for the last node’s shape
      Utility.targetNodeFor(path, ctx.typeInfo) match
        case Some(node) => shapeOfNode(node)
        case None =>
          // Fallback to spelling if schema missing
          if (hasWildcard) if (hasOptQ) SymbolType.OptionalList else SymbolType.List
          else if (rawLast.matches(""".*\[\d+\]$"""))
            if (hasOptQ) SymbolType.OptionalScalar else SymbolType.Scalar
          else if (hasOptQ) SymbolType.OptionalScalar else SymbolType.Scalar
    }
  }

  def rhsType(fn: Fn[?] )(using ctx: ExprContext): Option[SymbolType] =
    val fnName = fn.getClass.getSimpleName
    fn match {
      case f: GetFn =>
        // See if this is a val sym (if so look it up), else examine the path to get type
        ctx.sym.get(f.path).orElse( Some(getPathType(f.path)) )
      case f: ElseFn => rhsType(f.fallback)
      case f: IfFn[?] => rhsType(f.thenFn)
      case f: BlockFn[?] => rhsType(f.finalFn)
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
    "MedianFn" -> SymbolType.Scalar,
  )

  private def normalize(seg: String): String =
    seg.replaceAll("""\[\d*\]""", "").stripSuffix("?")

  /** Return the node for the *final* segment of a path:
   * - Map[String, Any] for object/list nodes (with "__type", caller may drop it)
   * - String for leaf markers: "", "?", "{}?", "[]", "[]?", etc.
   */
  private def targetNodeFor(path: String, ti: Map[String, Any]): Option[Any] = {
    val segs = path.split("\\.").toList

    @annotation.tailrec
    def walk(cur: Map[String, Any], rest: List[String]): Option[Any] = rest match
      case Nil => Some(cur)
      case segStr :: tail =>
        cur.get(normalize(segStr)) match
          case Some(m: Map[String @unchecked, Any @unchecked]) =>
            if tail.isEmpty then Some(m) else walk(m, tail)
          case Some(s: String) =>
            if tail.isEmpty then Some(s) else None
          case _ => None

    walk(ti, segs)
  }

  // Utility.scala

  /** Return the *element/value* schema for a collection node at `basePath`.
   * - For List/Option[List], looks into "__elemType".
   * - For Map/Option[Map],   looks into "__valType".
   * - For plain class nodes, returns its fields (minus "__type").
   * - For scalar leaves, returns Map.empty.
   */
  def elementSchemaFor(basePath: String, ti: Map[String, Any]): Map[String, Any] = {
    def asSchemaMap(a: Any): Map[String, Any] = a match
      case m: Map[?, ?] @unchecked => m.asInstanceOf[Map[String, Any]]
      case _ => Map.empty

    targetNodeFor(basePath, ti) match
      case Some(m: Map[String @unchecked, Any @unchecked]) =>
        // 1) List / Option[List] ⇒ element schema under "__elemType"
        m.get("__elemType") match
          case Some(elemNode) =>
            // If element is a class schema, drop its __type; else (scalar) → empty
            val em = asSchemaMap(elemNode)
            if em.nonEmpty then em - "__type" else Map.empty

          case None =>
            // 2) Map / Option[Map] ⇒ value schema under "__valType"
            m.get("__valType") match
              case Some(valNode) =>
                val vm = asSchemaMap(valNode)
                if vm.nonEmpty then vm - "__type" else Map.empty

              case None =>
                // 3) Plain class schema (fields + "__type")
                val fieldsOnly = m - "__type"
                // If it actually had fields, return them; else scalar (empty)
                if fieldsOnly.nonEmpty then fieldsOnly else Map.empty

      // 4) Leaf marker ("" / "?" / "{}" / "[]") ⇒ scalar-ish → no element schema
      case Some(_: String) => Map.empty
      case _ => Map.empty
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
          if (segs.nonEmpty) segs.append('.'); segs.append(n)
        case Path.IndexedField(n, _, _) =>
          if (segs.nonEmpty) segs.append('.');
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
        if (elemSchema.nonEmpty) Some(symName -> elemSchema) else None
      }.toMap

    // Install the receiver (`this`) and push the loop-scope map to the *top* of scopes
    val withRecv = ctx.withReceiverFromPath(cleanPath)
    if (loopScope.nonEmpty)
      withRecv.copy(scopes = loopScope :: withRecv.scopes)
    else
      withRecv
  }

  def areTypesCompatible(lhs: SymbolType, rhs: SymbolType): Boolean =
    (lhs, rhs) match {
      case (SymbolType.Scalar, SymbolType.Scalar | SymbolType.Boolean) => true
      case (SymbolType.Boolean, SymbolType.Boolean) => true
      case (SymbolType.Map, SymbolType.Map) => true
      case (SymbolType.List, SymbolType.List | SymbolType.OptionalList) => true
      case (SymbolType.OptionalScalar, SymbolType.Scalar | SymbolType.OptionalScalar | SymbolType.None) => true
      case (SymbolType.OptionalList, SymbolType.List | SymbolType.OptionalList | SymbolType.None) => true
      case (SymbolType.OptionalMap, SymbolType.Map | SymbolType.OptionalMap | SymbolType.None) => true
      case (SymbolType.None, SymbolType.None) => true
      case _ => false
    }

  // Return a Map("shipments" -> <elem schema>, "items" -> <elem schema>, ...)
  def loopScopeFor(cleanPath: String, typeInfo: Map[String, Any]): Map[String, Any] = {
    val parts = parsePath(cleanPath)
    val indexedNames = parts.collect { case IndexedField(name, _, _) => name }
    indexedNames.foldLeft(Map.empty[String, Any]) { (acc, collName) =>
      // use your existing helper — it expects a *basePath* to the element node
      // For a field 'items', element path is typically "<field>.__type"
      val elemSchema = elementSchemaFor(collName, typeInfo)
      if (elemSchema.nonEmpty) acc + (collName -> elemSchema) else acc
    }
  }

  enum Shape {
    case ScalarLike, ListLike, MapLike
  }

  private def isOptional(t: SymbolType): Boolean = t match
    case SymbolType.OptionalScalar | SymbolType.OptionalList | SymbolType.OptionalMap => true
    case _ => false

  private def lhsInnerShape(t: SymbolType): Option[Shape] = t match
    case SymbolType.OptionalScalar => Some(Shape.ScalarLike)
    case SymbolType.OptionalList => Some(Shape.ListLike)
    case SymbolType.OptionalMap => Some(Shape.MapLike)
    case _ => None

  private def rhsShape(t: SymbolType): Shape = t match
    case SymbolType.Scalar | SymbolType.Boolean | SymbolType.OptionalScalar => Shape.ScalarLike
    case SymbolType.List | SymbolType.OptionalList => Shape.ListLike
    case SymbolType.Map | SymbolType.OptionalMap => Shape.MapLike
  // SymbolType.None is a special “Option.None” marker; we won’t turn it into a shape here.

  /** Enforce that option-map (LHS is Optional*) doesn’t change container “kind”.
   * Allowed RHS for each LHS:
   *   - OptionalScalar: Scalar | Boolean | OptionalScalar | None
   *   - OptionalList  : List   | OptionalList            | None
   *   - OptionalMap   : Map    | OptionalMap             | None
   *
   * If rhsType is unknown (None), we don’t block the compile.
   */
  def checkRhsShapeForOptionMap(lhsSym: SymbolType, rhs: Fn[Any], off: Int)
                               (using ctx: ExprContext): Either[DLCompileError, Unit] =
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
    if (hasExplicitIndex(path) && lhs == SymbolType.OptionalList) SymbolType.List
    else lhs

  // Map your typeInfo subtree to a SymbolType
  private def symbolTypeOfNode(node: Any): Option[SymbolType] = node match {
    case m: Map[?, ?] @unchecked =>
      val mm = m.asInstanceOf[Map[String, Any]]
      mm.get("__type") match {
        case Some("[]") => Some(SymbolType.List)
        case Some("[]?") => Some(SymbolType.OptionalList)
        case Some("{}") => Some(SymbolType.Map)
        case Some("{}?") => Some(SymbolType.OptionalMap)
        case _ => Some(SymbolType.Scalar) // class/leaf treated as scalar at this level
      }
    case s: String =>
      s match {
        case "[]" => Some(SymbolType.List)
        case "[]?" => Some(SymbolType.OptionalList)
        case "{}" => Some(SymbolType.Map)
        case "{}?" => Some(SymbolType.OptionalMap)
        case "?" => Some(SymbolType.OptionalScalar)
        case "" => Some(SymbolType.Scalar)
        case _ => None
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
      case _ => seg
    }

    @tailrec
    def go(cur: Any, parts: List[String]): Option[Any] = (cur, parts) match {
      case (node, Nil) => Some(node)

      case (m: Map[?, ?] @unchecked, seg :: tail) =>
        val mm = m.asInstanceOf[Map[String, Any]]
        val base = baseOf(seg)

        mm.get(base) match {
          case None => None
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
                  case _ => s // class fields live in the map itself
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
  def indexResultType(inner: Fn[Any])(using ctx: ExprContext): Option[SymbolType] = inner match {
    case GetFn(p) =>
      nodeAtPath(p, ctx.typeInfo) match {
        case Some(m: Map[?, ?]) =>
          val mm = m.asInstanceOf[Map[String, Any]]
          mm.get("__type") match {
            case Some("[]") =>
              // element could be a class schema (mm("__elemType")) or a scalar
              mm.get("__elemType") match {
                case Some(em: Map[?, ?]) => Some(SymbolType.Map) // class-valued element → treat as Map (object)
                case _ => Some(SymbolType.Scalar) // scalar element
              }
            case Some("[]?") =>
              mm.get("__elemType") match {
                case Some(em: Map[?, ?]) => Some(SymbolType.OptionalMap)
                case _ => Some(SymbolType.OptionalScalar)
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
