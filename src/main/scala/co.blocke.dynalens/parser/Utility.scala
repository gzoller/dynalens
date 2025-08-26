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
  // Final-target typing from the *last segment's* suffixes only (no schema needed).
  def getPathType(path: String): SymbolType = {
    val rawLast = path.split("\\.").lastOption.getOrElse("")
    val isOpt   = rawLast.endsWith("?")
    val base    = if (isOpt) rawLast.dropRight(1) else rawLast

    val isIndexedElem = base.matches(""".*\[\d+\]$""") // ...[3]
    val isWildcardElt = base.endsWith("[]")            // ...[]
    val isMap         = base.endsWith("{}")            // ...{}

    if (isWildcardElt) {
      if (isOpt) SymbolType.OptionalList else SymbolType.List
    } else if (isIndexedElem) {
      if (isOpt) SymbolType.OptionalScalar else SymbolType.Scalar
    } else if (isMap) {
      if (isOpt) SymbolType.OptionalMap else SymbolType.Map
    } else {
      if (isOpt) SymbolType.OptionalScalar else SymbolType.Scalar
    }
  }

  @tailrec
  def rhsType(fn: Fn[?] )(using ctx: ExprContext): Option[SymbolType] =
    val fnName = fn.getClass.getSimpleName
    fn match {
      case f: GetFn =>
        // See if this is a val sym (if so look it up), else examine the path to get type
        ctx.sym.get(f.path).orElse( Some(getPathType(f.path)) )
      case f: ElseFn => rhsType(f.fallback)
      case f: IfFn[?] => rhsType(f.thenFn)
      case f: BlockFn[?] => rhsType(f.finalFn)
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
//    "PolyFn" -> SymbolType.Scalar, <-- Only used as an expression
    "LengthFn" -> SymbolType.Scalar,
//    "MapFwdFn" -> SymbolType.???,  <-- Only used as an expression
//    "MapRevFn" -> SymbolType.???,  <-- Only used as an expression
    "FormatDateFn" -> SymbolType.Scalar,
    "ParseDateFn" -> SymbolType.Scalar,
    "NowFn" -> SymbolType.Scalar,
    "UUIDFn" -> SymbolType.Scalar,
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

  /** For element scope, you already have this: returns element schema minus __type */
  def elementSchemaFor(basePath: String, ti: Map[String, Any]): Map[String, Any] = {
    targetNodeFor(basePath, ti) match
      case Some(m: Map[String @unchecked, Any @unchecked]) => m - "__type"
      case _ => Map.empty
  }

  /** Leaf marker for a field target: "", "?" (optional), etc. */
  def leafMarkerFor(path: String, ti: Map[String, Any]): String =
    targetNodeFor(path, ti) match
      case Some(s: String) => s
      case _ => "" // fallback: treat as scalar

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
  // Utility.scala
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
/*
  def checkRhsShapeForOptionMap(
                                 lhs: SymbolType,
                                 rhs: Fn[?],
                                 off: Int
                               )(using ctx: ExprContext): Either[DLCompileError, Unit] = {

    if !isOptional(lhs) then
      Right(()) // only enforced for Optional* LHS
    else
      rhsType(rhs) match {
        case None =>
          // Unknown compile-time RHS type; allow it (runtime will enforce)
          Right(())

        case Some(SymbolType.None) =>
          // Explicitly producing None is always okay for Option map
          Right(())

        case Some(rsym) =>
          (lhsInnerShape(lhs), rsym) match {
            // OptionalScalar accepts Scalar-like (Scalar or Boolean) and OptionalScalar
            case (Some(Shape.ScalarLike), SymbolType.Scalar)         => Right(())
            case (Some(Shape.ScalarLike), SymbolType.Boolean)        => Right(())
            case (Some(Shape.ScalarLike), SymbolType.OptionalScalar) => Right(())

            // OptionalList accepts List and OptionalList
            case (Some(Shape.ListLike),   SymbolType.List)           => Right(())
            case (Some(Shape.ListLike),   SymbolType.OptionalList)   => Right(())

            // OptionalMap accepts Map and OptionalMap
            case (Some(Shape.MapLike),    SymbolType.Map)            => Right(())
            case (Some(Shape.MapLike),    SymbolType.OptionalMap)    => Right(())

            // Anything else → mismatch
            case (Some(shape), otherSym) =>
              Left(DLCompileError(
                off,
                s"Option map shape mismatch: LHS expects $shape but RHS is $otherSym"
              ))

            // Shouldn’t happen (lhs is optional), but be permissive
            case (None, _) =>
              Right(())
          }
      }
  }
  */