package co.blocke.dynalens
package parser

import fastparse.*
import NoWhitespace.*

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

  /*
  enum SymbolType:
  case Scalar  // Fn[Any]
  case Boolean  // BooleanFn
  case Map
  case List
  case OptionalSimple
  case OptionalList
  case OptionalMap
    */
  def rhsType( fn: Fn[?] )(using ctx: ExprContext): Option[SymbolType] =
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
    "GreatherThanFn" -> SymbolType.Boolean,
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

  /*
  private def normalize(seg: String): String =
    seg.replaceAll("""\[\d*\]""", "").stripSuffix("?")

  def elementSchemaFor(basePath: String, ti: Map[String, Any]): Map[String, Any] = {
    val segs = basePath.split("\\.").toList

    @annotation.tailrec
    def loop(cur: Map[String, Any], rest: List[String]): Map[String, Any] = rest match
      case Nil => cur
      case seg :: tail =>
        cur.get(normalize(seg)) match
          case Some(m: Map[String @unchecked, Any @unchecked]) => loop(m, tail)
          case _                                                => Map.empty

    val node = loop(ti, segs)
    node.get("__type").collect { case s: String => s } match
      case Some("[]") | Some("[]?") | Some("{}") | Some("{}?") => node - "__type"
      case _                                                   => Map.empty // primitive or unknown → no relative fields
  }
  */

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

  def addThisType(path: String, ctx: ExprContext): ExprContext =
    if path != "this" && !ctx.relativeFields.contains("this") then
      val targetSym: SymbolType = getPathType(path)
      val endsAtElement = {
        val last = path.split("\\.").lastOption.getOrElse("")
        last.matches(""".*\[(\d+)?\]\??$""")
      }
      val thisFields: Map[String, Any] =
        if (endsAtElement) elementSchemaFor(path, ctx.typeInfo) // element schema (minus __type)
        else Map.empty // field value is scalar/leaf so no schema
      ctx.copy(
        relativeFields = ctx.relativeFields + ("this" -> thisFields),
        // NOTE: do NOT set searchThis here
        sym = ctx.sym + ("this" -> targetSym)
      )
    else ctx

