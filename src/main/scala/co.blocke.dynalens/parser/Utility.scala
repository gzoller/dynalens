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

  private def constantToFieldType(v: Any): Option[FieldType] = v match {
    case null => Some(ScalarType("", "scala.Null"))
    case _: String => Some(ScalarType("", "java.lang.String"))
    case _: Boolean => Some(ScalarType("", "scala.Boolean"))
    case _: Byte => Some(ScalarType("", "scala.Byte"))
    case _: Short => Some(ScalarType("", "scala.Short"))
    case _: Int => Some(ScalarType("", "scala.Int"))
    case _: Long => Some(ScalarType("", "scala.Long"))
    case _: Float => Some(ScalarType("", "scala.Float"))
    case _: Double => Some(ScalarType("", "scala.Double"))
    case _: BigDecimal => Some(ScalarType("", "scala.math.BigDecimal"))
    case _: java.util.Date => Some(ScalarType("", "java.util.Date"))
    case _: java.util.UUID => Some(ScalarType("", "java.util.UUID"))

    // put list handling **before** the default case
    case v: List[?] =>
      val elemType =
        v.headOption
          .flatMap(constantToFieldType)
          .getOrElse(ScalarType("", "scala.Any"))
      Some(ListType("", elemType, "scala.collection.immutable.List"))

    // must be last
    case other =>
      Some(ScalarType("", other.getClass.getName))
  }

  def normalizeNumeric(t: String): String = t match {
    case "Int" | "scala.Int" => "scala.Int"
    case "Long" | "scala.Long" => "scala.Long"
    case "Float" | "scala.Float" => "scala.Float"
    case "Double" | "scala.Double" => "scala.Double"
    case other => other
  }

  def isIndexable(ft: FieldType): Boolean = ft match
    case ListType(_, _, _) => true
    case OptionType(_, ListType(_, _, _), _) => true
    case _ => false

  def numericPromote(left: String, right: String): String = {
    val l = normalizeNumeric(left)
    val r = normalizeNumeric(right)
    (l, r) match {
      case ("scala.Int", "scala.Long") | ("scala.Long", "scala.Int") =>
        "scala.Long"
      case ("scala.Int", "scala.Double") | ("scala.Double", "scala.Int") =>
        "scala.Double"
      case ("scala.Double", _) | (_, "scala.Double") =>
        "scala.Double"
      case ("scala.Float", _) | (_, "scala.Float") =>
        "scala.Float"
      case ("scala.Long", _) | (_, "scala.Long") =>
        "scala.Long"
      case ("scala.Int", "scala.Int") =>
        "scala.Int"
      case other =>
        throw new IllegalArgumentException(s"Cannot promote numeric types: $other")
    }
  }

  private def postProcessIndexed(ft: FieldType, indexed: Boolean, basePath: String): FieldType =
    if !indexed then ft
    else
      ft match
        case ListType(_, elemType, _) =>
          println(s"[getPathType] path=$basePath[] returning elementType=$elemType")
          elemType
        case OptionType(_, inner: ListType, _) =>
          println(s"[getPathType] path=$basePath[] returning Option[List] elementType=${inner.elementType}")
          inner.elementType
        case other =>
          println(s"[getPathType] path=$basePath[] not a list type -> $other")
          other

  def getPathType(path: String)(using ctx: ExprContext): FieldType = {
    // --- NEW: split out any trailing fixed index (e.g. "[10]" or "[]") ---
    val idxPattern = "^(.*?)(?:\\[(?:\\d*)\\])?$".r
    val (basePath, indexed) = path match
      case idxPattern(base) if base != path => (base, true)
      case _ => (path, false)

    val segments = basePath.split("\\.").toList
    if segments.isEmpty then return ScalarType("", "scala.Any")

    // 1. check user-defined vals first (top of symbol stack wins)
    ctx.symbols.collectFirst { case m if m.contains(segments.head) => m(segments.head) } match
      case Some(v: ValType) =>
        val base = v.valueType
        val baseType =
          if segments.tail.nonEmpty then
            walkType(base, segments.tail).getOrElse(ScalarType("", "scala.Any"))
          else base

        return postProcessIndexed(baseType, indexed, basePath)

      case Some(ft) =>
        val baseType =
          if segments.tail.nonEmpty then
            walkType(ft, segments.tail).getOrElse(ScalarType("", "scala.Any"))
          else ft

        return postProcessIndexed(baseType, indexed, basePath)

      case None => // fall through to schema
    end match

    // 2. Fall through to schema
    val fieldOpt = ctx.schema.fields.find(_.name == segments.head)
    val result = fieldOpt match
      case Some(ft) =>
        val baseType = ft match
          case opt: OptionType => opt // preserve Option wrapper
          case other => other

        val resolved =
          if segments.tail.nonEmpty then
            walkType(baseType, segments.tail).getOrElse(ScalarType("", "scala.Any"))
          else baseType

        postProcessIndexed(resolved, indexed, basePath)

      case None =>
        ScalarType("", "scala.Any")

    println(s"[getPathType] resolved path=$path -> $result (${result.getClass.getSimpleName})")
    result
    // 2. fall back to schema-based resolution
//    @tailrec
//    def stepIntoForHead(ft: FieldType, headSeg: String): FieldType = {
//      val hasIndex = headSeg.matches(""".*\[\d+\]""")
//      ft match
//        case o: OptionType => stepIntoForHead(o.valueType, headSeg)
//        case l: ListType if hasIndex => l.elementType
//        case other => other
//    }
//
//    segments match
//      case head :: tail =>
//        val headNorm = head.replaceAll("\\[\\d+\\]", "")
//        ctx.schema.fields.find(_.name == headNorm) match
//          case None => ScalarType("", "scala.Any")
//          case Some(ft0) =>
//            val ft1 = stepIntoForHead(ft0, head)
//            walkType(ft1, tail).getOrElse(ScalarType("", "scala.Any"))
//
//      case Nil => ScalarType("", "scala.Any") // defensive, shouldn’t happen
  }

  /**
   * Walk down a type given path segments.
   * - Strips [123] index sugar automatically.
   * - Unwraps Option/List as needed.
   * - Stops when no match can be found.
   */
  private def walkType(ft: FieldType, segs: List[String]): Option[FieldType] =
    segs match
      case Nil =>
        Some(ft)

      case rawSeg :: tail =>
        val norm = rawSeg.replaceAll("\\[\\d+\\]", "")

        ft match
          case c: ClassType =>
            println(s"[walkType] class=${c.typeName}, seg=$norm, fields=${c.fields.map(_.name)}")
            c.fields.find(_.name == norm).flatMap(f => walkType(f, tail))

          case o: OptionType =>
            if tail.nonEmpty then
              // we’re navigating inside Option[T] — unwrap only for inner fields
              walkType(o.valueType, tail)
            else
              // path ends at the Option[T] itself → preserve OptionType
              Some(o)

          case ListType(_, elem, _) =>
            walkType(elem, segs)  // don’t consume segment, just unwrap

          case MapType(_, _, valueType, _) if segs.nonEmpty && (segs.head == "key" || segs.head == "value") =>
            Some(valueType) // or ScalarType("","scala.String") for .keys()

          case _ =>
            None

  /** Unwrap one level of Option or List to the element type */
  def unwrapCollection(ft: FieldType): Option[FieldType] = ft match {
    case ListType(_, elem, _) => Some(elem)
    case OptionType(_, ListType(_, elem, _), _) => Some(elem)
    case other => Some(other)
  }

  def rhsType(fn: Fn[?])(using ctx: ExprContext): Option[FieldType] =
    fn match

      case f: BlockFn[?] =>
        // return type of the last expression in the block
        rhsType(f.finalFn)

      // special-cases where we must inspect inner structure
      case IndexFn(inner, i) =>
        val innerT = rhsType(inner)
        println(s"[rhsType IndexFn] inner=$inner => $innerT (index=$i)")
        def elementOf(ft: FieldType): Option[FieldType] = ft match
          case ListType(_, elem, _)                         => Some(elem)
          case OptionType(_, ListType(_, elem, _), _)       => Some(elem)
          case _                                            => None

        Utility.rhsType(inner).map {
          case v: ValType => v.valueType
          case ft         => ft
        }.flatMap(elementOf)

      case NoneFn => // Special literal constant for None
        Some(OptionType("", ScalarType("", "scala.Any"), "scala.Option"))

      case f: GetFn =>
        val cleanPath  = f.path.replaceFirst("^_\\.", "")
        val normalized = cleanPath.replaceAll("\\[\\d+\\]", "")
        println(s"[DEBUG rhsType] cleanPath=$cleanPath, normalized=$normalized, f.recv=${f.recv}")

        // Check for invalid indexing before proceeding
        if cleanPath.contains("[") then
          println(s"[DEBUG rhsType:GetFn] Detected indexed path: $cleanPath")

          val basePath = cleanPath.takeWhile(_ != '[')
          println(s"[DEBUG rhsType:GetFn] basePath=$basePath")

          val baseTypeOpt = ctx.schema.fields.find(_.name == basePath)
          println(s"[DEBUG rhsType:GetFn] baseTypeOpt=$baseTypeOpt")

          baseTypeOpt match
            case Some(baseType) =>
              baseType match
                case ListType(_, _, _) | OptionType(_, ListType(_, _, _), _) =>
                  println(s"[DEBUG rhsType:GetFn] Base type ${baseType.typeName} is indexable — OK")
                case other =>
                  println(s"[DEBUG rhsType:GetFn] ❌ Base type ${other.typeName} is NOT indexable!")
                  return None
            case None =>
              println(s"[DEBUG rhsType:GetFn] ⚠️ No base field found for $basePath — schema miss?")

        // --- paths that explicitly use `this` or "_" ---
        if normalized == "this" || normalized.startsWith("this.") || normalized == "_" || normalized.startsWith("_.") then
          val base: Option[FieldType] =
            ctx.receiver.map(_.fieldType)
              .orElse(f.recv.flatMap(rhsType))

          base.flatMap { b =>
            val target = Utility.effectiveReceiverType(b)
            if normalized == "this" || normalized == "_" then
              Some(target)
            else {
              val segs = normalized.stripPrefix("this.").stripPrefix("_.").split("\\.").toList
              walkType(target, segs)
            }
          }

        // --- path with attached receiver ---
        else if f.recv.nonEmpty then
          val base0 = f.recv.flatMap(rhsType) // <- drop ctx.receiver first
          base0.flatMap { b =>
            val segs = cleanPath.split("\\.").toList
            walkType(b, segs)
          }

        // --- fallback: try SCHEMA first, then symbols ---
        else {
          // 1) Schema root resolution (top-level fields)
          val firstSeg = normalized.takeWhile(_ != '.')
          ctx.schema.fields.find(_.name == firstSeg) match
            case Some(rootFt) =>
              // exact top-level: "m" → return the field type directly
              if normalized == firstSeg then
                Some(rootFt)
              else
                // nested: "m.foo.bar" → walk starting from the root field
                val segs = normalized.split("\\.").toList.tail
                walkType(rootFt, segs)

            case None =>
              // 2) Fall back to symbol scopes (locals/vals), only if not in schema
              ctx.symbols.collectFirst { case scope if scope.contains(normalized) => scope(normalized) } match
                case Some(vt: ValType) => Some(vt)
                case Some(ft)          => Some(ft)
                case None              => None
        }

      case v: ValType =>
        println(s"[rhsType valType] $v")
        Some(v.valueType)

      case f: Fn[?] =>
        println(s"[rhsType Fn] entering: method=${f.methodName}, recv=${f.recv}, args=${f.args}")
        CompileFnRegistry.lookup(f.methodName) match
          case Some(cfn) =>
            val recvType = f.recv.flatMap(rhsType).getOrElse(ScalarType("", "scala.Any"))
            println(s"[rhsType Fn]   resolved recvType=$recvType for ${f.methodName}")

            // --- Friendly DSL name for reporting ---
            val methodName =
              CompileFnRegistry.functions.collectFirst {
                case (_, regCfn) if regCfn.getClass == cfn.getClass => regCfn.name
              }.getOrElse(f.methodName)

            // --- Early receiver rejection check ---
            if !cfn.accepts(recvType)(using ctx) then
              println(s"[rhsType Fn] receiver ${recvType.typeName} not accepted by ${cfn.name}")
              // Better human-facing error message
              throw DynaLensError(
                s"Method '$methodName' cannot be applied to receiver of type ${recvType.typeName}"
              )

            // --- Instrumentation: check accepts before args ---
            val accepts = cfn.accepts(recvType)(using ctx)
            println(s"[rhsType Fn]   cfn.accepts($recvType) = $accepts")

            if !accepts then
              println(s"[rhsType Fn]   skipping args: receiver not accepted for ${f.methodName}")
              None
            else
              val argTypeEs = f.args.map(a => rhsType(a))
              println(s"[rhsType Fn]   arg type results = $argTypeEs")
              val missingIx = argTypeEs.indexWhere(_.isEmpty)
              if missingIx >= 0 then
                println(s"[rhsType Fn]   arg#$missingIx type unresolved for arg=${f.args(missingIx)}")
                None
              else
                val argTypes = argTypeEs.flatten
                val res = cfn.resultType(recvType, argTypes)(using ctx)
                println(s"[rhsType Fn]   resultType=$res")
                Some(res)

          case None =>
            println(s"[rhsType Fn]   NO CompileFn for method=${f.methodName} (node=${f.getClass.getSimpleName})")
            f match
              case BooleanConstantFn(_) =>
                println(s"[rhsType Fn]   → BooleanConstantFn => Boolean")
                Some(ScalarType("", "scala.Boolean"))
              case ConstantFn(v) =>
                val t = Utility.constantToFieldType(v)
                println(s"[rhsType Fn]   → ConstantFn($v) => $t")
                t
              case _ =>
                println(s"[rhsType Fn]   → no idea, returning None")
                None


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
  private def rhsTypeFromSchemaPath(path: String, schema: ClassType): Option[FieldType] = {
    val segs = path.split("\\.").toList
    println(s"[rhsTypeFromSchemaPath] path=$path, schema=${schema.name}")
    segs match {
      case Nil => None
      case head :: tail =>
        schema.fields.find(_.name == head).flatMap { root =>
          if (tail.isEmpty) Some(root) else walkType(root, tail)
        }
    }
  }

  def elementTypeOf(fn: Fn[Any])(using ctx: ExprContext): FieldType =
    val r = rhsType(fn)

    println(s"[elementTypeOf] incoming fn = $fn, fn.recv = ${fn.recv}")
    val rt = rhsType(fn)
    println(s"[elementTypeOf] rhsType($fn) = $rt")

    rhsType(fn) match
      case Some(ListType(_, elem, _)) => elem
      case Some(OptionType(_, ListType(_, elem, _), _)) => elem
      case Some(ft) => ft
      case None => ScalarType("this", "scala.Any")

  def containsThis(fn: Fn[?]): Boolean =
    fn match
      case GetFn("this", _, _) => true
      case f: Fn[?] => f.args.exists(containsThis)
      case _ => false

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

  def effectiveReceiverType(ft: FieldType): FieldType = ft match
    case ListType(_, elem, _) => elem
    case OptionType(_, ListType(_, elem, _), _) => elem
    case ValType(_, inner, _) => effectiveReceiverType(inner)
    case other => other
    
  def unwrapVal(ft: FieldType): FieldType = ft match
    case v: ValType => v.valueType
    case other => other
    
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