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

import co.blocke.dynalens.fn.*

object Utility:

  def receiverFieldTypeOf(r: Fn[?])(using ctx: ExprContext): Option[FieldType] = {
    r match {
      case g: GetFn =>
        // 1) prefer bound symbol/val
        val fromCtx = ctx.symbols.collectFirst {
          case scope if scope.contains(g.path) => scope(g.path)
        }
        // 2) fallback to schema (top-level fields)
        fromCtx.orElse(Utility.elementSchemaFor(g.path, ctx.schema))

      case _ =>
        Utility.rhsType(r) match
          case TypeResult.Known(ft) => Some(ft)
          case _                    => None
    }
  }

  /**
   * Given one or more numeric type names (e.g. "scala.Int", "scala.Long", "scala.Double", etc.),
   * return the promoted result type per numeric widening rules.
   * Throws an error if any type is not numeric or if types are incompatible.
   */
  def numericPromote(typeNames: String*): String = {
    if typeNames.isEmpty then
      throw new IllegalArgumentException("numericPromote requires at least one type")

    // Normalize all types
    val normalized: Seq[String] = typeNames.map(normalizeNumeric)

    // Define ordering of numeric types by “strength” (widest / safest to hold values)
    // Lower index = more “restrictive”/small; higher index = more "general"/wider
    val rank: Map[String, Int] = Map(
      "scala.Byte"            -> 1,
      "scala.Short"           -> 2,
      "scala.Int"             -> 3,
      "scala.Long"            -> 4,
      "scala.Float"           -> 5,
      "scala.Double"          -> 6,
      "scala.math.BigInt"     -> 7,
      "scala.math.BigDecimal" -> 8
    )

    // Check all are numeric-known; if unknown, error
    normalized.foreach { t =>
      if !rank.contains(t) then
        throw new IllegalArgumentException(s"Cannot promote unknown / non-numeric type: $t")
    }

    // Find the widest type (max rank)
    val highest = normalized.maxBy(rank)

    highest
  }


  def normalizeNumeric(t: String): String = t match
    // Scala primitives
    case "Byte" | "scala.Byte" => "scala.Byte"
    case "Short" | "scala.Short" => "scala.Short"
    case "Int" | "scala.Int" => "scala.Int"
    case "Long" | "scala.Long" => "scala.Long"
    case "Float" | "scala.Float" => "scala.Float"
    case "Double" | "scala.Double" => "scala.Double"

    // Java boxed types (often appear via reflection)
    case "java.lang.Byte" => "scala.Byte"
    case "java.lang.Short" => "scala.Short"
    case "java.lang.Integer" => "scala.Int"
    case "java.lang.Long" => "scala.Long"
    case "java.lang.Float" => "scala.Float"
    case "java.lang.Double" => "scala.Double"

    // Extended numerics — keep them distinct but normalized to consistent forms
    case "scala.math.BigDecimal" | "BigDecimal" => "scala.math.BigDecimal"
    case "scala.math.BigInt" | "BigInt" => "scala.math.BigInt"

    // Anything else: leave untouched
    case other => other


  private def resolveRootType(rootName: String)(using ctx: ExprContext): Either[DLCompileError, FieldType] =
    // search from innermost to outermost symbol scope
    ctx.symbols.iterator.flatMap(_.get(rootName)).toList.headOption match
      case Some(v: ValType) => Right(v)
      case Some(ft) => Right(ft)
      case None =>
        ctx.schema.fields.find(_.fieldName == rootName) match
          case Some(ft) => Right(ft)
          case None =>
            Left(DLCompileError(ctx.posStr, s"Unknown root field or val '$rootName'"))


  /** For a list or map at `basePath`, return the FieldType of the element/value.
   * - For `List` or `Option[List]` → its element type.
   * - For `Map` or `Option[Map]`   → its value type.
   * - For plain classes → just return their own fields as a synthetic ClassType.
   * - For scalars → None.
   */
  def elementSchemaFor(basePath: String, schema: ClassType): Option[FieldType] =
    Schema.resolvePath(schema, basePath).map(_.fieldType) match
      case Some(ListType(_, elem, _, _)) => Some(elem)
      case Some(MapType(_, _, valueType, _, _)) => Some(valueType)
      case Some(ct: ClassType) => Some(ct) // whole class itself
      case _ => None


  def getPathType(path: String)(using ctx: ExprContext): Either[DLCompileError, FieldType] = {
    val segments = path.split("\\.").toList
    if segments.isEmpty then return Right(ScalarType("", "scala.Any"))

    val posStr = ctx.posStrFrom(ctx.pos)

    // --- Step 1: resolve root ---
    val rootName = segments.head
    val rootEither = resolveRootType(rootName)
    val tail = segments.tail

    // --- Step 2: traverse remaining segments ---
    tail.foldLeft(rootEither) {
      case (Left(err), _) => Left(err)
      case (Right(currentType), segment) =>
        val (fieldName, maybeIndex) =
          if segment.contains('[') && segment.endsWith("]") then
            val base = segment.takeWhile(_ != '[')
            val idx = segment.dropWhile(_ != '[').drop(1).dropRight(1)
            (base, Some(idx))
          else (segment, None)

        currentType match
          // ---- ClassType ----
          case c: ClassType =>
            c.fields.find(_.fieldName == fieldName) match
              case Some(f) =>
                maybeIndex match
                  case None => Right(f)
                  case Some(_) =>
                    f match
                      case ListType(_, elem, _, _) => Right(elem)
                      case MapType(_, _, v, _, _) => Right(v)
                      case _ =>
                        Left(DLCompileError(ctx.posStr, s"Cannot index into non-collection field '$fieldName' at $posStr"))
              case None =>
                Left(DLCompileError(ctx.posStr, s"Unknown field '$fieldName' in class '${c.typeName}' at $posStr"))

          // ---- ListType ----
          case l: ListType =>
            maybeIndex match
              case Some(_) => Right(l.elementType)
              case None =>
                Left(DLCompileError(ctx.posStr, s"Expected index for List field but found plain reference '$fieldName' at $posStr"))

          // ---- MapType ----
          case m: MapType =>
            maybeIndex match
              case Some(_) => Right(m.valueType)
              case None =>
                Left(DLCompileError(ctx.posStr, s"Expected key access for Map field but none provided at $posStr"))

          // ---- ValType ----
          case v: ValType =>
            Right(v)

          // ---- ScalarType ----
          case s: ScalarType =>
            Left(DLCompileError(ctx.posStr, s"Cannot traverse into scalar field '${s.fieldName}' at $posStr"))
    }
  }


  /**
   * If the path ends with a fixed index (e.g. [3] or ["key"]), unwrap the
   * collection to its element/value type.
   * Wildcard hints like [] are no longer supported.
   */
//  private def postProcessIndexed(ft: FieldType, indexed: Boolean): FieldType =
//    if !indexed then ft
//    else
//      ft match
//        case ListType(_, elemType, _, _) =>
//          // foo[3] → element type
//          elemType
//
//        case MapType(_, _, valueType, _, _) =>
//          // foo["key"] → value type
//          valueType
//
//        case other =>
//          // Not indexable — leave unchanged (safe fallback)
//          other


  /**
   * Walks down a FieldType following a path of segments.
   * - Removes numeric indices (e.g. [12]).
   * - Transparently unwraps Option/List/Map as needed.
   * - Stops gracefully when no matching field exists.
   */
  private def walkType(ft: FieldType, segs: List[String]): Option[FieldType] =
    segs match
      case Nil =>
        Some(ft)

      case rawSeg :: tail =>
        val seg = rawSeg.replaceAll("\\[\\d+\\]", "") // strip numeric index like [0]

        ft match
          // ---- 1. If it's a ClassType, descend into its fields ----
          case c: ClassType =>
            c.fields.find(_.fieldName == seg).flatMap(f => walkType(f, tail))

          // ---- 2. Optional wrapper (isOptional == true) ----
          case ft if ft.isOptional =>
            val inner = ft.cloneWithOptional(false)
            if tail.nonEmpty then walkType(inner, segs)
            else Some(ft)

          // ---- 3. ListType: treat as container, go into element type ----
          case l: ListType =>
            walkType(l.elementType, tail)

          // ---- 4. MapType: treat as container, go into value type ----
          case m: MapType =>
            // .key/.value pseudo-segments if ever used (futureproof)
            if seg == "key" then Some(ScalarType("", m.keyType.typeName))
            else if seg == "value" then Some(m.valueType)
            else walkType(m.valueType, tail)

          // ---- 5. Any other type: stop ----
          case _ =>
            None


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


  // Placeholder
  def rhsType(fn: Fn[?])(using ctx: ExprContext): TypeResult[FieldType] =
    fn match
      // ---------- Blocks: type = last expression ----------
      case b: BlockFn[?] =>
        rhsType(b.finalFn)

      // ---------- Constants ----------
      case NoneFn =>
        TypeResult.Known(NoneFieldType)

      case ConstantFn(v) =>
        Utility.constantToFieldType(v) match
          case Some(ft) => TypeResult.Known(ft)
          case None     => TypeResult.Unknown

      // ---------- GetFn ----------
      case g: GetFn =>
        g.recv match
          case r if r == RootFn || r == NoOpFn =>
            Utility.getPathType(g.path) match
              case Right(ft) => TypeResult.Known(ft)
              case Left(err) => TypeResult.Error(err)
          case r =>
            rhsType(r).flatMap { baseT =>
              Utility.walkType(baseT, g.path.split("\\.").toList) match
                case Some(ft) => TypeResult.Known(ft)
                case None     => TypeResult.Unknown
            }

      // ---------- Tuple2Fn ----------  (hack--create artificial ClassType that is unpacked in CMapFn.resultType()
      case t2: Tuple2Fn =>
        for
          leftT  <- rhsType(t2.recv)
          rightT <- t2.args.headOption match
            case Some(arg) => rhsType(arg)
            case None      => TypeResult.Unknown
        yield ClassType(
          fieldName = "",
          typeName = "scala.Tuple2",
          fields = List(leftT, rightT)
        )

      // ---------- MapFn ----------
      case f: MapFn =>
        val recvType = rhsType(f.recv)
        val transformType = f.args.headOption match
          case Some(arg) => rhsType(arg)
          case None      => TypeResult.Unknown
        (recvType, transformType) match
          // List.map() → List
          case (TypeResult.Known(ListType(name, _, collType, isOpt)), TypeResult.Known(elemType)) =>
            TypeResult.Known(ListType(name, elemType, collType, isOpt))

          // Map.map(), where transform returns a Tuple ⇒ stays a Map
          case (TypeResult.Known(MapType(name, _, _, collType, isOpt)), TypeResult.Known(ClassType(_, "scala.Tuple2", fields, _))) if fields.size == 2 =>
            TypeResult.Known(MapType(name, fields.head, fields(1), collType, isOpt))

          // Map.map(), where transform returns a scalar ⇒ becomes List
          case (TypeResult.Known(MapType(name, _, _, collType, isOpt)), TypeResult.Known(elemType)) =>
            TypeResult.Known(ListType(name, elemType, "scala.collection.immutable.List", isOpt))

          case (TypeResult.Error(err), _) => TypeResult.Error(err)
          case (_, TypeResult.Error(err)) => TypeResult.Error(err)
          case _ => TypeResult.Unknown

      // ---------- IndexFn (indexing at runtime) ----------
      case i: IndexFn =>
        rhsType(i.recv).flatMap {
          case l: ListType =>
            // unwrap list element
            val elem = l.elementType
            TypeResult.Known(elem.cloneWithOptional(l.isOptional))

          case m: MapType =>
            // map lookup is safe → return Option[valueType]
            val v = m.valueType.cloneWithOptional(true)
            TypeResult.Known(v)

          case ft if ft.isOptional =>
            // if the container itself is optional, unwrap one level
            val inner = ft.cloneWithOptional(false)
            TypeResult.Known(inner)
          case _ =>
            TypeResult.Unknown
        }

      // ---------- CaseWhenFn ----------
      case cw: CaseWhenFn =>
        val branchTypes = cw.cases.map(_._2).map(rhsType(_)(using ctx))
        val defaultType = cw.default.map(rhsType(_)(using ctx))
        val allTypes: List[FieldType] =
          (branchTypes.collect { case TypeResult.Known(ft) => ft }.toList) ++
            (defaultType match { case Some(TypeResult.Known(ft)) => List(ft); case _ => Nil })

        if allTypes.isEmpty then TypeResult.Unknown
        else if allTypes.map(_.typeName).distinct.size == 1 then TypeResult.Known(allTypes.head)
        else TypeResult.Known(ScalarType("", "scala.Any"))

      // ---------- Generic Fn nodes ----------
      case f: Fn[?] =>
        CompileFnRegistry.lookup(f.methodName) match
          case Some(cfn) =>
            val recvType =
              rhsType(f.recv) match
                case TypeResult.Known(ft) => ft
                case TypeResult.Unknown   => ctx.receiver.map(_.ftype).getOrElse(ScalarType("", "scala.Any"))
                case TypeResult.Error(_)  => ScalarType("", "scala.Any")

            val argTypeResults = f.args.map(rhsType)
            if argTypeResults.exists {
              case TypeResult.Error(_) => true
              case TypeResult.Unknown  => true
              case _ => false
            } then
              argTypeResults.collectFirst { case TypeResult.Error(err) => TypeResult.Error(err) }.getOrElse(TypeResult.Unknown)
            else
              val argTypes = argTypeResults.collect { case TypeResult.Known(ft) => ft }
              val recvName =
                if f.recv == RootFn then "<anon>"
                else f.recv.toString

              TypeResult.Known(
                cfn.resultType(
                  NamedReceiver(recvName, recvType, f.recv),
                  argTypes
                )
              )

          case None =>
            f match
              case _: BooleanConstantFn => TypeResult.Known(ScalarType("", "scala.Boolean"))
              case _: ConstantFn[?]     => TypeResult.Known(ScalarType("", "scala.Any"))
              case _                    => TypeResult.Unknown


  def isIndexable(ft: FieldType): Boolean = ft match
    case _: ListType => true
    case _: MapType => true
    case _ => false


  def unwrapVal(ft: FieldType): FieldType = ft match
    case v: ValType => v.valueType
    case other => other

  def isPathOptional(path: String, ctx: ExprContext): Boolean =
    Schema.resolvePath(ctx.schema, path).exists {
      case ResolvedType(ft, _) => ft.isOptional
    }

  def addThisType(cleanPath: String, ctx: ExprContext): Either[DLCompileError, ExprContext] = {
    val parts = Path.parsePath(cleanPath)

    // collect every collection segment along the path
    val loops: List[(String, String)] = {
      val buf = scala.collection.mutable.ListBuffer.empty[(String, String)]
      val path = new StringBuilder
      parts.foreach {
        case Path.Field(name) =>
          if path.nonEmpty then path.append('.')
          path.append(name)

        case Path.IndexedField(name, _) =>
          if path.nonEmpty then path.append('.')
          path.append(name)
          buf += ((path.toString, name)) // ← safer than result()
      }
      buf.toList
    }

    // resolve each loop segment’s element/value type
    val loopSymbols: Map[String, FieldType] =
      loops.flatMap { case (absPath, loopName) =>
        Schema.resolvePath(ctx.schema, absPath).flatMap { resolved =>
          resolved.fieldType match {
            case l: ListType => Some(loopName -> l.elementType)
            case m: MapType  => Some(loopName -> m.valueType)
            case ft if ft.isOptional =>
              ft match
                case l: ListType => Some(loopName -> l.elementType)
                case m: MapType  => Some(loopName -> m.valueType)
                case _           => None
            case _ => None
          }
        }
      }.toMap

    ctx.withReceiverFromPath(cleanPath) match
      case Right(withRecv) =>
        if loopSymbols.nonEmpty then
          Right(withRecv.copy(symbols = loopSymbols :: withRecv.symbols))
        else
          Right(withRecv)
      case Left(err) => Left(err)
  }

  def containsThis(fn: Fn[?]): Boolean =
    fn match
      case GetFn(path, _, _, _) =>
        path == "this" || path.startsWith("this.")
      case _ =>
        // Always scan both the receiver and the args
        (fn.recv :: fn.args).exists(containsThis)

  // get the NamedReceiver for a map entry's value type at the given path
  def mapEntryReceiverFor(path: String)(using ctx: ExprContext): NamedReceiver =
    Utility.getPathType(path) match
      case Right(ft: MapType) =>
        NamedReceiver(path, ft.valueType, NoOpFn)
      case Right(v: ValType) if v.valueType.isInstanceOf[MapType] =>
        val mt = v.valueType.asInstanceOf[MapType]
        NamedReceiver(path, mt.valueType, NoOpFn)
      case _ =>
        // Fallback: unknown or error, use Any so parsing continues
        NamedReceiver(path, ScalarType("", "scala.Any"), NoOpFn)

  def prettyFieldType(ft: FieldType): String = ft match {
    case ScalarType(_, t, _) => t
    case ListType(_, e, _, _) => s"List[${prettyFieldType(e)}]"
    case MapType(_, k, v, _, _) => s"Map[${prettyFieldType(k)}, ${prettyFieldType(v)}]"
    case ClassType(n, _, _, _) => n
    case ValType(_, v, _) => prettyFieldType(v)
  }