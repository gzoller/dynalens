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

  def getPathType(path: String)(using ctx: ExprContext): FieldType =
    Schema.resolvePath(ctx.schema, path) match
      case Some(resolved) =>
        resolved.fieldType match
          case ListType(_, elem, _) => elem
          case OptionType(_, inner, _) => inner
          case other => other
      case None =>
        ScalarType("", "scala.Any")

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

      case f: GetFn =>
        val cleanPath =
          f.path
            .replaceAll("\\[\\d+\\]", "")
            .replaceFirst("^_\\.", "")

        // 1) local bindings (vals/loop)
        ctx.symbols.headOption
          .flatMap(_.get(cleanPath))
          // 2) element-scoped access: this.*
          .orElse {
            if cleanPath.startsWith("this.") then
              ctx.receiver.flatMap { r =>
                val fieldName = cleanPath.stripPrefix("this.")
                // receiver already carries the element’s field map
                r.fields.get(fieldName)
              }
            else None
          }
          // 3) walk the schema for top-level and nested paths
          .orElse {
            val segments = cleanPath.split("\\.").toList

            def descend(ft: FieldType, tail: List[String]): Option[FieldType] =
              tail match
                case Nil => Some(ft)
                case h :: t => ft match
                  case c: ClassType =>
                    c.fields.find(_.name == h).flatMap(descend(_, t))
                  case o: OptionType =>
                    descend(o.valueType, tail)
                  case l: ListType if l.elementType.isInstanceOf[ClassType] =>
                    descend(l.elementType.asInstanceOf[ClassType], tail)
                  case _ => None

            segments.headOption.flatMap { h =>
              ctx.schema.fields.find(_.name == h).flatMap(descend(_, segments.tail))
            }
          }
          // 4) last conservative fallback
          .orElse {
            val ft = getPathType(cleanPath)
            if ft.typeName == "scala.Any" then None else Some(ft)
          }

      case f: ElseFn => rhsType(f.fallback)
      case f: IfFn[?] => rhsType(f.thenFn)
      case f: BlockFn[?] => rhsType(f.finalFn)

      case MapGetFn(recv, _) => recv match
        case GetFn(p, _) => Utility.mapGetValueType(p)
        case _ => None

      case IndexFn(inner, _) =>
        Utility.indexResultType(inner).orElse {
          rhsType(inner).flatMap {
            case l: ListType => Some(l.elementType)
            case o: OptionType if o.valueType.isInstanceOf[ListType] =>
              Some(o.valueType.asInstanceOf[ListType].elementType)
            case _ => None
          }
        }
      case CaseWhenFn(_, cases, default, _) =>
        val ts: List[FieldType] =
          (cases.iterator.flatMap { case (_, fn) => rhsType(fn) } ++ default.iterator.flatMap(rhsType)).toList
        ts.distinct match
          case single :: Nil => Some(single)
          case _             => None

      case c: ConstantFn[?] =>
        // If ConstantFn already carries a FieldType, prefer it; otherwise derive from value
        c match
          case cf: ConstantFn[?] if (/* has a fieldType */ false) =>
            // replace with your real accessor if present: Some(cf.fieldType)
            None
          case _ =>
            // replace `c.value` with your real accessor
            c.out match
              case _: Int     => Some(ScalarType("", "scala.Int"))
              case _: Long    => Some(ScalarType("", "scala.Long"))
              case _: Float   => Some(ScalarType("", "scala.Float"))
              case _: Double  => Some(ScalarType("", "scala.Double"))
              case _: Boolean => Some(ScalarType("", "scala.Boolean"))
              case _: String  => Some(ScalarType("", "java.lang.String"))
              case _          => Some(ScalarType("", "scala.Any"))

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
      // ----- Scalars (primitive-like) -----
      case (ScalarType(_, lt), ScalarType(_, rt)) =>
        numericCompatible(lt, rt) || lt == rt

      // ----- Options -----
      case (OptionType(_, lElem, _), OptionType(_, rElem, _)) =>
        areTypesCompatible(lElem, rElem)
      case (OptionType(_, lElem, _), other) =>
        areTypesCompatible(lElem, other)
      case (other, OptionType(_, rElem, _)) =>
        areTypesCompatible(other, rElem)

      // ----- Lists -----
      case (ListType(_, lElem, _), ListType(_, rElem, _)) =>
        areTypesCompatible(lElem, rElem)

      // ----- Maps -----
      case (MapType(_, lk, lv, _), MapType(_, rk, rv, _)) =>
        areTypesCompatible(lk, rk) && areTypesCompatible(lv, rv)

      // ----- Classes -----
      case (ClassType(_, lt, lf), ClassType(_, rt, rf)) if lt == rt =>
        // Optional: deep field compatibility check
        lf.zip(rf).forall { case (lfv, rfv) => areTypesCompatible(lfv, rfv) }

      // ----- Sealed Traits -----
      // Same trait
      case (SealedTraitType(_, lt, _, lSubs), SealedTraitType(_, rt, _, rSubs)) if lt == rt =>
        true
      // Trait on left, concrete on right
      case (SealedTraitType(_, _, _, lSubs), ClassType(_, rt, _)) =>
        lSubs.contains(rt)
      // Trait on right, concrete on left
      case (ClassType(_, lt, _), SealedTraitType(_, _, _, rSubs)) =>
        rSubs.contains(lt)

      // ----- Otherwise -----
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
    if hasExplicitIndex(path) then lhs match
      case OptionType(n, inner: ListType, tn) => inner // unwrap the Option[List]
      case _ => lhs
    else lhs

  /** If `inner` is a `GetFn(path)`, return the element type when that path
   * refers to a List or Option[List].  Otherwise `None`.
   */
  private def indexResultType(inner: Fn[Any])(using ctx: ExprContext): Option[FieldType] =
    inner match
      case GetFn(path, _) =>
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
              fieldType = entrySchema
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
      fieldType = entrySchema
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