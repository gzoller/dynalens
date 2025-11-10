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

    case _: BigInt | _: scala.math.BigInt => Some(ScalarType("", "scala.math.BigInt"))

    // put list handling **before** the default case
    case v: List[?] =>
      val elemType =
        v.headOption
          .flatMap(constantToFieldType)
          .getOrElse(ScalarType("", "scala.Any"))
      Some(ListType("", elemType, "scala.collection.immutable.List"))

    case v: Vector[?] =>
      val headT = v.headOption.flatMap(constantToFieldType)
        .getOrElse(ScalarType("", "scala.Any"))
      Some(ListType("", headT, "scala.collection.immutable.List"))

    case v: Seq[?] =>
      val headT = v.headOption.flatMap(constantToFieldType)
        .getOrElse(ScalarType("", "scala.Any"))
      Some(ListType("", headT, "scala.collection.immutable.List"))

    // must be last
    case other =>
      Some(ScalarType("", other.getClass.getName))
  }

  def rhsType(fn: Fn[?])(using ctx: ExprContext): TypeResult[FieldType] =
    println(s"[Utility.rhsType] called with ${fn.getClass.getSimpleName}: ${fn}")
    if fn != null then
      println(s"[Utility.rhsType] called for ${fn.getClass.getSimpleName}: $fn")
    else
      println("[Utility.rhsType] called with null rhs")
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
        g.maybeType match
          case Some(ct: ClassType) =>
            println(s"[rhsType:GetFn] matching field '${g.path}' against ${ct.fields.map(_.name)}")
            // Find the field type within the class by name
            ct.fields.find(_.name == g.path) match
              case Some(fieldType) =>
                TypeResult.Known(fieldType)
              case None =>
                TypeResult.Error(DLCompileError(g.posStr,s"Unknown field '${g.path}' in class ${ct.typeName}"))
          case Some(ft) =>
            TypeResult.Known(ft)
          case None =>
            TypeResult.Unknown

      // ---------- Tuple2Fn ----------  (hack--create artificial ClassType that is unpacked in CMapFn.resultType()
      case t2: Tuple2Fn =>
        for
          leftT  <- rhsType(t2.recv)
          rightT <- t2.args.headOption match
            case Some(arg) => rhsType(arg)
            case None      => TypeResult.Unknown
        yield ClassType(
          name = "",
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
        TypeResult.Known(f.resultType)

  def addThisType(cleanPath: String, ctx: ExprContext): Either[DLCompileError, ExprContext] =
    ctx.withReceiverFromPath(cleanPath) match
      case Right(ec) =>
        ec.receiver match
          case Some(nr @ NamedReceiver(_, ft, g: GetFn)) =>
            g.maybeType match
              case None =>
                // Ensure the path GetFn is typed so downstream rhsType sees a Known type
                val g2 = g.copy(maybeType = Some(ft))
                Right(ec.copy(receiver = Some(nr.copy(fn = g2))))
              case Some(_) =>
                Right(ec)
          case _ =>
            Right(ec)
      case Left(err) => Left(err)

  def containsThis(fn: Fn[?], seen: Set[Int] = Set.empty)(using ctx: ExprContext): Boolean = {
    val id = System.identityHashCode(fn)
    if seen.contains(id) then
      println(s"[containsThis] cycle detected at ${fn.getClass.getSimpleName}@$id")
      false
    else {
      val nextSeen = seen + id
      println(s"[containsThis] visiting ${fn.getClass.getSimpleName}@$id")

      fn match {
        case GetFn("this", _, _, _, _) if ctx.receiver.isEmpty =>
          println(s"[containsThis] found bare 'this'")
          true

        case g: GetFn =>
          // stop early if the receiver is RootFn or NoOpFn; those can form loops
          g.recv match {
            case RootFn | NoOpFn => false
            case r: Fn[?] => containsThis(r, nextSeen)
          }

        case c: Fn[?] =>
          // recursively check receiver + args if present
          val recvHasThis = c.recv match {
            case RootFn | NoOpFn => false
            case r: Fn[?] => containsThis(r, nextSeen)
          }
          val argsHasThis = c.args.collect { case f: Fn[?] => f }
            .exists(f => containsThis(f, nextSeen))
          recvHasThis || argsHasThis
      }
    }
  }


  def prettyFieldType(ft: FieldType): String = ft match {
    case ScalarType(_, t, _) => t
    case ListType(_, e, _, _) => s"List[${prettyFieldType(e)}]"
    case MapType(_, k, v, _, _) => s"Map[${prettyFieldType(k)}, ${prettyFieldType(v)}]"
    case ClassType(n, _, _, _) => n
    case ValType(_, v, _) => prettyFieldType(v)
    case EnumType(_, _, tn, _) => tn
  }

  def isIntegral(tn: String): Boolean =
    tn match
      case "scala.Byte" | "scala.Short" | "scala.Int" | "scala.Long" | "scala.math.BigInt" => true
      case _ => false

  def isFloating(tn: String): Boolean =
    tn match
      case "scala.Float" | "scala.Double" | "scala.math.BigDecimal" => true
      case _ => false

  def promoteIntegral(a: String, b: String): String =
    // simple lattice: Byte/Short/Int -> Int; Int+Long -> Long; anything + BigInt -> BigInt
    (a, b) match
      case ("scala.math.BigInt", _) | (_, "scala.math.BigInt") => "scala.math.BigInt"
      case ("scala.Long", _) | (_, "scala.Long") => "scala.Long"
      case _ => "scala.Int"

  def numericPromote(posStr: String, typeNames: String*): Either[DLCompileError, String] = {
    if typeNames.isEmpty then
      Left(DLCompileError(posStr,"numericPromote requires at least one type"))
    else
      val normalized = typeNames.map(normalizeNumeric)
      val rank = Map(
        "scala.Byte" -> 1,
        "scala.Short" -> 2,
        "scala.Int" -> 3,
        "scala.Long" -> 4,
        "scala.Float" -> 5,
        "scala.Double" -> 6,
        "scala.math.BigInt" -> 7,
        "scala.math.BigDecimal" -> 8
      )
      normalized.find(!rank.contains(_)) match
        case Some(bad) => Left(DLCompileError(posStr,s"Cannot promote unknown / non-numeric type: $bad"))
        case None      => Right(normalized.maxBy(rank))
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


  def isNumeric(tn: String): Boolean =
    isIntegral(tn) || isFloating(tn)

  def isStringLike(tn: String): Boolean =
    tn == "java.lang.String" || tn == "scala.Predef.String"

  enum ArithOp { case Add, Sub, Mul, Div, Mod }

  def calcNumericResultType(
                             posStr: String,
                             recv: FieldType,
                             arg: FieldType,
                             op: ArithOp
                           ): Either[DLCompileError, FieldType] = {
    val r = normalizeNumeric(recv.typeName)
    val a = normalizeNumeric(arg.typeName)

    def isNum(t: String) = Validation.isNumericType(ScalarType("", t, false))

    if !(isNum(r) && isNum(a)) then
      Left(DLCompileError(posStr, s"Incompatible operand types: $r and $a"))
    else
      val names = List(r, a)
      op match
        case ArithOp.Div =>
          val isFloaty = Set("scala.Float", "scala.Double", "scala.math.BigDecimal")
          val out =
            if names.exists(isFloaty) then
              numericPromote(posStr, r, a)
            else
              Right("scala.Double")
          out.map(t => ScalarType("", t, false))

        case ArithOp.Mod =>
          val isIntegral = Set("scala.Byte", "scala.Short", "scala.Int", "scala.Long", "scala.math.BigInt")
          if names.forall(isIntegral.contains) then
            numericPromote(posStr, r, a).map(t => ScalarType("", t, false))
          else
            Left(DLCompileError(posStr, s"Modulo requires integral operands: $r and $a"))

        case _ =>
          numericPromote(posStr, r, a).map(t => ScalarType("", t, false))
  }
