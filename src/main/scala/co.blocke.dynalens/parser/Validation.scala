package co.blocke.dynalens
package parser


object Validation:

  /**
   * If either operand type is unknown, defer until phase 2.
   * Otherwise, run the supplied validator immediately.
   */
  def deferIfUnknown(
                      lhsOpt: Option[FieldType],
                      rhsOpt: Option[FieldType],
                      validatePair: (FieldType, FieldType) => Either[DLCompileError, Unit]
                    ): Either[DLCompileError, Unit] =
    (lhsOpt, rhsOpt) match
      case (Some(lhs), Some(rhs)) =>
        validatePair(lhs, rhs)
      case _ =>
        // Defer validation: treat as temporarily OK
        Right(())

  def requireNumeric2(args: List[Fn[?]], method: String, posStr: String)
                     (using ctx: ExprContext): Either[DLCompileError, Unit] =
    val types = args.map(a => Utility.rhsType(a))
    println(s"[requireNumeric2] method=$method")
    types.zipWithIndex.foreach { case (opt, i) =>
      println(s"  arg$i -> ${opt.map(_.typeName).getOrElse("<?>")}")
      println(s"  arg$i full: $opt")
      println(s"  arg$i numeric? ${opt.exists(_.isNumeric)}")
    }

    (types.headOption, types.lift(1)) match
      case (Some(Some(l)), Some(Some(r))) if l.isNumeric && r.isNumeric =>
        println(s"[requireNumeric2] ✅ numeric pair: ${l.typeName}, ${r.typeName}")
        Right(())
      case (Some(Some(l)), Some(Some(r))) =>
        println(s"[requireNumeric2] ❌ non-numeric pair: ${l.typeName}, ${r.typeName}")
        Left(DLCompileError(posStr, s"$method requires numeric operands, found ${l.typeName} and ${r.typeName}"))
      case other =>
        println(s"[requireNumeric2] ❌ insufficient args or unresolved types: $other")
        Left(DLCompileError(posStr, s"$method requires numeric operands"))

  /** Ensure both args are boolean at compile time (for &&, ||). */
  def requireBoolean2(args: List[Fn[Any]], name: String, posStr: String)
                     (using ctx: ExprContext): Either[DLCompileError, Unit] =
    (Utility.rhsType(args.head), Utility.rhsType(args(1))) match
      case (Some(lt), Some(rt)) if lt.typeName == "scala.Boolean" && rt.typeName == "scala.Boolean" =>
        Right(())
      case (Some(lt), Some(rt)) =>
        Left(DLCompileError(posStr,
          s"$name requires boolean operands, found ${lt.typeName} and ${rt.typeName}"))
      case _ =>
        Left(DLCompileError(posStr,
          s"$name cannot determine operand types"))

  /** Ensure a single arg is boolean (for unary !). */
  def requireBoolean1(arg: Fn[Any], name: String, posStr: String)
                     (using ctx: ExprContext): Either[DLCompileError, Unit] =
    Utility.rhsType(arg) match
      case Some(ft) if ft.typeName == "scala.Boolean" => Right(())
      case Some(ft) =>
        Left(DLCompileError(posStr,
          s"$name requires a boolean operand, found ${ft.typeName}"))
      case None =>
        Left(DLCompileError(posStr,
          s"$name cannot determine operand type"))

  def expectBoolean1(arg: Fn[Any], name: String, posStr: String)
  : Either[DLCompileError, BooleanFn] =
    arg match
      case b: BooleanFn => Right(b)
      case _ =>
        Left(DLCompileError(posStr, s"$name requires a boolean operand"))


  private val numericTypeNames = List(
    "scala.Byte",
    "scala.Short",
    "scala.Int",
    "scala.Long",
    "scala.Float",
    "scala.Double",
    "scala.math.BigInt",
    "scala.math.BigDecimal"
  )

  def isNumericType(ft: FieldType): Boolean =
    ft match {
      case ScalarType(_, t) if numericTypeNames.contains(t) => true
      case _ => false
    }