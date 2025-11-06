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
      opt match
        case TypeResult.Known(ft) =>
          println(s"  arg$i -> ${ft.typeName}")
          println(s"  arg$i full: $ft")
          println(s"  arg$i numeric? ${ft.isNumeric}")
        case TypeResult.Unknown =>
          println(s"  arg$i -> UNKNOWN")
        case TypeResult.Error(e) =>
          println(s"  arg$i -> ERROR: ${e.msg}")
    }

    (types.headOption, types.lift(1)) match
      case (Some(TypeResult.Known(l)), Some(TypeResult.Known(r))) if l.isNumeric && r.isNumeric =>
        println(s"[requireNumeric2] ✅ numeric pair: ${l.typeName}, ${r.typeName}")
        Right(())
      case (Some(TypeResult.Known(l)), Some(TypeResult.Known(r))) =>
        println(s"[requireNumeric2] ❌ non-numeric pair: ${l.typeName}, ${r.typeName}")
        Left(DLCompileError(posStr, s"$method requires numeric operands, found ${l.typeName} and ${r.typeName}"))
      case (Some(TypeResult.Error(e)), _) => Left(e)
      case (_, Some(TypeResult.Error(e))) => Left(e)
      case _ =>
        println(s"[requireNumeric2] ❌ insufficient args or unresolved types")
        Left(DLCompileError(posStr, s"$method requires numeric operands"))

  /** Ensure both args are boolean at compile time (for &&, ||). */
  def requireBoolean2(args: List[Fn[Any]], name: String, posStr: String)
                     (using ctx: ExprContext): Either[DLCompileError, Unit] =
    val leftT = Utility.rhsType(args.head)
    val rightT = Utility.rhsType(args(1))
    (leftT, rightT) match
      case (TypeResult.Known(lt), TypeResult.Known(rt)) if lt.typeName == "scala.Boolean" && rt.typeName == "scala.Boolean" =>
        Right(())
      case (TypeResult.Known(lt), TypeResult.Known(rt)) =>
        Left(DLCompileError(posStr, s"$name requires boolean operands, found ${lt.typeName} and ${rt.typeName}"))
      case (TypeResult.Error(e), _) => Left(e)
      case (_, TypeResult.Error(e)) => Left(e)
      case _ =>
        Left(DLCompileError(posStr, s"$name cannot determine operand types"))

  /** Ensure a single arg is boolean (for unary !). */
  def requireBoolean1(arg: Fn[Any], name: String, posStr: String)
                     (using ctx: ExprContext): Either[DLCompileError, Unit] =
    Utility.rhsType(arg) match
      case TypeResult.Known(ft) if ft.typeName == "scala.Boolean" => Right(())
      case TypeResult.Known(ft) =>
        Left(DLCompileError(posStr, s"$name requires a boolean operand, found ${ft.typeName}"))
      case TypeResult.Error(e) => Left(e)
      case _ => Left(DLCompileError(posStr, s"$name cannot determine operand type"))

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
      case ScalarType(_, t, isOpt) if numericTypeNames.contains(t) && !isOpt => true
      case _ => false
    }