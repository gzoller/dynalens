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
                    )(using ctx: ExprContext): Either[DLCompileError, Unit] =
    (lhsOpt, rhsOpt) match
      case (Some(lhs), Some(rhs)) =>
        validatePair(lhs, rhs)
      case _ =>
        // Defer validation: treat as temporarily OK
        Right(())