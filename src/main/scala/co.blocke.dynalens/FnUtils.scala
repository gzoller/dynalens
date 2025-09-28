package co.blocke.dynalens

object FnUtils {
  /** Convert anything that should represent a collection into a List[Any],
   * unwrapping Option/None and normalizing null to Nil.
   */
  def asSeq(value: Any, opName: String): Either[DynaLensError, List[Any]] = value match {
    case null | None =>
      Right(Nil)

    case s: Seq[?] =>
      Right(s.asInstanceOf[Seq[Any]].toList)

    case i: Iterable[?] =>
      Right(i.asInstanceOf[Iterable[Any]].toList)

    case Some(inner) =>
      asSeq(inner, opName) // recursive unwrap

    case other =>
      Left(DynaLensError(s"$opName() may only be applied to Iterable types, but got: ${other.getClass.getSimpleName}"))
  }
}