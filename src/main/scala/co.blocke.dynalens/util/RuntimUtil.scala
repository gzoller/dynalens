package co.blocke.dynalens
package util

object RuntimeUtil:


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


  /**
   * Given one or more numeric type names (e.g. "scala.Int", "scala.Long", "scala.Double", etc.),
   * return the promoted result type per numeric widening rules.
   * Throws an error if any type is not numeric or if types are incompatible.
   */
  def numericPromote(typeNames: String*): Either[String, String] = {
    if typeNames.isEmpty then
      Left("numericPromote requires at least one type")
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
        case Some(bad) => Left(s"Cannot promote unknown / non-numeric type: $bad")
        case None      => Right(normalized.maxBy(rank))
  }