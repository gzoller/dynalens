package co.blocke.dynalens
package parser

import fastparse.*, NoWhitespace.*

//
// Bottom of the grammar dependency chain:
//     whitespace
//     comments
//     identifier
//     literals
//     constant (composition of literals)
//
trait Level0:

  def WS[$: P]: P[Unit] = P((CharsWhileIn(" \n\r\t") | comment).rep(1)) // WS required
  def WS0[$: P]: P[Unit] = P((CharsWhileIn(" \n\r\t") | comment).rep) // WS optional

  def Newline[$: P]: P[Unit] = P(CharsWhileIn(" \t").? ~ "\n" ~ CharsWhileIn(" \t").?)

  private def comment[$: P]: P[Unit] =
    P("#" ~ CharsWhile(_ != '\n', min = 0) ~ ("\n" | End))

  def identifier[$: P]: P[String] =
    P((CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_").?).!).map(_.trim)

  def stringLiteral[$: P]: P[Fn[Any]] =
    P("\"" ~/ CharsWhile(_ != '"', 0).! ~ "\"").map (s => ConstantFn(s))

  def numberLiteral[$: P]: P[Fn[Any]] =
    P(
      (CharIn("+\\-").? ~ CharsWhileIn("0-9") ~ ("." ~ CharsWhileIn("0-9")).?).!
    ).map { raw =>
      val trimmed = raw.trim
      if (trimmed.contains('.')) {
        try ConstantFn(trimmed.toDouble)
        catch case _: NumberFormatException =>
          throw new RuntimeException(s"Invalid double: $trimmed")
      } else {
        try ConstantFn(trimmed.toInt)
        catch
          case _: NumberFormatException =>
            try ConstantFn(trimmed.toLong)
            catch case _: NumberFormatException =>
              throw new RuntimeException(s"Invalid number: $trimmed")
      }
    }

  def booleanLiteral[$: P]: P[Fn[Any]] =
    P(StringIn("true", "false").!).map {
      case "true" => ConstantFn(true)
      case "false" => ConstantFn(false)
    } ~ WS.?

  // Specially typed boolean literal (for use in booleanExpr)
  def booleanLiteral2[$: P]: P[BooleanFn] = booleanLiteral.map(_.asInstanceOf[BooleanFn])

  def constant[$: P]: P[Fn[Any]] =
    P(stringLiteral | numberLiteral | booleanLiteral)
