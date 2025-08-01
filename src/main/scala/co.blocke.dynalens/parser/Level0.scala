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
    P("\"" ~/ CharsWhile(_ != '"', 0).! ~ "\"").map(s => ConstantFn(s))

  def noneLiteral[$: P]: P[Fn[Any]] =
    P("None").map(s => ConstantFn(None))

  def numberLiteral[$: P]: P[Fn[Any]] =
    P(
      (CharIn("+\\-").? ~ CharsWhileIn("0-9") ~ ("." ~ CharsWhileIn("0-9")).?).!
    ).map { raw =>
      val trimmed = raw.trim
      if trimmed.contains('.') then {
        try ConstantFn(trimmed.toDouble)
        catch
          case _: NumberFormatException =>
            throw new RuntimeException(s"Invalid double: $trimmed")
      } else {
        try ConstantFn(trimmed.toInt)
        catch
          case _: NumberFormatException =>
            try ConstantFn(trimmed.toLong)
            catch
              case _: NumberFormatException =>
                throw new RuntimeException(s"Invalid number: $trimmed")
      }
    }

  private def booleanLiteral[$: P]: P[Fn[Any]] =
    P(StringIn("true", "false").!).map {
      case "true"  => ConstantFn(true)
      case "false" => ConstantFn(false)
    } ~ WS.?

  // Specially typed boolean literal (for use in booleanExpr)
  def booleanLiteral2[$: P]: P[BooleanFn] = booleanLiteral.map(_.asInstanceOf[BooleanFn])

  def constant[$: P]: P[Fn[Any]] =
    P(stringLiteral | numberLiteral | booleanLiteral | noneLiteral)
