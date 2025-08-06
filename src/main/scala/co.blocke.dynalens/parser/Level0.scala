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

  private def comment[$: P]: P[Unit] =
    P("#" ~ CharsWhile(_ != '\n', min = 0) ~ ("\n" | End))

  def identifier[$: P]: P[String] =
    P((CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_").?).!).map(_.trim)

  def stringLiteral[$: P]: P[ParseFnResult] =
    P("\"" ~/ CharsWhile(_ != '"', 0).! ~ "\"")
      .map(s => Right(ConstantFn(s)))

  private def noneLiteral[$: P]: P[ParseFnResult] =
    P("None").map(_ => Right(ConstantFn(None)))

  def numberLiteral[$: P]: P[ParseFnResult] =
    P(Index ~ (CharIn("+\\-").? ~ CharsWhileIn("0-9") ~ ("." ~ CharsWhileIn("0-9")).?).!).map {
      case (offset, raw) =>
        val trimmed = raw.trim
        if trimmed.contains('.') then {
          try Right(ConstantFn(trimmed.toDouble))
          catch case _: NumberFormatException =>
            Left(DLCompileError(offset, s"Invalid double literal: $trimmed"))
        } else {
          try Right(ConstantFn(trimmed.toInt))
          catch case _: NumberFormatException =>
            try Right(ConstantFn(trimmed.toLong))
            catch case _: NumberFormatException =>
              Left(DLCompileError(offset, s"Invalid numeric literal: $trimmed"))
        }
    }

  def booleanLiteral[$: P]: P[BooleanFn] =
    P(StringIn("true", "false").!).map {
      case "true"  => BooleanConstantFn(true)
      case "false" => BooleanConstantFn(false)
    } ~ WS.?

  def constant[$: P]: P[ParseFnResult] =
    P(
      stringLiteral |
        numberLiteral |
        noneLiteral |
        booleanLiteral.map(b => Right(b: Fn[Any])) // Upcast BooleanFn to Fn[Any]
    )