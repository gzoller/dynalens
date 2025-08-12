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

import fastparse.*
import zio.*

object Script {

  def compile(script: String, lens: DynaLens[?]): zio.Task[BlockStmt] =
    given ExprContext = ExprContext(lens._typeInfo)
    zio.ZIO
      .fromEither(parseScript(script))
      .mapError(err => DynaLensError(err.render(script)))

  def compileNoZIO(script: String, lens: DynaLens[?]): Either[DynaLensError, BlockStmt] =
    given ExprContext = ExprContext(lens._typeInfo)
    parseScript(script).left.map(err => DynaLensError(err.render(script)))

  private def parseScript(script: String)(using ExprContext): Either[DLCompileError, BlockStmt] =
    parse(script, s => Grammar.topLevelBlock(using s)) match {
      case Parsed.Success(astEither, _) => astEither                    // Either[DLCompileError, BlockStmt]
      case f: Parsed.Failure            => Left(DLCompileError(f.index, f.trace().longMsg))
    }
}