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

import zio.*

// one generic error-or wrapper
type ParseResult[A] = Either[DLCompileError, A]

// concrete, readable aliases
type ParseFnResult = ParseResult[Fn[Any]]
type ParseStmtResult = ParseResult[(ExprContext, Statement)]
type ParseBoolResult = ParseResult[BooleanFn]
type ParseFnListResult = ParseResult[List[Fn[Any]]]

case class DLCompileError(posStr: String, msg: String):
  def render(input: String): String =
    s"(compile) $posStr Error: $msg"

case object NoOpFn extends Fn[Any]:
  override val methodName: String = "<noop>"
  override val recv: Fn[Any] = this
  override val args: List[Fn[Any]] = Nil
  override val posStr: String = "<noop>"

  override def children: List[Fn[?]] = Nil
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this

  override def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ZIO.fail(DynaLensError(posStr, "NoOpFn should never be resolved"))

