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

// one generic error-or wrapper
type ParseResult[A] = Either[DLCompileError, A]

// concrete, readable aliases
type ParseFnResult = ParseResult[Fn[Any]]
type ParseStmtResult = ParseResult[(ExprContext, Statement)]
type ParseBoolResult = ParseResult[BooleanFn]
type ParseFnListResult = ParseResult[List[Fn[Any]]]

case class DLCompileError(posStr: String, msg: String):
  def render: String =
    s"(compile) $posStr Error: $msg"


// New: TypeResult ADT for richer result handling
sealed trait TypeResult[+A]
object TypeResult {
  case class Known[+A](value: A) extends TypeResult[A]

  case class Error(err: DLCompileError) extends TypeResult[Nothing]

  case object Unknown extends TypeResult[Nothing]

  def fromOption[A](opt: Option[A]): TypeResult[A] = opt match {
    case Some(a) => Known(a)
    case None => Unknown
  }

  def fromEither[A](e: Either[DLCompileError, A]): TypeResult[A] = e match {
    case Right(a) => Known(a)
    case Left(err) => Error(err)
  }
}
extension [A](tr: TypeResult[A])
  def map[B](f: A => B): TypeResult[B] = tr match
    case TypeResult.Known(v)  => TypeResult.Known(f(v))
    case TypeResult.Unknown   => TypeResult.Unknown
    case TypeResult.Error(e)  => TypeResult.Error(e)

  def flatMap[B](f: A => TypeResult[B]): TypeResult[B] = tr match
    case TypeResult.Known(v)  => f(v)
    case TypeResult.Unknown   => TypeResult.Unknown
    case TypeResult.Error(e)  => TypeResult.Error(e)