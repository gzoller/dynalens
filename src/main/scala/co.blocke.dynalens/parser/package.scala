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

// one generic error-or wrapper
type ParseResult[A] = Either[DLCompileError, A]

// concrete, readable aliases
type ParseFnResult = ParseResult[Fn[Any]]
type ParseStmtResult = ParseResult[Statement]
type ParseBoolResult = ParseResult[BooleanFn]
type ParseFnListResult = ParseResult[List[Fn[Any]]]

case class DLCompileError(offset: Int, msg: String):
  def render(input: String): String =
    val (line,pos) = offsetToLineCol(input)
    s"[$line,$pos] Error: $msg"
  private def offsetToLineCol(input: String): (Int, Int) =
    val lines = input.take(offset).split('\n')
    val line = lines.length
    val col = lines.lastOption.map(_.length).getOrElse(0) + 1
    (line, col)


extension [A](parser: P[Either[DLCompileError, A]])
  def withExprCtx(using ExprContext): P[Either[DLCompileError, A]] = parser

//
// ExprContext used during compilation
//
enum SymbolType:
//  case Exempt // eg top
  case Simple  // Fn[Any]
  case Boolean  // BooleanFn
  case Map
  case List
  case OptionalSimple
  case OptionalScalarWithDefault
  case OptionalList
  case OptionalMap

case class ExprContext(sym: Map[String, SymbolType] = Map.empty, searchThis: Boolean = false)

given defaultExprContext: ExprContext = ExprContext()

// We can't tell Boolean of OptionalSclarWithDefault from path alone
def getPathType(path: String): SymbolType = {
  val segs = path.split("\\.")
  var shape: SymbolType = SymbolType.Simple

  var sawContainer = false                 // have we hit [] / [n] / {} yet?
  var anyOptional = false                  // any ? anywhere
  var containerOptional = false            // ? that applies before or on first container

  segs.zipWithIndex.foreach { case (raw, i) =>
    val isOpt = raw.endsWith("?")
    val seg   = if (isOpt) raw.dropRight(1) else raw

    val isList = seg.endsWith("[]") || seg.matches(""".*\[\d+\]$""")
    val isMap  = seg.endsWith("{}")

    // record optionality
    if (isOpt) {
      anyOptional = true
      // optional that applies before we've entered a container, or on the container segment itself
      if (!sawContainer || isList || isMap) containerOptional = true
    }

    // update shape
    if (isList) { shape = SymbolType.List; sawContainer = true }
    else if (isMap) { shape = SymbolType.Map; sawContainer = true }
    // scalars don't change shape
  }

  // Wrap optional according to where the first relevant ? occurred
  if (sawContainer) {
    if (containerOptional) shape match {
      case SymbolType.List => SymbolType.OptionalList
      case SymbolType.Map  => SymbolType.OptionalMap
      case other           => other // shouldn't occur, but safe
    } else shape
  } else {
    // no container anywhere → scalar chain
    if (anyOptional) SymbolType.OptionalSimple else SymbolType.Simple
  }
}
