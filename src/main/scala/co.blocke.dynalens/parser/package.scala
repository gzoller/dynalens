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

case class DLCompileError(offset: Int, msg: String):
  def render(input: String): String =
    val (line,pos) = offsetToLineCol(input)
    s"[$line,$pos] Error: $msg"
  private def offsetToLineCol(input: String): (Int, Int) =
    val lines = input.take(offset).split('\n')
    val line = lines.length
    val col = lines.lastOption.map(_.length).getOrElse(0) + 1
    (line, col)


//extension [A](parser: P[Either[DLCompileError, A]])
//  def withExprCtx(using ExprContext): P[Either[DLCompileError, A]] = parser

//
// ExprContext used during compilation
//
enum SymbolType:
  case Scalar  // Fn[Any]
  case Boolean  // BooleanFn
  case Map
  case List
  case OptionalScalar
  case OptionalList
  case OptionalMap
  case None

private def pretty(sym: SymbolType): String =
  sym match {
    case SymbolType.Scalar => "scalar"
    case SymbolType.Boolean => "boolean"
    case SymbolType.Map => "map"
    case SymbolType.List => "list"
    case SymbolType.OptionalScalar => "optional scalar"
    case SymbolType.OptionalList => "optional list"
    case SymbolType.OptionalMap => "optional map"
    case SymbolType.None => "none"
  }

//case class ExprContext(
//                        typeInfo: Map[String, Any],
//                        sym: Map[String, SymbolType] = Map.empty,
//                        searchThis: Boolean = false,
//                        relativeFields: Map[String, Any] = Map.empty
//                      ):
//  def merge(that: ExprContext): ExprContext =
//    this.copy(
//      typeInfo = this.typeInfo ++ that.typeInfo,
//      sym = this.sym ++ that.sym
//    )

val M_STARTSWITH = "startsWith"
val M_ENDSWITH = "endsWith"
val M_CONTAINS = "contains"
val M_EQUALSIGNORECASE = "equalsIgnoreCase"
val M_MATCHESREGEX = "matchesRegex"
val M_ELSE = "else"
val M_ISDEFINED = "isDefined"
val M_LEN = "len"
val M_TOUPPERCASE = "toUpperCase"
val M_TOLOWERCASE = "toLowerCase"
val M_TRIM = "trim"
val M_TEMPLATE = "template"
val M_SUBSTR = "substr"
val M_REPLACE = "replace"
val M_DATEFMT = "dateFmt"
val M_TODATE = "toDate"
val M_SORTASC = "sortAsc"
val M_SORTDESC = "sortDesc"
val M_FILTER = "filter"
val M_DISTINCT = "distinct"
val M_LIMIT = "limit"
val M_REVERSE = "reverse"
val M_CLEAN = "clean"
