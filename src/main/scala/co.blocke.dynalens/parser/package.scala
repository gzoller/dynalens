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
    val (line, pos) = offsetToLineCol(input)
    s"[$line,$pos] Error: $msg"
  private def offsetToLineCol(input: String): (Int, Int) =
    val lines = input.take(offset).split('\n')
    val line = lines.length
    val col = lines.lastOption.map(_.length).getOrElse(0) + 1
    (line, col)

// Math functions
val M_MIN = "min"
val M_MAX = "max"
val M_SUM = "sum"
val M_AVG = "avg"
val M_MEDIAN = "median"
val M_ABS = "abs"

// Boolean functions (strings)
val M_STARTSWITH = "startsWith"
val M_ENDSWITH = "endsWith"
val M_CONTAINS = "contains"
val M_EQUALSIGNORECASE = "equalsIgnoreCase"
val M_MATCHESREGEX = "matchesRegex"

val M_ELSE = "else"

// Option functions
val M_ISDEFINED = "isDefined"

// String functions
val M_LEN = "len"
val M_TOUPPERCASE = "toUpperCase"
val M_TOLOWERCASE = "toLowerCase"
val M_TRIM = "trim"
val M_TEMPLATE = "template"
val M_SUBSTR = "substr"
val M_REPLACE = "replace"
val M_DATEFMT = "dateFmt"
val M_TODATE = "toDate"

// Seq functions (+ len, which is both)
val M_SORTASC = "sortAsc"
val M_SORTDESC = "sortDesc"
val M_FILTER = "filter"
val M_DISTINCT = "distinct"
val M_LIMIT = "limit"
val M_REVERSE = "reverse"
val M_CLEAN = "clean"

// Map functions
val M_KEYS = "keys"
val M_VALUES = "values"
val M_GET = "get"

// Misc
// val M_NOW = "now" // Date function
// val M_UUID = "uuid"
// val M_MAPFROM = "mapFrom"
// val M_MAPTO = "mapTo"
