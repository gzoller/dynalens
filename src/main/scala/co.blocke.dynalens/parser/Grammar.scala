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

object Grammar extends Level2 {

  // Top-level script block — no `{}`, just a list of statements
  def topLevelBlock[$: P]: P[BlockStmt] =
    P(WS0 ~ statement(expr).rep ~ WS0).map(BlockStmt(_))

  /*
      expr
       ├── ifFn
       ├── blockFn
       ├── booleanExpr
       │    └── boolOrExpr
       │         └── boolAndExpr
       │              └── boolNotExpr
       │                   └── boolBaseExpr
       │                        └── comparisonExpr
       │                             └── arithmeticExpr
       │                                  └── baseExpr
       ├── arithmeticExpr
       │    ├── termExpr       (e.g., a * b)
       │    │    └── factorExpr
       │    │         └── baseExpr
       │    └── Add/Sub (left-associative)
       └── baseExpr
            ├── constant
            ├── path
            └── standaloneFn
   */
  def expr[$: P]: P[Fn[Any]] = {
    def e: P[Fn[Any]] =
      booleanExpr(e).log("BOOL") |
      comparisonExpr(e).log("COMPARE") |
      arithmeticExpr(e).log("ARITH") |
      baseExpr(e).log("BASE") |
      ifFn(e).log("IF") |
      blockFn(e).log("BLOCK")

    P(e)
  }
}