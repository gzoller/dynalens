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
// Second level:
//   arithmetic support
//   blocks (statement and fn)
//   if support
//
trait Level2 extends Level1:

  // ---- Boolean ----

  private def booleanAtom[$: P]: P[BooleanFn] =
    P(
      WS0 ~ (
        "(" ~/ booleanExpr ~ ")" |
          comparisonExpr |
          booleanLiteral2 |
          arithmeticExpr.map(toBooleanFn.apply)
      )
    )

  def booleanExpr[$: P]: P[BooleanFn] =
    P(booleanAnd ~ (WS0 ~ "||" ~ WS0 ~ booleanAnd).rep).flatMap {
      case (left, Nil) =>
        left match
          case b: BooleanFn => P(Pass(b))
          case null         => P(Fail) // soft fail to try another expression rule
      case (first, rest) =>
        P(Pass(rest.foldLeft(first)(OrFn(_, _))))
    }

  private def booleanAnd[$: P]: P[BooleanFn] =
    P(booleanNot ~ (WS0 ~ "&&" ~ WS0 ~ booleanNot).rep).map { case (first, rest) =>
      rest.foldLeft(first)(AndFn(_, _))
    }

  private def booleanNot[$: P]: P[BooleanFn] =
    P("!" ~ WS0 ~ booleanNot).map(NotFn(_)) | booleanAtom

  private def comparisonExpr[$: P]: P[BooleanFn] =
    P(
      arithmeticExpr ~ WS0 ~
        StringIn("==", "!=", ">=", "<=", ">", "<").! ~
        WS0 ~ arithmeticExpr
    ).map {
      case (left, "==", right) => EqualFn(left, right)
      case (left, "!=", right) => NotEqualFn(left, right)
      case (left, ">=", right) => GreaterThanOrEqualFn(left, right)
      case (left, "<=", right) => LessThanOrEqualFn(left, right)
      case (left, ">", right)  => GreaterThanFn(left, right)
      case (left, "<", right)  => LessThanFn(left, right)
      case (_, op, _)          => throw new RuntimeException(s"Unknown operator $op")
    }

  // ---- Arithmetic ----

  private def arithmeticExpr[$: P]: P[Fn[Any]] = arithmeticTerm

  private def arithmeticTerm[$: P]: P[Fn[Any]] =
    P(arithmeticFactor ~ (WS0 ~ CharIn("+\\-").! ~ WS0 ~ arithmeticFactor).rep).map { case (first, rest) =>
      rest.foldLeft(first) {
        case (left, ("+", right)) => AddFn(left, right)
        case (left, ("-", right)) => SubtractFn(left, right)
        case (_, (op, _))         => throw new RuntimeException(s"Unknown arithmetic operator $op")
      }
    }

  private def arithmeticFactor[$: P]: P[Fn[Any]] =
    P(unaryMinus ~ (WS0 ~ CharIn("*/%").! ~ WS0 ~ unaryMinus).rep).map { case (first, rest) =>
      rest.foldLeft(first) {
        case (left, ("*", right)) => MultiplyFn(left, right)
        case (left, ("/", right)) => DivideFn(left, right)
        case (left, ("%", right)) => ModuloFn(left, right)
        case (_, (op, _))         => throw new RuntimeException(s"Unknown arithmetic operator $op")
      }
    }

  private def arithmeticAtom[$: P]: P[Fn[Any]] =
    P(
      baseExpr(valueExpr) |
        numberLiteral |
        stringLiteral | // optional if you support it
        "(" ~/ arithmeticExpr ~ ")"
    )

  // support -x
  private def unaryMinus[$: P]: P[Fn[Any]] =
    P("-" ~/ WS0 ~ arithmeticAtom).map(NegateFn(_)) | arithmeticAtom

  // ---- String Concat ----

  private def concatExpr[$: P]: P[Fn[Any]] =
    P(arithmeticExpr ~ (WS0 ~ "::" ~ WS0 ~ arithmeticExpr).rep).map {
      case (first, Nil)  => first
      case (first, rest) => ConcatFn(first :: rest.toList)
    }

  // ---- valueExpr => Top-Level Expr ----

  def valueExpr[$: P]: P[Fn[Any]] =
    P(
      ifFn |
        blockFn | // Optional block expression
        concatExpr | // Includes hook into arithmeticExpr, which hooks into baseExpr (path+methods)
        booleanExpr.map(b => b: Fn[Any]) // Boolean logic safely downgraded
    )

  private def blockFn[$: P]: P[BlockFn[?]] =
    P(
      "{" ~/ WS0 ~
        statement.rep(sep = WS0) ~ // zero or more val/update statements
        valueExpr ~ // the final expression
        WS0 ~ "}"
    ).map { case (stmts, result) =>
      BlockFn(stmts.toList, result)
    }

  private def blockStmt[$: P]: P[BlockStmt] =
    P("{" ~/ WS0 ~ statement.rep(sep = WS0) ~ WS0 ~ "}").map(BlockStmt.apply)

  def statement[$: P]: P[Statement] =
    P(
      WS0 ~ (
        collectionStmt(booleanExpr) |
          valDecl |
          updateOrMapStmt() |
          ifStmt |
          blockStmt
      ) ~ WS0
    )

  private def valDecl[$: P]: P[ValStmt[?]] =
    P("val" ~/ WS ~ identifier.! ~ WS0 ~ "=" ~ WS0 ~ valueExpr).map { case (name, value) =>
      ValStmt(name, value)
    }

  private def updateOrMapStmt[$: P](): P[Statement] =
    P(path.map(_._1) ~ WS0 ~ "=" ~/ WS0 ~ valueExpr).map { case (p, v) =>
      if p.contains("[]") then MapStmt(p, v) else UpdateStmt(p, v)
    }

  private def ifStmt[$: P]: P[IfStmt] =
    P(
      "if" ~/ WS ~ booleanExpr ~ WS0 ~
        "then" ~ WS0 ~ statement ~
        (WS0 ~ "else" ~ WS0 ~ statement).?
    ).map { case (cond, thenPart, elseOpt) =>
      IfStmt(cond, thenPart, elseOpt)
    }

  private def ifFn[$: P]: P[Fn[Any]] =
    P(
      "if" ~/ WS ~ booleanExpr ~ WS0 ~
        "then" ~/ WS0 ~ valueExpr ~ WS0 ~
        "else" ~/ WS0 ~ valueExpr
    ).map { case (cond, thenBranch, elseBranch) =>
      IfFn(cond, thenBranch, elseBranch)
    }
