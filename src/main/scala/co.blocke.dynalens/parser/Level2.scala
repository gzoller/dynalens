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

  /** Helpers to combine boolean results left-to-right, short-circuiting on the first Left */
  private inline def andCombine(a: ParseBoolResult, b: ParseBoolResult): ParseBoolResult =
    (a, b) match
      case (Left(e), _) => Left(e)
      case (Right(_), Left(e)) => Left(e)
      case (Right(a1), Right(b1)) => Right(AndFn(a1, b1))

  private inline def orCombine(a: ParseBoolResult, b: ParseBoolResult): ParseBoolResult =
    (a, b) match
      case (Left(e), _) => Left(e)
      case (Right(_), Left(e)) => Left(e)
      case (Right(a1), Right(b1)) => Right(OrFn(a1, b1))

  /** atom := '(' booleanExpr ')' | comparisonExpr | booleanLiteral | toBoolean(arithmeticExpr) */
  private def booleanAtom[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(
      WS0 ~ (
        "(" ~/ booleanExpr ~ ")" |
          comparisonExpr |
          booleanLiteral.map(b => Right(b): ParseBoolResult) |
          arithmeticExpr.map(_.map(toBooleanFn.apply): ParseBoolResult)
        )
    )

  /** booleanExpr := booleanAnd ('||' booleanAnd)* */
  def booleanExpr[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(booleanAnd ~ (WS0 ~ "||" ~ WS0 ~ booleanAnd).rep).map { (first, rest) =>
      rest.foldLeft(first)(orCombine)
    }

  /** booleanAnd := booleanNot ('&&' booleanNot)* */
  private def booleanAnd[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(booleanNot ~ (WS0 ~ "&&" ~ WS0 ~ booleanNot).rep).map { (first, rest) =>
      rest.foldLeft(first)(andCombine)
    }

  /** booleanNot := '!' booleanNot | atom */
  private def booleanNot[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(("!" ~ WS0 ~ booleanNot).map {
      case Right(b) => Right(NotFn(b)): ParseBoolResult
      case Left(e) => Left(e)
    } | booleanAtom)

  /** comparisonExpr := arithmeticExpr (==|!=|>=|<=|>|<) arithmeticExpr */
  private def comparisonExpr[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(
      arithmeticExpr ~ WS0 ~
        StringIn("==", "!=", ">=", "<=", ">", "<").! ~
        WS0 ~ arithmeticExpr
    ).map { case (lE, op, rE) =>
      for {
        left  <- lE
        right <- rE
      } yield op match
        case "==" => EqualFn(left, right)
        case "!=" => NotEqualFn(left, right)
        case ">=" => GreaterThanOrEqualFn(left, right)
        case "<=" => LessThanOrEqualFn(left, right)
        case ">"  => GreaterThanFn(left, right)
        case "<"  => LessThanFn(left, right)
    }

  // ---- Arithmetic ----
  
  private def arithmeticExpr[$: P](using ctx: ExprContext): P[ParseFnResult] =
    arithmeticTerm

  private def arithmeticTerm[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(Index ~ arithmeticFactor ~ (WS0 ~ CharIn("+\\-").! ~ WS0 ~ arithmeticFactor).rep).map {
      case (off, first, rest) =>
        rest.foldLeft(first) {
          case (Left(e), _) => Left(e) // short-circuit error
          case (Right(acc), (op, right)) =>
            right match {
              case Left(e) => Left(e)
              case Right(r) =>
                op match
                  case "+" => Right(AddFn(acc, r))
                  case "-" => Right(SubtractFn(acc, r))
                  case _ =>
                    Left(DLCompileError(off, s"Unsupported arithmetic operator: $op"))
            }
        }
    }

  private def arithmeticFactor[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(Index ~ unaryMinus ~ (WS0 ~ CharIn("*/%").! ~ WS0 ~ unaryMinus).rep).map {
      case (off, first, rest) =>
        rest.foldLeft(first) {
          case (Left(e), _) => Left(e) // propagate first error
          case (Right(acc), (op, right)) =>
            right match {
              case Left(e) => Left(e)
              case Right(r) =>
                op match
                  case "*" => Right(MultiplyFn(acc, r))
                  case "/" => Right(DivideFn(acc, r))
                  case "%" => Right(ModuloFn(acc, r))
                  case _ =>
                    Left(DLCompileError(off, s"Unsupported arithmetic operator: $op"))
            }
        }
    }

  private def arithmeticAtom[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(
      baseExpr(valueExpr) | // already P[ParseFnResult]
        numberLiteral | // P[ParseFnResult]
        stringLiteral | // P[ParseFnResult]
        ("(" ~/ valueExpr ~ ")").flatMap {
          case Right(expr) => methodChain(expr, valueExpr) // attach trailing .methods to parenthesized expr
          case left@Left(_) => P(Pass(left))
        }
    )

  // support unary minus: -x
  private def unaryMinus[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P("-" ~/ WS0 ~ arithmeticAtom).map {
      case Right(fn) => Right(NegateFn(fn))
      case Left(err) => Left(err)
    } | arithmeticAtom

  // ---- String Concat ----

  private def concatExpr[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(
      arithmeticExpr ~
        (WS0 ~ "::" ~ WS0 ~ arithmeticExpr).rep ~
        &(WS0 ~ !CharIn("=<>!")) // Lookahead to reject comparisons
    ).map { case (firstE, restE) =>
      val all: List[ParseFnResult] = firstE :: restE.toList

      val (errs, oks) = all.partitionMap(identity)
      if errs.nonEmpty then
        Left(errs.head) // propagate first error
      else {
        val fns: List[Fn[Any]] = oks
        if fns.lengthCompare(1) == 0 then
          Right(fns.head) // a :: nothing => just the first
        else
          Right(ConcatFn(fns)) // a :: b :: c ...
      }
    }

  // ---- valueExpr => Top-Level Expr ----

  def valueExpr[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(
      ifFn |
        blockFn | // optional block expression
        concatExpr | // includes arithmeticExpr -> baseExpr (path+methods)
        booleanExpr.map {
          case Right(b) => Right(b.asInstanceOf[Fn[Any]]) // widen BooleanFn -> Fn[Any]
          case Left(err) => Left(err)
        }
    )

  private def blockFn[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(
      "{" ~/ WS0 ~
        statement.rep(sep = WS0) ~ // Seq[ParseStmtResult]
        valueExpr ~ // ParseFnResult
        WS0 ~ "}"
    ).map { case (stmtResults, valueRes) =>
      // propagate first statement error, if any
      stmtResults.collectFirst { case Left(e) => e } match
        case Some(err) => Left(err)
        case None =>
          valueRes match
            case Left(err) => Left(err)
            case Right(resultFn) =>
              val stmts = stmtResults.collect { case Right(s) => s }.toList
              Right(BlockFn(stmts, resultFn): Fn[Any]) // widen to Fn[Any]
    }

  // block { ... } as a *statement* block
  private def blockStmt[$: P](using ctx: ExprContext): P[ParseStmtResult] =
    P("{" ~/ WS0 ~ statement.rep(sep = WS0) ~ WS0 ~ "}").map { stmtResults =>
      // bubble the first error, if any
      stmtResults.collectFirst { case Left(e) => e } match
        case Some(err) => Left(err)
        case None =>
          val stmts = stmtResults.collect { case Right(s) => s }.toList
          Right(BlockStmt(stmts))
    }

  // A single statement
  def statement[$: P](using ctx: ExprContext): P[ParseStmtResult] =
    P(
      WS0 ~ (
        valDecl |
        updateOrMapStmt() |
        ifStmt |
        blockStmt |
        collectionStmt(booleanExpr)
      ) ~ WS0
    )

  // val x = <expr>
  private def valDecl[$: P](using ctx: ExprContext): P[ParseStmtResult] =
    P("val" ~/ WS ~ identifier.! ~ WS0 ~ "=" ~ WS0 ~ valueExpr).map {
      case (name, Right(vfn)) => Right(ValStmt(name, vfn))
      case (_, Left(err)) => Left(err)
    }

  private val pattern = """LengthFn\(GetFn\(\w+\[\]""".r

  private def updateOrMapStmt[$: P]()(using ctx: ExprContext): P[ParseStmtResult] =
    P(Index ~ path.map(_._1) ~ WS0 ~ "=" ~/ WS0 ~ valueExpr).map {
      case (offset, p, vres) =>
        vres match
          case Left(err) => Left(err)

          case Right(v) =>
            if p.contains("[]") then
              // warn user if they try to use foo[].len() in predicate--won't work!
              if pattern.findFirstIn(v.toString).isDefined then
                Left(
                  DLCompileError(
                    offset,
                    "Sorry...we don't support len() function on collections in a predicate (LHS).\nConsider using an intermediate val"
                  )
                )
              else
                Right(MapStmt(p, v))
            else
              Right(UpdateStmt(p, v))
//              v match
//                // keep your raw-get optimization
//                case g: GetFn if g.elseValue.isEmpty =>
//                  Right(UpdateStmt(p, g.copy(useRawValue = true)))
//                case _ =>
//                  Right(UpdateStmt(p, v))
    }

  private def ifStmt[$: P](using ctx: ExprContext): P[ParseStmtResult] =
    P(
      "if" ~/ WS ~ booleanExpr ~ WS0 ~
        "then" ~ WS0 ~ statement ~
        (WS0 ~ "else" ~ WS0 ~ statement).?
    ).map {
      case (condRes, thenRes, elseOptRes) =>
        val base: Either[DLCompileError, (BooleanFn, Statement)] =
          for
            c <- condRes
            t <- thenRes
          yield (c, t)

        elseOptRes match
          case None =>
            base.map { case (c, t) => IfStmt(c, t, None) }

          case Some(er) =>
            for
              (c, t) <- base
              e <- er
            yield IfStmt(c, t, Some(e))
    }

  private def ifFn[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(
      "if" ~/ WS ~ booleanExpr ~ WS0 ~
        "then" ~/ WS0 ~ valueExpr ~ WS0 ~
        "else" ~/ WS0 ~ valueExpr
    ).map {
      case (condRes, thenRes, elseRes) =>
        for
          c <- condRes
          t <- thenRes
          e <- elseRes
        yield IfFn(c, t, e)
    }