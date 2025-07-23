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

  // Precedence: factor (highest), term (medium), expr (lowest)
//  private def factor[$: P](expr: => P[Fn[Any]]): P[Fn[Any]] =
//    P(parens(expr) | baseExpr(expr))
//
//  private def parens[$: P](expr: => P[Fn[Any]]): P[Fn[Any]] =
//    P("(" ~/ expr ~ ")" ~ WS0)
//
//  private def term[$: P](expr: => P[Fn[Any]]): P[Fn[Any]] =
//    P(factor(expr) ~ (CharIn("*/").! ~ WS0 ~ factor(expr)).rep).map {
//      case (left, rest) =>
//        rest.foldLeft(left) {
//          case (acc, ("*", right)) => MultiplyFn(acc, right)
//          case (acc, ("/", right)) => DivideFn(acc, right)
//        }
//    }
//
//  def arithmeticExpr[$: P](expr: => P[Fn[Any]]): P[Fn[Any]] =
//    P(term(expr) ~ (CharIn("+\\-").! ~ WS0 ~ term(expr)).rep).map {
//      case (left, rest) =>
//        rest.foldLeft(left) {
//          case (acc, ("+", right)) => AddFn(acc, right)
//          case (acc, ("-", right)) => SubtractFn(acc, right)
//        }
//    }

  def factor[$: P](expr: => P[Fn[Any]]): P[Fn[Any]] =
    P(baseExpr(expr) | ("(" ~/ expr ~ ")"))

  def term[$: P](expr: => P[Fn[Any]]): P[Fn[Any]] =
    P(factor(expr) ~ (CharIn("*/").! ~/ WS0 ~ factor(expr)).rep).map {
      case (left: Fn[Any], rest) =>
        rest.foldLeft(left) {
          case (acc: Fn[Any], ("*", right: Fn[Any])) => MultiplyFn(acc, right)
          case (acc: Fn[Any], ("/", right: Fn[Any])) => DivideFn(acc, right)
        }
    }

  def arithmeticExpr[$: P](expr: => P[Fn[Any]]): P[Fn[Any]] =
    P(term(expr) ~ (CharIn("+\\-").! ~/ WS0 ~ term(expr)).rep).map {
      case (left: Fn[Any], rest) =>
        rest.foldLeft(left) {
          case (acc: Fn[Any], ("+", right: Fn[Any])) => AddFn(acc, right)
          case (acc: Fn[Any], ("-", right: Fn[Any])) => SubtractFn(acc, right)
        }
    }

  // ---- blocks ----

  def blockFn[$: P](expr: => P[Fn[Any]]): P[Fn[Any]] =
    P("{" ~/ WS0 ~ statement(expr).rep ~ expr ~ "}").map {
        case (stmts, finalFn) =>
          BlockFn(stmts, finalFn)
      }

  private def blockStmt[$: P](expr: => P[Fn[Any]]): P[Statement] =
    P("{" ~/ statement(expr).rep ~ "}").map(BlockStmt(_))

  // ---- statements ----

  private def valDecl[$: P](expr: => P[Fn[Any]]): P[ValStmt[?]] =
    P("val" ~/ WS ~ identifier.! ~ WS0 ~ "=" ~ WS0 ~ Index ~ expr ~ Index).map {
      case (name, start, valueExpr, end) =>
        ValStmt(name, valueExpr)
    }

  private def updateOrMapStmt[$: P](expr: => P[Fn[Any]]): P[Statement] =
    P(path.! ~ WS0 ~ "=" ~/ WS0 ~ expr).map { case (p, v) =>
      if p.contains("[]") then MapStmt(p, v) else UpdateStmt(p, v)
    }

  def statement[$: P](expr: => P[Fn[Any]]): P[Statement] =
    P(WS0 ~ (ifStmt(expr) | updateOrMapStmt(expr) | valDecl(expr) | blockStmt(expr)) ~ WS0)

  // ---- if ----

  def comparisonExpr[$: P](expr: => P[Fn[Any]]): P[BooleanFn] =
    P(arithmeticExpr(expr) ~ (StringIn("==", "!=", ">=", "<=", ">", "<").! ~/ WS0 ~ arithmeticExpr(expr)).?).map {
      case (left, Some(("==", right))) => EqualFn(left, right)
      case (left, Some(("!=", right))) => NotEqualFn(left, right)
      case (left, Some((">", right))) => GreaterThanFn(left, right)
      case (left, Some(("<", right))) => LessThanFn(left, right)
      case (left, Some((">=", right))) => GreaterThanOrEqualFn(left, right)
      case (left, Some(("<=", right))) => LessThanOrEqualFn(left, right)
      case (left, None) =>
        left match
          case b: BooleanFn => b
          case other => throw new RuntimeException(s"Expected boolean expression, got: $other")
    }

  private def ensureBoolean(fn: Fn[Any], op: String): BooleanFn = fn match
    case b: BooleanFn => b
    case _ => throw new RuntimeException(s"$op requires boolean expressions")

  private def boolOrExpr[$: P](expr: => P[Fn[Any]]): P[BooleanFn] =
    P(boolAndExpr(expr) ~ ("||" ~/ WS0 ~ boolAndExpr(expr)).rep).map {
      case (left, rest) =>
        val result = rest.foldLeft(ensureBoolean(left, "||")) {
          case (acc, next) => OrFn(acc, ensureBoolean(next, "||"))
        }
        result // now seen as Fn[Any] due to +R
    }

  private def boolAndExpr[$: P](expr: => P[Fn[Any]]): P[BooleanFn] =
    P(boolNotExpr(expr) ~ ("&&" ~/ WS0 ~ boolNotExpr(expr)).rep).map {
      case (left, rest) =>
        rest.foldLeft(ensureBoolean(left, "&&")) { (acc, next) =>
          AndFn(acc, ensureBoolean(next, "&&"))
        }
    }

  private def boolNotExpr[$: P](expr: => P[Fn[Any]]): P[BooleanFn] =
    P("!" ~/ WS0.? ~ boolBaseExpr(expr)).map {
      case b: BooleanFn => NotFn(b)
      case _ => throw new RuntimeException("! operator expects a boolean expression")
    }

  private def boolBaseExpr[$: P](expr: => P[Fn[Any]]): P[BooleanFn] =
    P(
      "(" ~/ booleanExpr(expr) ~ ")" |
        comparisonExpr(expr)
    )
  /*
  private def boolNotExpr[$: P](expr: => P[Fn[Any]]): P[BooleanFn] =
    P("!" ~/ WS0.? ~ boolBaseExpr(expr)).map {
      (fn: Fn[?]) =>
        fn match {
          case b: BooleanFn => NotFn(b)
          case _ => throw new RuntimeException("! operator expects a boolean expression")
        }
    }

  private def boolBaseExpr[$: P](expr: => P[Fn[Any]]): P[BooleanFn] =
    P(
      ("(" ~/ booleanExpr(expr) ~ ")") |
        comparisonExpr(expr)
    )
    */

  def booleanExpr[$: P](expr: => P[Fn[Any]]): P[BooleanFn] =
    boolOrExpr(expr)

  private def statementOrBlock[$: P](expr: => P[Fn[Any]]): P[Statement] =
    P(blockStmt(expr) | statement(expr))

  private def ifStmt[$: P](expr: => P[Fn[Any]]): P[IfStmt] =
    P("if" ~/ WS ~ expr ~ WS0 ~ "then" ~/ WS ~ statementOrBlock(expr) ~ ("else" ~/ WS ~ statementOrBlock(expr)).?).map {
      case (condFn, thenStmt, elseOpt) =>
        condFn match
          case b: BooleanFn => IfStmt(b, thenStmt, elseOpt)
          case other => throw new RuntimeException(s"if condition must be Boolean expression, got: $other")
    }

  def ifFn[$: P](expr: => P[Fn[Any]]): P[Fn[Any]] =
    P("if" ~/ WS ~ expr ~ WS0 ~ "then" ~/ WS ~ blockFn(expr) ~ WS ~ "else" ~/ WS ~ blockFn(expr)).map {
      case (condition, thenBlock, elseBlock) =>
        condition match
          case b: BooleanFn => IfFn(b, thenBlock, elseBlock)
          case other => throw new RuntimeException(s"ifFn condition must be a Boolean expression, got: $other")
    }

    /* TODO:
       How to handle Option[]

    * Do arithmeticExprs for comparisons handle strings?  For example "foo" == "bar"?

    */