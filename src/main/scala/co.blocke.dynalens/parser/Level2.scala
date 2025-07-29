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

  def booleanAtom[$: P]: P[BooleanFn] =
    P(
      WS0 ~ (
        "(" ~/ booleanExpr ~ ")" |
          comparisonExpr |
          booleanLiteral2 |
          arithmeticExpr.map(toBooleanFn.apply)
        )
    ).log("BOOL_ATOM")

  def booleanExpr[$: P]: P[BooleanFn] =
    P(booleanAnd ~ (WS0 ~ "||" ~ WS0 ~ booleanAnd).rep).flatMap {
      case (left, Nil) =>
        left match
          case b: BooleanFn => P(Pass(b))
          case _ => P(Fail) // soft fail to try another expression rule
      case (first, rest) =>
        P(Pass(rest.foldLeft(first)(OrFn(_, _))))
    }

  private def booleanAnd[$: P]: P[BooleanFn] =
    P(booleanNot ~ (WS0 ~ "&&" ~ WS0 ~ booleanNot).rep).map {
      case (first, rest) => rest.foldLeft(first)(AndFn(_, _))
    }

  private def booleanNot[$: P]: P[BooleanFn] =
    P("!" ~ WS0 ~ booleanNot).map(NotFn(_)) | booleanAtom

  def comparisonExpr[$: P]: P[BooleanFn] =
    P(
      arithmeticExpr ~ WS0 ~
        StringIn("==", "!=", ">=", "<=", ">", "<").! ~
        WS0 ~ arithmeticExpr
    ).map {
      case (left, "==", right) => EqualFn(left, right)
      case (left, "!=", right) => NotEqualFn(left, right)
      case (left, ">=", right) => GreaterThanOrEqualFn(left, right)
      case (left, "<=", right) => LessThanOrEqualFn(left, right)
      case (left, ">", right) => GreaterThanFn(left, right)
      case (left, "<", right) => LessThanFn(left, right)
    }

  // ---- Arithmetic ----

  def arithmeticExpr[$: P]: P[Fn[Any]] = arithmeticTerm

  private def arithmeticTerm[$: P]: P[Fn[Any]] =
    P(arithmeticFactor ~ (WS0 ~ CharIn("+\\-").! ~ WS0 ~ arithmeticFactor).rep).map {
      case (first, rest) =>
        rest.foldLeft(first) {
          case (left, ("+", right)) => AddFn(left, right)
          case (left, ("-", right)) => SubtractFn(left, right)
        }
    }

  private def arithmeticFactor[$: P]: P[Fn[Any]] =
    P(unaryMinus ~ (WS0 ~ CharIn("*/%").! ~ WS0 ~ unaryMinus).rep).map {
      case (first, rest) =>
        rest.foldLeft(first) {
          case (left, ("*", right)) => MultiplyFn(left, right)
          case (left, ("/", right)) => DivideFn(left, right)
          case (left, ("%", right)) => ModuloFn(left, right)
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
      case (first, Nil) => first
      case (first, rest) => ConcatFn(first :: rest.toList)
    }

  // ---- valueExpr => Top-Level Expr ----

//  def valueExpr[$: P]: P[Fn[Any]] =
//    P("this.qty > 4".!.map(_ => GreaterThanFn(GetFn("this.qty"), ConstantFn(4))).map{h=>println("HERE! Parsed valueExpr");h})
  def valueExpr[$: P]: P[Fn[Any]] =
    P(
      ifFn |
        blockFn |                // Optional block expression
        concatExpr |  // Includes hook into arithmeticExpr, which hooks into baseExpr (path+methods)
        booleanExpr.map(b => b: Fn[Any]) // Boolean logic safely downgraded
    )

  def blockFn[$: P]: P[BlockFn[?]] =
    P(
      "{" ~/ WS0 ~
        statement.rep(sep = WS0) ~ // zero or more val/update statements
        valueExpr ~                           // the final expression
        WS0 ~ "}"
    ).map {
      case (stmts, result) => BlockFn(stmts.toList, result)
    }

  def blockStmt[$: P]: P[BlockStmt] =
    P("{" ~/ WS0 ~ statement.rep(sep = WS0).log("BLOCK_STATEMENTS") ~ WS0 ~ "}").map(BlockStmt.apply)

  def statement[$: P]: P[Statement] =
    P(WS0 ~ (
      P(collectionStmt(valueExpr, booleanExpr).log("!COLLECTION_STMT")) |
      P(valDecl.log("!VAL")) |
      P(updateOrMapStmt.log("TRY_UPD")) |
        P(ifStmt.log("!IF")) |
        P(blockStmt.log("!BLOCK"))
      ).log("+STATEMENT") ~ WS0)
//  def statement[$: P]: P[Statement] =
//    P(WS0 ~ (valDecl | updateOrMapStmt | ifStmt.log("!IF") | blockStmt | collectionStmt(booleanExpr).log("COLLECTION_STMT")).log("STATEMENT") ~ WS0)

  def valDecl[$: P]: P[ValStmt[?]] =
    P("val" ~/ WS ~ identifier.! ~ WS0 ~ "=" ~ WS0 ~ valueExpr).map {
      case (name, value) => ValStmt(name, value)
    }

  def updateOrMapStmt[$: P]: P[Statement] =
    P(path.map(_._1) ~ WS0 ~ "=" ~/ WS0 ~ valueExpr).map {
      case (p, v) =>
        if p.contains("[]") then MapStmt(p, v) else UpdateStmt(p, v)
    }

  def ifStmt[$: P]: P[IfStmt] =
    P(
      "if" ~/ WS ~ booleanExpr.log("COND") ~ WS0 ~
        "then" ~ WS0 ~ statement.log("THEN") ~
        (WS0 ~ "else" ~ WS0 ~ statement.log("ELSE")).?
    ).log("IF_FULL").map {
      case (cond, thenPart, elseOpt) => IfStmt(cond, thenPart, elseOpt)
    }

  def ifFn[$: P]: P[Fn[Any]] =
    P(
      "if" ~/ WS ~ booleanExpr.log("COND") ~ WS0 ~
        "then" ~/ WS0 ~ valueExpr ~ WS0 ~
        "else" ~/ WS0 ~ valueExpr
    ).log("IF_FULL_2").map {
      case (cond, thenBranch, elseBranch) =>
        IfFn(cond, thenBranch, elseBranch)
    }
