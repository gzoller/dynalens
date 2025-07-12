package co.blocke.dynalens
package parser

import fastparse._, NoWhitespace._
import fastparse.Parsed.{Success, Failure}

object Grammar {

  def WS[$: P]: P[Unit] = P((CharsWhileIn(" \n\r\t") | comment).rep(1))  // WS required
  def WS0[$: P]: P[Unit] = P((CharsWhileIn(" \n\r\t") | comment).rep)    // WS optional

  def identifier[$: P]: P[String] =
    P(CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_").?).! ~ WS.?

  def number[$: P]: P[Fn[Any]] =
    P(CharIn("+\\-").? ~ CharsWhileIn("0-9") ~ ("." ~ CharsWhileIn("0-9")).?).!.map { s =>
      val trimmed = s.trim
      if trimmed.contains(".") then
        try ConstantFn(trimmed.toDouble)
        catch case _: NumberFormatException => throw new RuntimeException(s"Invalid double: $trimmed")
      else
        try ConstantFn(trimmed.toInt)
        catch case _: NumberFormatException =>
          try ConstantFn(trimmed.toLong)
          catch case _: NumberFormatException => throw new RuntimeException(s"Invalid number: $trimmed")
    } ~ WS.?

  def stringLiteral[$: P]: P[String] =
    P("\"" ~/ CharsWhile(_ != '"').! ~ "\"") ~ WS.?

  def path[$: P]: P[String] = {
    def dotField = P("." ~/ identifier).map("." + _)
    def arrayIndex = P("[" ~/ CharsWhileIn("0-9").! ~ "]").map("[" + _ + "]")
    def arrayAll = P("[" ~ "]").map(_ => "[]")
    def segment = P(arrayAll | arrayIndex | dotField)
    P(identifier ~ segment.rep(min = 0)).map {
      case (head, tail) => tail.foldLeft(head)(_ + _)
    }
  }

  def booleanLiteral[$: P]: P[Fn[Any]] =
    P(StringIn("true", "false").!).map {
      case "true" => ConstantFn(true)
      case "false" => ConstantFn(false)
    } ~ WS.?

  def constantFn[$: P]: P[Fn[Any]] =
    P(booleanLiteral | number | stringLiteral.map(ConstantFn(_))) ~ WS.?

  def ifFn[$: P]: P[Fn[Any]] =
    P("if" ~ WS ~ boolExpr ~ WS0 ~ "then" ~ WS0 ~ (expr | blockExpr) ~ (WS0 ~ "else" ~ WS0 ~ (expr | blockExpr))).map {
      case (cond, thenBranch, elseBranch) =>
        IfFn(cond, thenBranch, elseBranch)
    }

  def ifStmt[$: P]: P[Statement] =
    P("if" ~ WS ~ boolExpr ~ WS0 ~ "then" ~ WS0 ~ (stmt | blockStmt) ~ (WS0 ~ "else" ~ WS0 ~ (stmt | blockStmt)).?).map {
      case (cond, thenBranch, maybeElse) =>
        IfStmt(cond, thenBranch, maybeElse)
    }

  def getFn[$: P]: P[Fn[Any]] =
    P(path).map(GetFn(_))

  def parensFn[$: P]: P[Fn[Any]] =
    P("(" ~/ expr ~ ")" ~ WS)

  def factor[$: P]: P[Fn[Any]] =
    P(ifFn | constantFn | getFn | parensFn | blockExpr)

  def leafExpr[$: P]: P[Fn[Any]] =
    P(blockExpr | ifFn | constantFn | getFn | parensFn) ~ WS0

  def mulDiv[$: P]: P[Fn[Any]] =
    P(leafExpr ~ (WS0 ~ CharIn("*\\/").! ~ WS0 ~ leafExpr).rep).map {
      case (left, rest) =>
        rest.foldLeft(left) {
          case (l, ("*", r)) => MultiplyFn(l, r)
          case (l, ("/", r)) => DivideFn(l, r)
          case (l, (op, _)) => throw new RuntimeException(s"Unsupported operator: $op")
        }
    } ~ WS0

  def addSub[$: P]: P[Fn[Any]] =
    P(mulDiv ~ (WS0 ~ CharIn("+\\-").! ~ WS0 ~ mulDiv).rep).map {
      case (left, rest) =>
        rest.foldLeft(left) {
          case (l, ("+", r)) => AddFn(l, r)
          case (l, ("-", r)) => SubtractFn(l, r)
          case (l, (op, _)) => throw new RuntimeException(s"Unsupported operator: $op")
        }
    } ~ WS0

  def valueExpr[$: P]: P[Fn[Any]] = addSub

  def compOp[$: P]: P[String] =
    P(StringIn("==", "!=", ">=", "<=", ">", "<")).! ~ WS0

  def boolAtom[$: P]: P[Fn[Boolean]] =
    P(
      "(" ~ WS0 ~ boolExpr ~ WS0 ~ ")" |
        (valueExpr ~ compOp ~ valueExpr).map {
          case (l, "==", r) => EqualFn(l, r)
          case (l, "!=", r) => NotEqualFn(l, r)
          case (l, "<", r) => LessThanFn(l, r)
          case (l, ">", r) => GreaterThanFn(l, r)
          case (l, "<=", r) => LessThanOrEqualFn(l, r)
          case (l, ">=", r) => GreaterThanOrEqualFn(l, r)
          case (l, op, r) => throw new RuntimeException(s"Unsupported comparator: $op")
        } |
        constantFn.collect { case ConstantFn(b: Boolean) => ConstantFn(b) } | // naked boolean constant
        getFn.map(_.asInstanceOf[Fn[Boolean]])
    ) ~ WS0

  def notExpr[$: P]: P[Fn[Boolean]] =
    P("!" ~ WS0 ~ notExpr).map(NotFn(_)) | boolAtom

  def andExpr[$: P]: P[Fn[Boolean]] =
    P(notExpr ~ (WS0 ~ "&&" ~ WS0 ~ notExpr).rep).map {
      case (first, rest) => rest.foldLeft(first)(AndFn(_, _))
    }

  def orExpr[$: P]: P[Fn[Boolean]] =
    P(andExpr ~ (WS0 ~ "||" ~ WS0 ~ andExpr).rep).map {
      case (first, rest) => rest.foldLeft(first)(OrFn(_, _))
    }

  def boolExpr[$: P]: P[Fn[Boolean]] = orExpr

  def expr[$: P]: P[Fn[Any]] = valueExpr

  def updateOrMapStmt[$: P]: P[Statement] =
    P(path ~ "=" ~ WS ~ expr).map {
      case (p, v) =>
        if p.contains("[]") then MapStmt(p, v) else UpdateStmt(p, v)
    }

  def valStmt[$: P]: P[Statement] =
    P("val" ~ WS ~ identifier ~ WS.? ~ "=" ~ WS.? ~ (expr | boolExpr)).map {
      case (name, value) => ValStmt(name, value)
    }

  def stmt[$: P]: P[Statement] =
    P(WS.? ~ (ifStmt | valStmt | updateOrMapStmt | blockStmt) ~ WS.?)

  def blockExpr[$: P]: P[Fn[Any]] =
    P("{" ~/ WS.? ~ stmt.rep ~ expr ~ WS.? ~ "}").map {
      case (stmts, finalFn) =>
        BlockFn(stmts.toList, finalFn)
    }

  // Explicit block with `{}` — used in nested places
  def blockStmt[$: P]: P[BlockStmt] =
    P("{" ~/ WS.? ~ stmt.rep ~ WS.? ~ "}").map(stmts => BlockStmt(stmts.toList))

  // Top-level script block — no `{}`, just a list of statements
  def topLevelBlock[$: P]: P[BlockStmt] =
    P(WS.? ~ stmt.rep ~ WS.?).map(stmts => BlockStmt(stmts.toList))

  def comment[$: P]: P[Unit] =
    P("#" ~ CharsWhile(_ != '\n', min = 0) ~ ("\n" | End))
}