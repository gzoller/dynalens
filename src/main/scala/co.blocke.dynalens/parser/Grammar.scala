package co.blocke.dynalens
package parser

import fastparse._, NoWhitespace._

object Grammar {

  def WS[$: P]: P[Unit] = P(CharsWhileIn(" \n\r\t").rep)

  def identifier[$: P]: P[String] =
    P(CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_").?).! ~ WS.?

  def number[$: P]: P[String] =
    P(CharsWhileIn("0-9")).! ~ WS.?

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

  def constantFn[$: P]: P[Fn[Any]] =
    P(number.map(n => ConstantFn(n.toInt)) | stringLiteral.map(ConstantFn(_))) ~ WS.?

  def getFn[$: P]: P[Fn[Any]] =
    P(path).map(GetFn(_))

  def parensFn[$: P]: P[Fn[Any]] =
    P("(" ~/ expr ~ ")" ~ WS)

  def factor[$: P]: P[Fn[Any]] =
    P(constantFn | getFn | parensFn | blockExpr)

  def mulDiv[$: P]: P[Fn[Any]] =
    P(factor ~ (WS.? ~ CharIn("*\\/").! ~/ WS.? ~ factor).rep).map {
      case (left, rest) =>
        rest.foldLeft(left) {
          case (l, ("*", r)) => MultiplyFn(l, r)
          case (l, ("/", r)) => DivideFn(l, r)
          case (l, (op, _)) => throw new RuntimeException(s"Unsupported operator: $op")
        }
    }

  def addSub[$: P]: P[Fn[Any]] =
    P(mulDiv ~ (WS.? ~ CharIn("+\\-").! ~/ WS.? ~ mulDiv).rep).map {
      case (left, rest) =>
        rest.foldLeft(left) {
          case (l, ("+", r)) => AddFn(l, r)
          case (l, ("-", r)) => SubtractFn(l, r)
          case (l, (op, _)) => throw new RuntimeException(s"Unsupported operator: $op")
        }
    }

  def expr[$: P]: P[Fn[Any]] =
    P(addSub)

  def updateOrMapStmt[$: P]: P[Statement] =
    P(path ~ "=" ~ WS ~ expr).map {
      case (p, v) =>
        if p.contains("[]") then MapStmt(p, v) else UpdateStmt(p, v)
    }

  def valStmt[$: P]: P[Statement] =
    P("val" ~ WS ~ identifier ~ WS.? ~ "=" ~ WS.? ~ expr).map {
      case (name, value) => ValStmt(name, value)
    }

//  def mapStmt[$: P]: P[Statement] =
//    P("map" ~ WS ~ "(" ~ WS ~ path ~ "," ~ WS ~ expr ~ WS ~ ")").map {
//      case (p, f) => MapStmt(p, f)
//    }

  def stmt[$: P]: P[Statement] =
    P(WS.? ~ (valStmt | updateOrMapStmt) ~ WS.?)

  def blockExpr[$: P]: P[Fn[Any]] =
    P("{" ~/ WS.? ~ stmt.rep ~ expr ~ WS.? ~ "}").map {
      case (stmts, finalFn) =>
        BlockFn(stmts.toList, finalFn)
    }

  def block[$: P]: P[BlockStmt] =
    P(WS.? ~ stmt.rep ~ WS.?).map(stmts => BlockStmt(stmts.toList))
}