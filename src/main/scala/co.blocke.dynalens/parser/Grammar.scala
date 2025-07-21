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

import fastparse._, NoWhitespace._

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

  def literalWithMethods[$: P]: P[Fn[Any]] =
    P(stringLiteral ~ method.rep).map {
      case (str, methods) =>
        val base: Fn[Any] = ConstantFn(str)
        methodChain(base, methods)
    }

  def path[$: P]: P[String] = {
    def dotField[$: P]: P[String] =
      P(&("." ~ identifier ~ "(").!.?).flatMap {
        case Some(_) => Fail
        case None    => P("." ~/ identifier).map("." + _)
      }
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
    P(path ~ method.rep).map {
      case (basePath, methods) =>
        val base = GetFn(basePath)
        methodChain(base, methods)
    }
  def methodChain(base: Fn[Any], calls: Seq[(String, List[Fn[Any]])]): Fn[Any] =
    calls.foldLeft(base) {
      case (recv, (name, args)) =>
        stringMethods.get(name) match
          case Some(f) => f(recv, args)
          case None    => throw new RuntimeException(s"Unknown method: $name")
    }

  def argList[$: P]: P[List[Fn[Any]]] =
    P(literalExpr.rep(sep = P("," ~ WS0)).map(_.toList))
  def methodExpr[$: P]: P[Fn[Any]] =
    P(terminalExpr ~ ("." ~ identifier.! ~ "(" ~ argList ~ ")").rep).map {
      case (base, calls) => methodChain(base, calls)
    }

  def parensFn[$: P]: P[Fn[Any]] =
    P("(" ~/ expr ~ ")" ~ WS)

  def factor[$: P]: P[Fn[Any]] =
    P(ifFn | constantFn | getFn | parensFn | blockExpr)

  def literalExpr[$: P]: P[Fn[Any]] =
    P(literalWithMethods | constantFn | getFn)

  def terminalExpr[$: P]: P[Fn[Any]] =
    P(
      standaloneFnExpr |
      mapFwdFn |
      mapRevFn |
      ifFn |
      constantFn |
      getFn |
      parensFn
    )

  def leafExpr[$: P]: P[Fn[Any]] =
    P(blockExpr | methodExpr | literalWithMethods) ~ WS0

  case class ToStringFn(input: Fn[Any]) extends Fn[String]:
    def resolve(ctx: Map[String, (Any, DynaLens[?])]) =
      input.resolve(ctx).map {
        case null => ""
        case v => v.toString
      }

  def stringyExpr[$: P]: P[Fn[Any]] =
    P {
      literalWithMethods | getFn.map(g => ToStringFn(g).asInstanceOf[Fn[Any]])
    }

  def stringConcat[$: P]: P[Fn[Any]] =
    P(valueExpr.rep(min = 1, sep = WS0 ~ "::" ~ WS0)).map {
      case single :: Nil => single
      case many          => ConcatFn(many.map(_.asInstanceOf[Fn[Any]]).toList).asInstanceOf[Fn[Any]]
    }

  def expr[$: P]: P[Fn[Any]] = P(stringConcat)

  def filterStmt[$: P]: P[Statement] =
    P(path ~ ".filter(" ~/ boolExpr ~ ")").map {
      case (p, predicate) =>
        MapStmt(p, FilterFn(predicate))
    }

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
    P(WS.? ~ (updateOrMapStmt | filterStmt | ifStmt | valStmt | blockStmt) ~ WS.?)

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

  def mapFwdFn[$: P]: P[Fn[Any]] =
    P("mapFwd(" ~/ stringLiteral.map(MapFwdFn.apply) ~ ")")

  def mapRevFn[$: P]: P[Fn[Any]] =
    P("mapRev(" ~/ stringLiteral.map(MapRevFn.apply) ~ ")")

  def method[$: P]: P[(String, List[Fn[Any]])] =
    P("." ~/ identifier ~ "(" ~/ expr.rep(sep = ","./) ~ ")")
      .map { case (name, args) => (name, args.toList) }

  val stringMethods: Map[String, (Fn[Any], List[Fn[Any]]) => Fn[Any]] = Map(
    "startsWith" -> { (recv, args) =>
      args.headOption match {
        case Some(a) => StartsWithFn(recv.asInstanceOf[Fn[String]], a.asInstanceOf[Fn[String]]).asInstanceOf[Fn[Any]]
        case None => throw new RuntimeException("startsWith() requires two String arguments")
      }
    },
    "endsWith" -> { (recv, args) =>
      args.headOption match {
        case Some(a) => EndsWithFn(recv.asInstanceOf[Fn[String]], a.asInstanceOf[Fn[String]]).asInstanceOf[Fn[Any]]
        case None => throw new RuntimeException("endsWith() requires two String arguments")
      }
    },
    "contains" -> { (recv, args) =>
      args.headOption match {
        case Some(a) => ContainsFn(recv.asInstanceOf[Fn[String]], a.asInstanceOf[Fn[String]]).asInstanceOf[Fn[Any]]
        case None => throw new RuntimeException("contains() requires two String arguments")
      }
    },
    "equalsIgnoreCase" -> { (recv, args) =>
      args.headOption match {
        case Some(a) => EqualsIgnoreCaseFn(recv.asInstanceOf[Fn[String]], a.asInstanceOf[Fn[String]]).asInstanceOf[Fn[Any]]
        case None => throw new RuntimeException("equalsIgnoreCase() requires two String arguments")
      }
    },
    "matchesRegex" -> { (recv, args) =>
      args.headOption match {
        case Some(a) => MatchesRegexFn(recv.asInstanceOf[Fn[String]], a.asInstanceOf[Fn[String]]).asInstanceOf[Fn[Any]]
        case None => throw new RuntimeException("matchesRegex() requires two String arguments")
      }
    },
    "len"         -> { (recv, _) => LengthFn( recv ).asInstanceOf[Fn[Any]] },
    "toUpperCase" -> { (recv, _) => ToUpperFn( recv ).asInstanceOf[Fn[Any]] },
    "toLowerCase" -> { (recv, _) => ToLowerFn( recv ).asInstanceOf[Fn[Any]] },
    "trim"        -> { (recv, _) => TrimFn( recv ).asInstanceOf[Fn[Any]] },
    "template"    -> { (recv, _) =>
      val varMap = recv match
        case ConstantFn(s: String) =>
          TemplateUtils.extractVariables(s).map(v => v -> GetFn(v)).toMap
        case _ =>
          Map.empty[String, Fn[Any]] // template is not a constant, so defer resolution
      InterpolateFn(recv, varMap).asInstanceOf[Fn[Any]]
    },
    "substr"      -> { (recv, args) =>
      args.headOption match {
        case Some(a) => SubstringFn(recv, a.asInstanceOf[Fn[Int]], args.lift(1).map(_.asInstanceOf[Fn[Int]])).asInstanceOf[Fn[Any]]
        case None => throw new RuntimeException("substr() requires at least 1 Int argument")
      }
    },
    "replace"     -> { (recv, args) =>
      if args.length != 2 then
        throw new RuntimeException("replace() requires 2 arguments")
      ReplaceFn(recv, args(0), args(1)).asInstanceOf[Fn[Any]]
    },
    "dateFmt" -> { (recv, args) =>
      args.headOption match {
        case Some(fmtFn) =>
          FormatDateFn(recv, fmtFn.asInstanceOf[Fn[String]]) // recv should be Fn[java.util.Date]
        case None => throw new RuntimeException("format() requires a pattern argument")
      }
    },
    "toDate" -> { (recv, args) =>
      args.headOption match {
        case Some(fmtFn) =>
          ParseDateFn(recv, fmtFn.asInstanceOf[Fn[String]]) // recv is string, returns Fn[Date]
        case None => throw new RuntimeException("toDate() requires a pattern argument")
      }
    }
  )

  val dateMethods: Map[String, (Fn[Any], List[Fn[Any]]) => Fn[Any]] = Map(
    "dateFmt" -> { (recv, args) =>
      args match {
        case fmt :: Nil =>
          FormatDateFn(recv, fmt.asInstanceOf[Fn[String]])
        case _ =>
          throw new RuntimeException("dateFmt() requires exactly one string argument")
      }
    }
  )

  def standaloneFnExpr[$: P]: P[Fn[Any]] =
    P(StringIn("now", "uuid").! ~ "()").map { fnName =>
      standaloneFns(fnName)
    }

  val standaloneFns: Map[String, Fn[Any]] = Map(
    "now" -> NowFn,
    "uuid" -> UUIDFn
  )
}
