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
// First level:
//     path
//     functions
//     collection statements
//
trait Level1 extends Level0:

  // Simple symbol or dotted path, possibly with array notation
  //   foo.bar
  //   foo[].bar
  //   foo[3].bar

  def path[$: P]: P[(String, Option[String])] = {

    def fullPath[$: P]: P[String] =
      P(CharsWhileIn("a-zA-Z0-9_.[]").rep(1).!)

    // Lookahead to see if the *last identifier* is followed by a `(`
    def isFunc[$: P]: P[Boolean] =
      P(
        &("(").map(_ => true)
          | Pass.map(_ => false)
      )

    P(fullPath ~ isFunc).flatMap { case (raw, hasFunc) =>
      if hasFunc then
        val lastDot = raw.lastIndexOf('.')
        if lastDot == -1 then
          // This is like `foo()` — illegal in path context
          Fail.opaque("Function call detected where path was expected")
        else {
          val base = raw.take(lastDot)
          val fn = raw.drop(lastDot + 1)
          Pass((base, Some(fn)))
        }
      else Pass((raw, None))
    }
  }

  private def pathFn[$: P](valueExpr: => P[Fn[Any]])(using ctx: ExprContext): P[Fn[Any]] =
    P(path).flatMap { case (rawPath, maybeMethod) =>
      val baseFn = GetFn(rawPath, searchThis = ctx.searchThis)

      maybeMethod match
        case None =>
          P(Pass(baseFn))

        case Some("else") =>
          // This expects the format: `.else(...)`
          P("(" ~ valueExpr ~ ")").map { elseFn =>
            baseFn.copy(elseValue = Some(elseFn))
          }

        case Some("isDefined") =>
          // This expects the format: `.else(...)`
          P("(" ~ WS0 ~ ")").map { _ =>
            baseFn.copy(isDefined = true)
          }

        case Some(methodName) =>
          methodFunctions.get(methodName) match
            case Some(fnBuilder) =>
              P(methodArgs(valueExpr).map(args => fnBuilder(baseFn, args)))
            case None =>
              throw new RuntimeException(s"Unknown method: $methodName")
    }

  private def methodArgs[$: P](valueExpr: => P[Fn[Any]]): P[List[Fn[Any]]] =
    P("(" ~ valueExpr.rep(sep = "," ~ WS0) ~ ")" ~ WS0).map(_.toList)

  def methodCall[$: P](valueExpr: => P[Fn[Any]]): P[(String, List[Fn[Any]])] =
    P(WS0 ~ "." ~ identifier.! ~ methodArgs(valueExpr))
      .map { case (name, args) => (name, args.toList) }

  def methodChain[$: P](base: Fn[Any], valueExpr: => P[Fn[Any]]): P[Fn[Any]] =
    P(WS0 ~ methodCall(valueExpr).rep).map { chain =>
      chain.foldLeft(base) { case (inner, (fnName, args)) =>
        methodFunctions.get(fnName) match
          case Some(fnBuilder) => fnBuilder(inner, args).asInstanceOf[Fn[Any]]
          case None => throw new RuntimeException(s"Unknown method: $fnName")
      }
    }

  def baseExpr[$: P](valueExpr: => P[Fn[Any]])(using ctx: ExprContext): P[Fn[Any]] =
    P((standaloneFn | constant | pathFn(valueExpr)).flatMap(base => methodChain(base, valueExpr)) ~ WS0)

  // ---- Functions ----

  private def standaloneFn[$: P]: P[Fn[Any]] =
    P(
      StringIn("now", "uuid").! ~ "(" ~ WS0 ~ ")"
    ).map {
      case "now"  => NowFn
      case "uuid" => UUIDFn
    }

  private def checkArgs(fnName: String, args: List[Fn[Any]], required: Int): Unit =
    if args.length != required then throw new RuntimeException(s"Function $fnName() expected $required argument(s), got ${args.length}")

  private val methodFunctions: Map[String, (Fn[Any], List[Fn[Any]]) => Fn[?]] = Map(
    "startsWith" -> { (recv, args) =>
      checkArgs("startsWith", args, 1)
      StartsWithFn(recv.as[String], args.head.as[String])
    },
    "endsWith" -> { (recv, args) =>
      checkArgs("endsWith", args, 1)
      EndsWithFn(recv.as[String], args.head.as[String])
    },
    "contains" -> { (recv, args) =>
      checkArgs("contains", args, 1)
      ContainsFn(recv.as[String], args.head.as[String])
    },
    "equalsIgnoreCase" -> { (recv, args) =>
      checkArgs("equalsIgnoreCase", args, 1)
      EqualsIgnoreCaseFn(recv.as[String], args.head.as[String])
    },
    "matchesRegex" -> { (recv, args) =>
      checkArgs("matchesRegex", args, 1)
      MatchesRegexFn(recv.as[String], args.head.as[String])
    },
    "isDefined" -> { (recv, _) =>
      IsDefinedFn(recv)
    },
    "len" -> { (recv, _) =>
      LengthFn(recv)
    },
    "toUpperCase" -> { (recv, _) =>
      ToUpperFn(recv)
    },
    "toLowerCase" -> { (recv, _) =>
      ToLowerFn(recv)
    },
    "trim" -> { (recv, _) =>
      TrimFn(recv)
    },
    "template" -> { (recv, _) =>
      val varMap = recv match
        case ConstantFn(s: String) =>
          TemplateUtils.extractVariables(s).map(v => v -> GetFn(v)).toMap
        case _ =>
          Map.empty[String, Fn[Any]] // template is not a constant, so defer resolution
      InterpolateFn(recv, varMap)
    },
    "substr" -> { (recv, args) =>
      if args.isEmpty then throw new RuntimeException("substr() requires at least 1 argument")
      val start = args.head.as[Int]
      val endOpt = args.lift(1).map(_.as[Int])
      SubstringFn(recv, start, endOpt)
    },
    "replace" -> { (recv, args) =>
      checkArgs("replace", args, 2)
      ReplaceFn(recv, args.head, args(1))
    },
    "dateFmt" -> { (recv, args) =>
      checkArgs("dateFmt", args, 1)
      FormatDateFn(recv, args.head.as[String])
    },
    "toDate" -> { (recv, args) =>
      checkArgs("toDate", args, 1)
      ParseDateFn(recv, args.head.as[String])
    }
  )

  // ---- Collection Statements ----

  private trait CollectionMethodParser {
    def name: String
    def parseFn[$: P](inner: Fn[Any]): P[Fn[Any]]
  }

  def collectionStmt[$: P](booleanExpr: ExprContext ?=> P[BooleanFn]): P[Statement] =

    def simpleFieldPath[$: P]: P[String] =
      P(CharsWhileIn("a-zA-Z0-9_.").!)

    case object SortAscMethod extends CollectionMethodParser {
      val name = "sortAsc"
      def parseFn[$: P](inner: Fn[Any]): P[Fn[Any]] =
        simpleFieldPath.?.map(p => SortFn(p))
    }

    case object SortDescMethod extends CollectionMethodParser {
      val name = "sortDesc"
      def parseFn[$: P](inner: Fn[Any]): P[Fn[Any]] =
        simpleFieldPath.?.map(p => SortFn(p, false))
    }

    case object FilterMethod extends CollectionMethodParser {
      val name = "filter"
      def parseFn[$: P](inner: Fn[Any]): P[Fn[Any]] =
        booleanExpr(using ExprContext(searchThis = true)).map(v => FilterFn(toBooleanFn(v)))
    }

    case object DistinctMethod extends CollectionMethodParser {
      val name = "distinct"
      def parseFn[$: P](inner: Fn[Any]): P[Fn[Any]] =
        simpleFieldPath.?.map(p => DistinctFn(p))
    }

    case object LimitMethod extends CollectionMethodParser {
      val name = "limit"
      def parseFn[$: P](inner: Fn[Any]): P[Fn[Any]] =
        number.map(LimitFn.apply)
      private def number[$: P]: P[Int] =
        P(CharsWhileIn("0-9").!).map(_.toInt)
    }

    case object ReverseMethod extends CollectionMethodParser {
      val name = "reverse"
      def parseFn[$: P](inner: Fn[Any]): P[Fn[Any]] =
        P("(" ~ WS0 ~ ")" | Pass(())).map(_ => ReverseFn())
    }

    case object CleanMethod extends CollectionMethodParser {
      val name = "clean"
      def parseFn[$: P](inner: Fn[Any]): P[Fn[Any]] =
        P("(" ~ WS0 ~ ")" | Pass(())).map(_ => CleanFn())
    }

    case object MapToMethod extends CollectionMethodParser {
      val name = "mapTo"
      def parseFn[$: P](inner: Fn[Any]): P[Fn[Any]] =
        stringLiteral.map {
          case ConstantFn(mapName) => MapFwdFn(mapName.toString)
          case _                   => throw new RuntimeException("String-typed constant arg required for mapTo()")
        }
    }

    case object MapFromMethod extends CollectionMethodParser {
      val name = "mapFrom"
      def parseFn[$: P](inner: Fn[Any]): P[Fn[Any]] =
        stringLiteral.map {
          case ConstantFn(mapName) => MapRevFn(mapName.toString)
          case _                   => throw new RuntimeException("String-typed constant arg required for mapFrom()")
        }
    }

    val collectionMethodRegistry: Map[String, CollectionMethodParser] =
      List(SortAscMethod, SortDescMethod, FilterMethod, DistinctMethod, LimitMethod, ReverseMethod, CleanMethod, MapToMethod, MapFromMethod).map(m => m.name -> m).toMap

    P(path).flatMap {
      case (basePath, Some(firstMethod)) =>
        collectionMethodRegistry.get(firstMethod) match {
          case Some(firstParser) =>
            for {
              firstFn <- P("(" ~/ WS0 ~ firstParser.parseFn(IdentityFn) ~ WS0 ~ ")")
              restFns <- P((WS0 ~ "." ~ identifier.!).rep).flatMap { methodNames =>
                methodNames.foldLeft(Pass(List.empty[Fn[Any]]): P[List[Fn[Any]]]) { case (accP, methodName) =>
                  accP.flatMap { acc =>
                    collectionMethodRegistry.get(methodName) match {
                      case Some(parser) =>
                        P("(" ~/ WS0 ~ parser.parseFn(IdentityFn) ~ WS0 ~ ")").map(fn => acc :+ fn)
                      case None =>
                        P(Fail.opaque(s"Unknown collection method: $methodName"))
                    }
                  }
                }
              }
            } yield {
              val allFns = firstFn +: restFns
              if allFns.size == 1 then MapStmt(basePath, allFns.head)
              else MapStmt(basePath, PolyFn(allFns))
            }

          case None =>
            Fail.opaque(s"Unknown collection method: $firstMethod")
        }

      case _ =>
        Fail.opaque("Collection statement must have at least one method")
    }
