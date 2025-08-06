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

  private def pathFn[$: P](valueExpr: => P[ParseFnResult])(using ctx: ExprContext): P[ParseFnResult] =
    P(path).flatMap { case (rawPath, maybeMethod) =>
      val baseFn = GetFn(rawPath, searchThis = ctx.searchThis)

      maybeMethod match
        case None =>
          P(Pass(Right(baseFn)))

        case Some(methodName) =>
          methodFunctions.get(methodName) match
            case Some(fnBuilder) =>
              methodArgs(valueExpr).flatMap {
                case Right(args) =>
                  P(Pass(Right(fnBuilder(baseFn, args))))
                case Left(err) =>
                  P(Pass(Left(err)))
              }
            case None =>
              val offset = implicitly[ParsingRun[?]].index
              P(Pass(Left(DLCompileError(offset, s"Unknown method: $methodName"))))
    }

  // Parse a method's argument list, with error propagation
  private def methodArgs[$: P](valueExpr: => P[ParseFnResult]): P[Either[DLCompileError, List[Fn[Any]]]] =
    P("(" ~ valueExpr.rep(sep = "," ~ WS0) ~ ")" ~ WS0).map { results =>
      val (errs, oks) = results.partitionMap(identity)
      if errs.nonEmpty then Left(errs.head)
      else Right(oks.toList)
    }

  // Parse a full method call: `.foo(arg1, arg2)`
  private def methodCall[$: P](valueExpr: => P[ParseFnResult]): P[Either[DLCompileError, (String, List[Fn[Any]])]] =
    P(WS0 ~ "." ~ identifier.! ~ methodArgs(valueExpr)).map {
      case (methodName, Right(args)) => Right((methodName, args))
      case (_, Left(err))            => Left(err)
    }

  def methodChain[$: P](base: Fn[Any], valueExpr: => P[ParseFnResult]): P[ParseFnResult] =
    P(WS0 ~ Index ~ methodCall(valueExpr).rep).map {
      case (offset, chain) =>
        val (errs, oks) = chain.partitionMap(identity)

        if errs.nonEmpty then
          Left(errs.head)
        else
          oks.foldLeft[ParseFnResult](Right(base)) {
            case (Right(inner), (fnName, args)) =>
              methodFunctions.get(fnName) match
                case Some(fnBuilder) =>
                  Right(fnBuilder(inner, args).asInstanceOf[Fn[Any]])
                case None =>
                  Left(DLCompileError(offset, s"Unknown method: $fnName"))

            case (err@Left(_), _) => err
          }
    }

  def baseExpr[$: P](valueExpr: => P[ParseFnResult])(using ctx: ExprContext): P[ParseFnResult] =
    P((standaloneFn.map(Right(_)) | constant | pathFn(valueExpr)).flatMap {
      case Right(fn) => methodChain(fn, valueExpr)
      case err@Left(_) => P(Pass(err))
    } ~ WS0)

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
    "else" -> { (recv, args) =>
      checkArgs("else", args, 1)
      ElseFn(recv, args.head)
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
    // TODO: Promote all the collection methods as regular functions too (predicates)
  )

  // ---- Collection Statements ----

  private trait CollectionMethodParser {
    def name: String
    def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult]
  }

  def collectionStmt[$: P](booleanExpr: ExprContext ?=> P[ParseBoolResult])
                          (using ctx: ExprContext): P[ParseStmtResult] = {

    def simpleFieldPath[$: P]: P[String] =
      P(CharsWhileIn("a-zA-Z0-9_.").!)

    case object SortAscMethod extends CollectionMethodParser {
      val name = "sortAsc"
      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        P(Index ~ simpleFieldPath.?).map { case (offset, maybeField) =>
          // No validation needed: field is optional by design
          Right(SortFn(maybeField)) // ascending by default
        }
    }

    case object SortDescMethod extends CollectionMethodParser {
      val name = "sortDesc"
      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        P(Index ~ simpleFieldPath.?).map { case (offset, maybeField) =>
          Right(SortFn(maybeField, asc = false))
        }
    }

    case object FilterMethod extends CollectionMethodParser {
      val name = "filter"

      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        booleanExpr(using summon[ExprContext].copy(searchThis = true)).map {
          case Right(pred: BooleanFn) => Right(FilterFn(pred): Fn[Any])
          case Left(err: DLCompileError) => Left(err)
        }
    }

    case object DistinctMethod extends CollectionMethodParser {
      val name = "distinct"
      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        P(Index ~ simpleFieldPath.?).map { case (offset, maybeField) =>
          Right(DistinctFn(maybeField))
        }
    }

    case object LimitMethod extends CollectionMethodParser {
      val name = "limit"

      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        P(Index ~ number).map {
          case (_, n) if n >= 0 => Right(LimitFn(n))
          case (offset, n) => Left(DLCompileError(offset, s"limit(...) requires a non-negative integer, found $n"))
        }

      private def number[$: P]: P[Int] =
        P(CharsWhileIn("0-9", min = 1).!).map(_.toInt)
    }

    case object ReverseMethod extends CollectionMethodParser {
      val name = "reverse"
      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        P(Index ~ (("(" ~ WS0 ~ ")").?)).map { _ =>
          Right(ReverseFn())
        }
    }

    case object CleanMethod extends CollectionMethodParser {
      val name = "clean"
      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        P(Index ~ (("(" ~ WS0 ~ ")").?)).map { _ =>
          Right(CleanFn())
        }
    }

    case object MapToMethod extends CollectionMethodParser {
      val name = "mapTo"
      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        // We capture the current offset for good error messages
        P(Index ~ stringLiteral).map { case (idx, strRes) =>
          strRes match {
            case Right(ConstantFn(mapName: String)) =>
              Right(MapFwdFn(mapName)) // success: build the function

            case Right(_) =>
              Left(DLCompileError(idx, """mapTo(...) requires a string literal, e.g. mapTo("target")"""))

            case Left(err) =>
              Left(err) // bubble up any error produced by stringLiteral
          }
        }
    }

    case object MapFromMethod extends CollectionMethodParser {
      val name = "mapFrom"
      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        P(Index ~ stringLiteral).map {
          case (offset, Right(ConstantFn(mapName: String))) =>
            Right(MapRevFn(mapName))
          case (offset, Right(_)) =>
            Left(DLCompileError(offset, "mapFrom(...) requires a string literal argument"))
          case (_, Left(err)) =>
            Left(err) // propagate any error produced by stringLiteral
        }
    }

    val collectionMethodRegistry: Map[String, CollectionMethodParser] =
      List(SortAscMethod, SortDescMethod, FilterMethod, DistinctMethod, LimitMethod, ReverseMethod, CleanMethod, MapToMethod, MapFromMethod)
        .map(m => m.name -> m).toMap

    def parseMethodArgs[$: P](parser: CollectionMethodParser)
                             (using ExprContext): P[ParseFnResult] =
      P("(" ~/ WS0 ~ parser.parseFn(IdentityFn) ~ WS0 ~ ")")

    def lookupMethod[$: P](name: String): P[Either[DLCompileError, CollectionMethodParser]] =
      P(Index).map { off =>
        collectionMethodRegistry.get(name)
          .toRight(DLCompileError(off, s"Unknown collection method: $name"))
      }

    // Parse: path followed by first method, then zero+ of ".method"
    P(Index ~ path).flatMap { case (off0, (basePath, maybeFirstMethod)) =>
      maybeFirstMethod match {
        case Some(firstMethodName) =>
          for {
            // Resolve and parse the first method’s args
            firstResolved <- lookupMethod(firstMethodName)
            firstResult <- firstResolved match {
              case Left(err) => P(Pass(Left(err)))
              case Right(par) => parseMethodArgs(par) // ParseFnResult
            }

            // Zero or more ".identifier"
            methodNames <- P((WS0 ~ "." ~ identifier.!).rep)

            // Resolve & parse each subsequent "(...)" and collect
            restFns <- methodNames.foldLeft(Pass(Right(Nil): ParseFnListResult): P[ParseFnListResult]) {
              case (accP, methodName) =>
                accP.flatMap {
                  case left@Left(_) => P(Pass(left)) // keep first error
                  case Right(accum) =>
                    for {
                      resolved <- lookupMethod(methodName)
                      fnRes <- resolved match {
                        case Left(err) => P(Pass(Left(err)))
                        case Right(par) => parseMethodArgs(par)
                      }
                    } yield fnRes match {
                      case Left(err) => Left(err)
                      case Right(fn) => Right(accum :+ fn)
                    }
                }
            }
          } yield {
            // Combine into a single Fn, then into a MapStmt, and wrap as ParseStmtResult
            (for {
              f1 <- firstResult
              rxs <- restFns
            } yield {
              val all = f1 :: rxs
              val fn = if all.size == 1 then all.head else PolyFn(all)
              MapStmt(basePath, fn): Statement
            }): ParseStmtResult
          }

        case None =>
          P(Fail)
//          P(Pass(Left(DLCompileError(off0, "Collection statement must have at least one method"))))
      }
    }
  }