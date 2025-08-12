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

import fastparse.*
import NoWhitespace.*

import scala.annotation.tailrec


sealed trait Idx
case object Wildcard extends Idx // []
case class Fixed(i: Int) extends Idx // [3]
case class Seg(base: String, idx: Option[Idx], opt: Boolean)

private val segRx = "^([A-Za-z0-9_]+)(?:\\[(\\d*)\\])?(\\?)?$".r

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

  def path[$: P](using ctx: ExprContext): P[(String, Option[String])] = {

    def fieldName[$: P]: P[String] =
      P(CharsWhileIn("a-zA-Z0-9_").!)

    def optionalSuffix[$: P]: P[Boolean] =
      P("?".!.?).map(_.isDefined)

    def indexedWildcard[$: P]: P[Option[Int]] = P("[]").map(_ => None)

    def indexPart[$: P]: P[Option[Int]] = P("[" ~ CharsWhileIn("0-9").!.map(i => Some(i.toInt)) ~ "]")

    def segment[$: P]: P[String] =
      P(fieldName ~ (indexedWildcard | indexPart).? ~ optionalSuffix).map {
        case (name, Some(Some(i)), true) => s"$name[$i]?"
        case (name, Some(Some(i)), false) => s"$name[$i]"
        case (name, Some(None), true) => s"$name[]?"
        case (name, Some(None), false) => s"$name[]"
        case (name, None, true) => s"$name?"
        case (name, None, false) => name
      }
      
    def fullPath[$: P]: P[String] =
      P(segment.rep(1, sep = ".")).map(_.mkString("."))

    def isFunc[$: P]: P[Boolean] =
      P(&("(").map(_ => true) | Pass.map(_ => false))


    def parseSeg(name: String): Either[String, Seg] = name match
      case segRx(base, idxStr, optStr) =>
        val idx =
          if (idxStr eq null) None
          else if (idxStr.isEmpty) Some(Wildcard)
          else Some(Fixed(idxStr.toInt))
        Right(Seg(base, idx, opt = optStr != null))
      case _ =>
        Left(s"Invalid path segment '$name'")

    def render(base: String, idx: Option[Idx], opt: Boolean): String =
      val idxTxt = idx match
        case Some(Wildcard) => "[]"
        case Some(Fixed(i)) => s"[$i]"
        case None => ""
      val optTxt = if opt then "?" else ""
      s"$base$idxTxt$optTxt"

    // Force-correct suffix based on expected type, but preserve a fixed index when list-typed
    def correct(seg: Seg, expectedType: String): Either[String, String] =
      expectedType match
        case "[]" =>
          seg.idx match
            case Some(Fixed(i)) => Right(render(seg.base, Some(Fixed(i)), opt = false))
            case _ => Right(render(seg.base, Some(Wildcard), opt = false))
        case "[]?" =>
          seg.idx match
            case Some(Fixed(i)) => Right(render(seg.base, Some(Fixed(i)), opt = true))
            case _ => Right(render(seg.base, Some(Wildcard), opt = true))
        case "{}" | "{}?" | "?" | "" =>
          // Non-list types may not be indexed
          seg.idx match
            case Some(_) => Left(s"Cannot index into non-list field '${seg.base}'")
            case None =>
              val opt = expectedType.endsWith("?") || expectedType == "?"
              Right(render(seg.base, None, opt))
        case _ =>
          Right(render(seg.base, seg.idx, seg.opt)) // fallback

    // --- your checker ---------------------------------------------------------

    def checkAndCorrectPath(rawPath: String, offset: Int)(using ctx: ExprContext): Either[DLCompileError, String] =
      val segments = rawPath.split("\\.").toList
      
      @annotation.tailrec
      def loop(current: Map[String, Any], segs: List[String], acc: List[String], isFirst: Boolean): Either[DLCompileError, List[String]] =
        segs match
          case Nil => Right(acc)

          case segStr :: rest =>
            parseSeg(segStr) match
              case Left(msg) =>
                Left(DLCompileError(offset, msg))

              case Right(seg) =>
                // Absolute lookup first
                current.get(seg.base) match
                  case None =>
                    // If first segment and searchThis enabled, try relativeFields
                    if isFirst && (ctx.searchThis || seg.base == "this") && ctx.relativeFields.nonEmpty then
                      ctx.relativeFields.get(seg.base) match
                        case Some(subtree: Map[String @unchecked, Any @unchecked]) =>
                          val expectedType = subtree.get("__type").collect { case s: String => s }.getOrElse("{}")
                          correct(seg, expectedType) match
                            case Left(msg) => Left(DLCompileError(offset, msg))
                            case Right(spelled) => loop(subtree, rest, acc :+ spelled, isFirst = false)

                        case Some(expectedType: String) =>
                          correct(seg, expectedType) match
                            case Left(msg) => Left(DLCompileError(offset, msg))
                            case Right(spelled) => loop(Map.empty, rest, acc :+ spelled, isFirst = false)

                        case None =>
                          // allow top-level val symbols
                          val valKey = s"__val_${seg.base}"
                          if ctx.typeInfo.contains(valKey) then
                            loop(current, rest, acc :+ segStr, isFirst = false)
                          else
                            Left(DLCompileError(offset, s"Field '${seg.base}' does not exist in typeInfo or relativeFields"))

                    else
                      // No relative match — allow top-level val symbols
                      val valKey = s"__val_${seg.base}"
                      if ctx.typeInfo.contains(valKey) then
                        loop(current, rest, acc :+ segStr, isFirst = false)
                      else
                        Left(DLCompileError(offset, s"Field '${seg.base}' does not exist in typeInfo"))

                  case Some(subtree: Map[String @unchecked, Any @unchecked]) =>
                    val expectedType = subtree.get("__type").collect { case s: String => s }.getOrElse("{}")
                    correct(seg, expectedType) match
                      case Left(msg) => Left(DLCompileError(offset, msg))
                      case Right(spelled) => loop(subtree, rest, acc :+ spelled, isFirst = false)

                  case Some(expectedType: String) =>
                    correct(seg, expectedType) match
                      case Left(msg) => Left(DLCompileError(offset, msg))
                      case Right(spelled) => loop(Map.empty, rest, acc :+ spelled, isFirst = false)

                  case _ =>
                    Left(DLCompileError(offset, s"Unexpected structure for field '${seg.base}'"))

      loop(ctx.typeInfo, segments, Nil, isFirst = true).map(_.mkString("."))

    P(Index ~ fullPath ~ isFunc).flatMap {
      case (offset, raw, true) =>
        val lastDot = raw.lastIndexOf('.')
        if lastDot == -1 then
          Fail.opaque("Function call detected where path was expected")
        else {
          val base = raw.take(lastDot)
          val fn = raw.drop(lastDot + 1)
          checkAndCorrectPath(base, offset) match
            case Left(err) =>
              P(Index).flatMap(_ => Fail.opaque(err.msg)) // dummy to return a Parser
            case Right(clean) =>
              Pass((clean, Some(fn)))
        }

      case (offset, raw, false) =>
        checkAndCorrectPath(raw, offset) match
          case Left(err) =>
            P(Index).flatMap(_ => Fail.opaque(err.msg))
          case Right(clean) =>
            Pass((clean, None))
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
      case "now"  => NowFn()
      case "uuid" => UUIDFn()
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
          // Promote inner class' fields to top level (relativeFields)
          val ctxForArgs =
              ctx.copy(
                relativeFields = Utility.elementSchemaFor(basePath, ctx.typeInfo),
                searchThis = true
              )

          given ExprContext = ctxForArgs
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
              (ctx, MapStmt(basePath, fn))
            }): ParseStmtResult
          }

        case None =>
          P(Fail)
      }
    }
  }