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

  private def identS[$: P]: P[String] =
    P(CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_").rep).!

  private def identU[$: P]: P[Unit] =
    P(CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_").rep)

  private def indexPart[$: P]: P[String] =
    P("[" ~ CharsWhileIn("0-9").! ~ "]").map(i => s"[$i]")

  private def wildcardIndex[$: P]: P[String] =
    P("[]").!

  private def curlyBraces[$: P]: P[String] =
    P("{}").!

  private def optSuffix[$: P]: P[String] =
    P("?".!.?).map(_.getOrElse(""))

  private def segment[$: P]: P[String] =
    P(identS ~ (wildcardIndex | indexPart | curlyBraces).? ~ optSuffix).map {
      case (name, Some(suffixPart), suf) => s"$name$suffixPart$suf"
      case (name, None, suf) => s"$name$suf"
    }

  def pathBase[$: P]: P[String] =
    P(segment ~ (!("." ~ identU ~ "(") ~ "." ~ segment).rep).map {
      case (head, tail) => (head +: tail.toList).mkString(".")
    }

  // 1) Non-failing path parser that *returns* the semantic error
  private def pathEither[$: P](using ctx: ExprContext): P[Either[DLCompileError, String]] =
    P(Index ~ pathBase).map { case (offset, raw) =>
      CorrectPath.rewritePath(raw, offset) // Either[DLCompileError, String]
    }

  // 2) Keep a strict version (for places where you *want* a hard parse error)
  def path[$: P](using ctx: ExprContext): P[String] =
    pathEither.flatMap {
      case Right(clean) => P(Pass(clean))
      case Left(err) => P(Fail.opaque(err.msg)) // <- only use where a hard parse failure is desired
    }

  // 3) Make pathFn propagate domain errors (no parser Fail here)
  private def pathFn[$: P](using ctx: ExprContext): P[ParseFnResult] =
    pathEither.map {
      case Left(err) => Left(err) // bubble semantic error
      case Right(path) => Right(GetFn(path, searchThis = ctx.searchThis)) // success
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
  
  def methodChain[$: P](base: Fn[Any], valueExpr: => P[ParseFnResult])
                       (using ctx: ExprContext): P[ParseFnResult] =
    P(WS0 ~ Index ~ methodCall(valueExpr).rep).map { (offset, chain) =>
      // keep the given in scope for Utility.rhsType / other helpers
      given ExprContext = ctx

      val (errs, oks) = chain.partitionMap(identity)
      if (errs.nonEmpty) Left(errs.head)
      else {
        val startT: Either[DLCompileError, SymbolType] =
          Utility.rhsType(base).toRight(DLCompileError(offset, "Unable to infer receiver type for method chain"))

        val folded = oks.foldLeft[Either[DLCompileError, (Fn[Any], SymbolType)]](startT.map(t => (base, t))) {
          case (Left(e), _) => Left(e)
          case (Right((recvFn, recvT)), (fnName, args)) =>
            MethodSig.methodSigs.get(fnName) match {
              case None => Left(DLCompileError(offset, s"Unknown method: $fnName"))
              case Some(sig) =>
                if (!sig.upon.contains(recvT))
                  Left(DLCompileError(offset, s"Method '$fnName' cannot be applied to ${pretty(recvT)}"))
                else
                  methodFunctions.get(fnName) match {
                    case None => Left(DLCompileError(offset, s"No builder registered for method: $fnName"))
                    case Some(build) =>
                      build(recvFn, args, offset).map { built =>
                        (built.asInstanceOf[Fn[Any]], sig.out(recvT))
                      }
                  }
            }
        }

        folded.map(_._1)
      }
    }

  def baseExpr[$: P](valueExpr: => P[ParseFnResult])(using ctx: ExprContext): P[ParseFnResult] =
    P((standaloneFn.map(Right(_)) | constant | pathFn).flatMap {
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

  private def expectGetPath(arg: Fn[Any], name: String, off: Int): Either[DLCompileError, String] =
    arg match {
      case g: GetFn => Right(g.path) // already corrected by your path parser
      case other => Left(DLCompileError(off, s"$name(...) expects a field path argument, got ${other.getClass.getSimpleName}"))
    }

  private def expectBoolean(arg: Fn[Any], name: String, off: Int): Either[DLCompileError, BooleanFn] =
    arg match {
      case b: BooleanFn => Right(b)
      case other => Left(DLCompileError(off, s"$name(...) requires a boolean expression, got ${other.getClass.getSimpleName}"))
    }

  private def expectConstInt(arg: Fn[Any], name: String, off: Int): Either[DLCompileError, Int] =
    arg match {
      case ConstantFn(i: Int) => Right(i)
      case ConstantFn(x) => Left(DLCompileError(off, s"$name(...) requires an integer literal, got ${x.getClass.getSimpleName}"))
      case other => Left(DLCompileError(off, s"$name(...) requires an integer literal, got ${other.getClass.getSimpleName}"))
    }

  private def checkArgs(
                          fnName: String,
                          args: List[Fn[Any]],
                          required: Int,
                          offset: Int
                        ): Either[DLCompileError, Unit] =
    if (args.length != required)
      Left(DLCompileError(offset, s"Function $fnName() expected $required argument(s), got ${args.length}"))
    else
      Right(())

  private val methodFunctions
  : Map[String, (Fn[Any], List[Fn[Any]], Int) => Either[DLCompileError, Fn[Any]]] = Map(
    M_STARTSWITH -> { (recv, args, off) =>
      for {
        _ <- checkArgs(M_STARTSWITH, args, 1, off)
      } yield StartsWithFn(recv.as[String], args.head.as[String])
    },
    M_ENDSWITH -> { (recv, args, off) =>
      for {
        _ <- checkArgs(M_ENDSWITH, args, 1, off)
      } yield EndsWithFn(recv.as[String], args.head.as[String])
    },
    M_CONTAINS -> { (recv, args, off) =>
      for {
        _ <- checkArgs(M_CONTAINS, args, 1, off)
      } yield ContainsFn(recv.as[String], args.head.as[String])
    },
    M_EQUALSIGNORECASE -> { (recv, args, off) =>
      for {
        _ <- checkArgs(M_EQUALSIGNORECASE, args, 1, off)
      } yield EqualsIgnoreCaseFn(recv.as[String], args.head.as[String])
    },
    M_MATCHESREGEX -> { (recv, args, off) =>
      for {
        _ <- checkArgs(M_MATCHESREGEX, args, 1, off)
      } yield MatchesRegexFn(recv.as[String], args.head.as[String])
    },
    M_ELSE -> { (recv, args, off) =>
      for {
        _ <- checkArgs(M_ELSE, args, 1, off)
      } yield ElseFn(recv, args.head)
    },
    M_ISDEFINED -> { (recv, args, off) =>
      for {
        _ <- checkArgs(M_ISDEFINED, args, 0, off)
      } yield IsDefinedFn(recv)
    },
    M_LEN -> { (recv, args, off) =>
      for {
        _ <- checkArgs(M_LEN, args, 0, off)
      } yield LengthFn(recv)
    },
    M_TOUPPERCASE -> { (recv, args, off) =>
      for {
        _ <- checkArgs(M_TOUPPERCASE, args, 0, off)
      } yield ToUpperFn(recv)
    },
    M_TOLOWERCASE -> { (recv, args, off) =>
      for {
        _ <- checkArgs(M_TOLOWERCASE, args, 0, off)
      } yield ToLowerFn(recv)
    },
    M_TRIM -> { (recv, args, off) =>
      for {
        _ <- checkArgs(M_TRIM, args, 0, off)
      } yield TrimFn(recv)
    },
    M_TEMPLATE -> { (recv, args, off) =>
      for {
        _ <- checkArgs(M_TEMPLATE, args, 0, off)
      } yield {
        val varMap = recv match
          case ConstantFn(s: String) =>
            TemplateUtils.extractVariables(s).map(v => v -> GetFn(v)).toMap
          case _ =>
            Map.empty[String, Fn[Any]] // template is not a constant, so defer resolution
        InterpolateFn(recv, varMap)
      }
    },
    M_SUBSTR -> { (recv, args, off) =>
      if args.isEmpty then Left(DLCompileError(off, s"Function substr() expected at least 1 argument, but found none"))
      else
        val start = args.head.as[Int]
        val endOpt = args.lift(1).map(_.as[Int])
        Right(SubstringFn(recv, start, endOpt))
    },
    M_REPLACE -> { (recv, args, off) =>
      for {
        _ <- checkArgs(M_REPLACE, args, 2, off)
      } yield ReplaceFn(recv, args.head, args(1))
    },
    M_DATEFMT -> { (recv, args, off) =>
      for {
        _ <- checkArgs(M_DATEFMT, args, 1, off)
      } yield FormatDateFn(recv, args.head.as[String])
    },
    M_TODATE -> { (recv, args, off) =>
      for {
        _ <- checkArgs(M_TODATE, args, 1, off)
      } yield ParseDateFn(recv, args.head.as[String])
    },
    M_SORTASC -> { (_, args, off) =>
      args match {
        case Nil =>
          Right(SortFn(None, asc = true))
        case a1 :: Nil =>
          expectGetPath(a1, M_SORTASC, off).map(p => SortFn(Some(p), asc = true))
        case _ =>
          Left(DLCompileError(off, s"Function sortAsc() expected 0 or 1 argument(s), got ${args.length}"))
      }
    },
    M_SORTDESC -> { (_, args, off) =>
      args match {
        case Nil =>
          Right(SortFn(None, asc = false))
        case a1 :: Nil =>
          expectGetPath(a1, M_SORTDESC, off).map(p => SortFn(Some(p), asc = false))
        case _ =>
          Left(DLCompileError(off, s"Function sortDesc() expected 0 or 1 argument(s), got ${args.length}"))
      }
    },
    M_FILTER -> { (_, args, off) =>
      for {
        _     <- checkArgs(M_FILTER, args, 1, off)
        pred  <- expectBoolean(args.head, M_FILTER, off)
      } yield FilterFn(pred)
    },
    M_DISTINCT -> { (_, args, off) =>
      args match {
        case Nil =>
          Right(DistinctFn(None))
        case a1 :: Nil =>
          expectGetPath(a1, M_DISTINCT, off).map(p => DistinctFn(Some(p)))
        case _ =>
          Left(DLCompileError(off, s"Function distinct() expected 0 or 1 argument(s), got ${args.length}"))
      }
    },
    M_LIMIT -> { (_, args, off) =>
      for {
        _   <- checkArgs(M_LIMIT, args, 1, off)
        n   <- expectConstInt(args.head, M_LIMIT, off)   // must be an int literal
      } yield LimitFn(n)
    },
    M_REVERSE -> { (_, args, off) =>
      for {
        _ <- checkArgs(M_REVERSE, args, 0, off)
      } yield ReverseFn()
    },
    M_CLEAN -> { (_, args, off) =>
      for {
        _ <- checkArgs(M_CLEAN, args, 0, off)
      } yield CleanFn()
    },
  )

  // ---- Collection Statements ----

  private trait CollectionMethodParser {
    def name: String
    def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult]
  }

  def collectionStmt[$: P](booleanExpr: ExprContext ?=> P[ParseBoolResult])
                          (using ctx: ExprContext): P[ParseStmtResult] = {

    case object SortAscMethod extends CollectionMethodParser {
      val name: String = M_SORTASC

      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        P(Index ~ pathBase.?).map {
          case (offset, None) =>
            // sortAsc() — no key, natural ordering
            Right(SortFn(None, true))

          case (offset, Some(rawKey)) =>
            // sortAsc(field) — rewrite/validate the (possibly relative) key
            CorrectPath.rewritePath(rawKey, offset) match {
              case Left(err) => Left(err) // bubble DLCompileError
              case Right(cleanKey) => Right(SortFn(Some(cleanKey), true)) // ascending
            }
        }
    }

    case object SortDescMethod extends CollectionMethodParser {
      val name: String = M_SORTDESC

      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        P(Index ~ pathBase.?).map {
          case (offset, None) =>
            // sortAsc() — no key, natural ordering
            Right(SortFn(None, false))

          case (offset, Some(rawKey)) =>
            // sortAsc(field) — rewrite/validate the (possibly relative) key
            CorrectPath.rewritePath(rawKey, offset) match {
              case Left(err) => Left(err) // bubble DLCompileError
              case Right(cleanKey) => Right(SortFn(Some(cleanKey), false)) // descending
            }
        }
    }

    case object FilterMethod extends CollectionMethodParser {
      val name: String = M_FILTER

      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        booleanExpr(using summon[ExprContext].copy(searchThis = true)).map {
          case Right(pred: BooleanFn) => Right(FilterFn(pred): Fn[Any])
          case Left(err: DLCompileError) => Left(err)
        }
    }

    case object DistinctMethod extends CollectionMethodParser {
      val name: String = M_DISTINCT

      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        P(Index ~ pathBase.?).map {
          case (offset, None) =>
            Right(DistinctFn(None))

          case (offset, Some(rawKey)) =>
            CorrectPath.rewritePath(rawKey, offset) match {
              case Left(err) => Left(err) // bubble DLCompileError
              case Right(cleanKey) => Right(DistinctFn(Some(cleanKey)))
            }
        }
    }

    case object LimitMethod extends CollectionMethodParser {
      val name: String = M_LIMIT

      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        P(Index ~ number).map {
          case (_, n) if n >= 0 => Right(LimitFn(n))
          case (offset, n) => Left(DLCompileError(offset, s"limit(...) requires a non-negative integer, found $n"))
        }

      private def number[$: P]: P[Int] =
        P(CharsWhileIn("0-9", min = 1).!).map(_.toInt)
    }

    case object ReverseMethod extends CollectionMethodParser {
      val name: String = M_REVERSE
      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        P(Index ~ (("(" ~ WS0 ~ ")").?)).map { _ =>
          Right(ReverseFn())
        }
    }

    case object CleanMethod extends CollectionMethodParser {
      val name: String = M_CLEAN
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


    P(Index ~ path).flatMap { case (off0, basePath) =>

      val ctxWithThis = Utility.addThisType(basePath, ctx)

      // Promote inner class' fields to top level (relativeFields)
      val ctxForArgs =
        ctxWithThis.copy(
          relativeFields = ctxWithThis.relativeFields ++ Utility.elementSchemaFor(basePath, ctxWithThis.typeInfo),
          searchThis = true
        )

      given ExprContext = ctxForArgs

      // resolve + parse a single method's args into a Fn
      def parseOneMethod(name: String): P[ParseFnResult] =
        for {
          resolved <- lookupMethod(name)
          fnRes <- resolved match {
            case Left(err) => P(Pass(Left(err)))
            case Right(par) => parseMethodArgs(par)
          }
        } yield fnRes

      // --- REQUIRE a first method: ".name(" ---
      // parse ".name", then *peek* "(" separately and return the name
      val firstMethodNameP: P[String] =
        P(WS0 ~ "." ~ identifier.!).flatMap { name =>
          P(&("(")).map(_ => name)
        }

      firstMethodNameP.flatMap { firstMethodName =>
        for {
          firstResult <- parseOneMethod(firstMethodName)

          // zero+ additional methods, each must also be followed by '('
          moreNames <- P((WS0 ~ "." ~ identifier.!).flatMap { n =>
            P(&("(")).map(_ => n)
          }).rep

          restFns <- moreNames.foldLeft(Pass(Right(Nil): ParseFnListResult): P[ParseFnListResult]) {
            case (accP, methodName) =>
              accP.flatMap {
                case left@Left(_) => P(Pass(left)) // keep first error
                case Right(accum) =>
                  parseOneMethod(methodName).map {
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
      }
    }
  }