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
import fn.*

import scala.annotation.tailrec

sealed trait Idx
case object Wildcard extends Idx // []
case class Fixed(i: Int) extends Idx // [3]
case class Seg(base: String, idx: Option[Idx], opt: Boolean)

private val segRx = "^([A-Za-z0-9_]+)(?:\\[(\\d*)\\])?(\\?)?$".r

// A tiny module that exposes valueExpr bound to the current given ExprContext
trait ValueExprModule {
  def valueExpr[$: P](using ExprContext): P[ParseFnResult]
}

//
// First level:
//     path
//     functions
//     collection statements
//
trait Level1 extends Level0 {
  self: ValueExprModule =>

//  private def identS[$: P]: P[String] =
//    P(CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_").rep).!
//
//  private def identU[$: P]: P[Unit] =
//    P(CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_").rep)

//  private def indexPart[$: P]: P[String] =
//    P("[" ~ CharsWhileIn("0-9").! ~ "]").map(i => s"[$i]")

//  private def segment[$: P]: P[String] =
//    // ident + optional [index] / [] / {}
//    P(identS.! ~ (indexPart).?).map {
//      case (name, Some(suffix)) => s"$name$suffix"
//      case (name, None) => s"$name"
//    }

  private def segmentFn[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(identifier.!).map { name =>
      Right(GetFn(name, Utility.isPathOptional(name, ctx), RootFn, ctx.posStr))
    }

  def pathFn[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(Index ~ segmentFn ~ ("." ~ segmentFn).rep).map {
      case (offset, head, tail) =>
        val (errs, oks) = (head :: tail.toList).partitionMap(identity)
        if errs.nonEmpty then Left(errs.head)
        else {
          val linked = oks.reduceLeft { (recv, next) =>
            next match {
              case g: GetFn => g.copy(recv = recv)
              case i: IndexFn => IndexFn(recv, i.index, ctx.posStrFrom(offset))
              case fn => fn
            }
          }
          Right(linked)
        }
    }

  // maybeIndex: parses optional bracketed index/key access
  // -----------------------------------------------------
  private def maybeIndex[$: P](base: Fn[Any])(using ctx: ExprContext): P[ParseFnResult] =
    // Match zero or more bracketed index expressions, each containing one valueExpr
    P(("[" ~/ valueExpr ~ "]").rep(0)).map { indexExprs =>
      if indexExprs.isEmpty then
        Right(base)
      else {
        // Split errors from OKs
        val (errs, oks) = indexExprs.partitionMap(identity)
        if errs.nonEmpty then
          Left(errs.head)
        else {
          // Fold successive IndexFn wrappers for curried access: foo[3][wow]
          val folded = oks.foldLeft(base) { (recv, idxFn) =>
            IndexFn(recv, idxFn, ctx.posStr)
          }
          Right(folded)
        }
      }
    }

//  def pathBase[$: P]: P[String] =
//    P(segment ~ (!("." ~ identU ~ "(") ~ "." ~ segment).rep).map { case (head, tail) =>
//      (head +: tail.toList).mkString(".")
//    }

//  // 1) Non-failing path parser that *returns* the semantic error
//  private def pathEither[$: P](using ctx: ExprContext): P[Either[DLCompileError, String]] =
//    P(Index ~ pathBase).map { case (offset, rawPath) =>
//      // Ensure position accuracy for any potential DLCompileError
//      val validation = Utility.getPathType(rawPath)(using ctx.copy(pos = offset))
//
//      validation match
//        case Left(err) =>
//          // Preserve existing error type so Level2 callers work unchanged
//          Left(err.copy(msg = s"Invalid path '$rawPath': ${err.msg}"))
//        case Right(_) =>
//          Right(rawPath)
//    }

//  // 2) Keep a strict version (for places where you *want* a hard parse error)
//  def path[$: P](using ctx: ExprContext): P[String] =
//    pathEither.flatMap {
//      case Right(clean) => P(Pass(clean))
//      case Left(err)    => P(Fail.opaque(err.msg)) // <- only use where a hard parse failure is desired
//    }


//    println(s"[pathFn] offset=$offset")
//    println(s"[pathFn] base=$base")
//    println(s"[pathFn] ctx.schema.fields=${ctx.schema.fields.map(_.name)}")
  
  // Parses: "." ident "(" args ")"
  private def methodCall[$: P](using ctx: ExprContext): P[Either[(DLCompileError, String, Int), (String, List[Fn[Any]], Int)]] =
    P(
      Index ~ // capture offset *before* the dot
        WS0 ~ "." ~ identifier.! ~
        "(" ~/ WS0 ~
        valueExpr.rep(sep = "," ~/ WS0) ~
        WS0 ~ ")"
    ).map { case (off, name, argsRaw) =>
      println(s"[methodCall args] $argsRaw")
      val (errs, oks) = argsRaw.partitionMap(identity)

      if errs.nonEmpty then
        println(s"[methodCall err] matched .$name(...) with ${argsRaw.size} args at offset $off and err ${errs.head}")
        Left((errs.head, name, off))
      else {
        println(s"[methodCall] matched .$name(...) with ${argsRaw.size} args at offset $off")
        Right((name, oks.toList, off))
      }
    }

  def methodChain[$: P](base: Fn[Any])(using ctx: ExprContext): P[ParseFnResult] =
    def loop(current: Fn[Any]): P[Either[DLCompileError, Fn[Any]]] =
      P(methodCall).flatMap {
        case Left((err, name, off)) =>
          P(Pass(Left(err)))

        case Right((name, args, off)) =>
          CompileFnRegistry.lookup(name) match
            case Some(cfn) =>
              given ExprContext = ctx.copy(pos = off)

              val recv = NamedReceiver("anon", Utility.rhsType(current).toKnownType, current)
              cfn.build(recv, args)(using ctx) match {
                case Left(err) => P(Pass(Left(err)))
                case Right(fn) => loop(fn)
              }

            case None =>
              P(Pass(Left(DLCompileError(ctx.posStrFrom(off), s"Unknown method: $name"))))
      } | P(Pass(Right(current)))

    // 🔹 Keep this — enables foo.do()[3]
    loop(base).flatMap {
      case Left(err) => P(Pass(Left(err)))
      case Right(fn0) => maybeIndex(fn0)
    }

  def baseExpr[$: P](using ctx: ExprContext): P[ParseFnResult] =
    (standaloneFn | constant | pathFn).flatMap {
      case Right(fn) => methodChain(fn)
      case l @ Left(_) => P(Pass(l))
    } ~ WS0

  // ---- Functions ----

  private def standaloneFn[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(Index ~ identifier.! ~ "(" ~/ WS0 ~ valueExpr.rep(sep = "," ~/ WS0) ~ WS0 ~ ")")
      .flatMap { case (off, name, argsRaw) =>
        CompileFnRegistry.lookup(name) match {
          case None =>
            P(Pass(Left(DLCompileError(ctx.posStrFrom(off), s"Unknown function: $name"))))

          case Some(cfn) =>
            val (errs, oks) = argsRaw.partitionMap(identity)
            if errs.nonEmpty then P(Pass(Left(errs.head)))
            else {
              val buildResult =
                cfn.build(
                  NamedReceiver(
                    path = "anon",
                    ftype = ScalarType("", "scala.Any"),
                    fn = NoOpFn
                  ),
                  oks.toList
                )(using ctx).asInstanceOf[Either[DLCompileError, Fn[Any]]]

              buildResult match
                case Left(err) => P(Pass(Left(err)))
                case Right(fnBuilt) =>
                  cfn.validate(fnBuilt.asInstanceOf)(using ctx) match
                    case Left(vErr) => P(Pass(Left(vErr)))
                    case Right(_) => maybeIndex(fnBuilt)
            }
        }
      }
}
