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

  private def optQ[$: P]: P[String] =
    P("?").!.?.map(_.getOrElse(""))

  private def segment[$: P]: P[String] =
    // ident + optional '?' + optional [index] / [] / {}
    P(identS.! ~ optQ ~ (wildcardIndex | indexPart | curlyBraces).?).map {
      case (name, q, Some(suffix)) => s"$name$q$suffix"
      case (name, q, None) => s"$name$q"
    }

  def pathBase[$: P]: P[String] =
    P(segment ~ (!("." ~ identU ~ "(") ~ "." ~ segment).rep).map { case (head, tail) =>
      (head +: tail.toList).mkString(".")
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
      case Left(err)    => P(Fail.opaque(err.msg)) // <- only use where a hard parse failure is desired
    }

  // 3) Make pathFn propagate domain errors (no parser Fail here)
  private def pathFn[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(Index ~ segmentFn ~ (!("." ~ identU ~ "(") ~ "." ~ segmentFn).rep).flatMap {
      case (offset, headRes, tailRes) =>
        println(s"[pathFn] START head=$headRes tail=$tailRes")

        (headRes :: tailRes.toList).partitionMap(identity) match
          case (err :: _, _) =>
            println(s"[pathFn] ❌ parse error bubble: $err")
            P(Pass(Left(err)))

          case (Nil, okFns) =>
            val head = okFns.head
            val tail = okFns.tail

            val base: Fn[Any] = tail.foldLeft(head) {
              case (g1: GetFn, g2: GetFn)   => g2.copy(recv = Some(g1))
              case (g1: GetFn, idx: IndexFn) => IndexFn(g1, idx.index)
              case (idx1: IndexFn, idx2: IndexFn) => IndexFn(idx1, idx2.index)
              case (idx: IndexFn, g2: GetFn) => g2.copy(recv = Some(idx))
              case (acc, next)               => next.asInstanceOf[Fn[Any]]
            }

            println(s"[pathFn] offset=$offset")
            println(s"[pathFn] base=$base  (${base.getClass.getSimpleName})")
            println(s"[pathFn] ctx.receiver=${ctx.receiver}")
            println(s"[pathFn] ctx.schema.fields=${ctx.schema.fields.map(_.name)}")

            base match
              case GetFn(path, _, _) =>
                println(s"[pathFn] evaluating GetFn path='$path' receiver?=${ctx.receiver.isDefined}")
                if path == "Nil" then
                  println(s"[pathFn] ✅ recognized Nil keyword")
                  P(Pass(Right(ConstantFn[List[Any]](Nil).asInstanceOf[Fn[Any]])))
                else if path == "this" && ctx.receiver.isEmpty then
                // inside pathFn, in the base match for GetFn(path, _, _)
                  println(s"[pathFn] 🚨 illegal 'this' detected — receiver=${ctx.receiver}")
                  P(Pass(Left(DLCompileError(offset,
                    "Use of 'this' with no receiver in scope"))))
                else {
                  // 1️⃣ Compute an effective receiver type that unwraps List/Option[List]
                  val effectiveRecvType =
                    ctx.receiver
                      .map(_.fieldType)
                      .map(Utility.unwrapVal)
                      .flatMap {
                        case ListType(_, elem, _) => Some(elem)
                        case OptionType(_, ListType(_, elem, _), _) => Some(elem)
                        case other => Some(other)
                      }

                  println(s"[pathFn] effectiveRecvType=$effectiveRecvType")

                  // 2️⃣ Build a combined field lookup
                  val fieldExists =
                    ctx.resolveSymbol(path).isDefined ||
                      effectiveRecvType.exists {
                        case ClassType(_, _, fields) => fields.exists(_.name == path)
                        case _ => false
                      } ||
                      ctx.schema.fields.exists(_.name == path)

                  // 3️⃣ Validate
                  if !fieldExists && path != "this" then
                    P(Pass(Left(DLCompileError(offset, s"Field '$path' does not exist"))))
                  else
                    methodChain(base)
                }

              case other =>
                println(s"[pathFn] other node type = ${other.getClass.getSimpleName}")
                methodChain(base)
    }

//    println(s"[pathFn] offset=$offset")
//    println(s"[pathFn] base=$base")
//    println(s"[pathFn] ctx.schema.fields=${ctx.schema.fields.map(_.name)}")

  // Parses: "." ident "(" args ")"
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

  // parse optional fixed index and wrap.
  private def maybeIndex[$: P](fn: Fn[Any])(using ctx: ExprContext): P[ParseFnResult] =
    P("[" ~ CharsWhileIn("0-9").! ~ "]").?.map {
      case Some(iStr) =>
        println(s"[maybeIndex] attempting to index fn=$fn with index=$iStr")

        val baseTypeOpt = Utility.rhsType(fn)
        println(s"[maybeIndex] baseTypeOpt = $baseTypeOpt")

        baseTypeOpt match
          case Some(v: ValType) =>  // ✅ unwrap ValType before checking
            v.valueType match
              case lt if Utility.isIndexable(lt) =>
                println(s"[maybeIndex] (ValType) field type ${lt.typeName} IS indexable → creating IndexFn")
                Right(IndexFn(fn, iStr.toInt))
              case lt =>
                println(s"[maybeIndex] (ValType) field type ${lt.typeName} is NOT indexable → returning Left")
                Left(DLCompileError(1, s"Cannot index into non-list field '${fn match
                  case g: GetFn => g.path
                  case _        => "value"
                }' of type ${lt.typeName}"))

          case Some(ft) if Utility.isIndexable(ft) =>
            println(s"[maybeIndex] field type ${ft.typeName} IS indexable → creating IndexFn")
            Right(IndexFn(fn, iStr.toInt))

          case Some(ft) =>
            println(s"[maybeIndex] field type ${ft.typeName} is NOT indexable → returning Left")
            Left(DLCompileError(1, s"Cannot index into non-list field '${fn match
              case g: GetFn => g.path
              case _        => "value"
            }' of type ${ft.typeName}"))

          case None =>
            println(s"[maybeIndex] field type UNKNOWN for $fn → returning Left")
            Left(DLCompileError(1, "Cannot determine type for indexed receiver"))

      case None =>
        println(s"[maybeIndex] no index found for fn=$fn → returning Right(fn)")
        Right(fn)
    }

  // Level1.scala (or wherever getPathType/pathFn lives)
  private def effectiveReceiverType(using ctx: ExprContext): Option[FieldType] =
    ctx.receiver
      .map(_.fieldType)
      .map(Utility.unwrapVal)
      .flatMap {
        case ListType(_, elem, _) => Some(elem) // <- use element
        case OptionType(_, ListType(_, elem, _), _) => Some(elem) // <- use element
        case other => Some(other) // class/scalar/option(non-list)
      }

  private def segmentFn[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(identU.!).flatMap { name =>
      val isOpt = Utility.isPathOptional(name, ctx)
      val recv: Option[Fn[Any]] = ctx.receiver.flatMap(_.parentFn)
      val gf = GetFn(name, isOptional = isOpt, recv)
      maybeIndex(gf) // <- now returns ParseFnResult
    }

  def methodChain[$: P](base: Fn[Any])(using ctx: ExprContext): P[ParseFnResult] = {
    // === 1) Context setup (unchanged) ===
    val argsCtx: ExprContext =
      base match {
        // only for top-level schema paths, NOT for "this" or "this.*"
        case GetFn(p, _, _) if !p.startsWith("this") =>
          val maybeField: Option[FieldType] = Utility.elementSchemaFor(p, ctx.schema)

          val childFields: List[FieldType] = maybeField match {
            case Some(c: ClassType) => c.fields
            case Some(o: OptionType) => o.valueType match {
              case c: ClassType => c.fields
              case _ => Nil
            }
            case Some(l: ListType) => l.elementType match {
              case c: ClassType => c.fields
              case _ => Nil
            }
            case _ => Nil
          }

          val withRecv0 = ctx.withReceiverFromPath(p)

          val withBaseBound =
            maybeField.fold(withRecv0)(ft => withRecv0.withVals(p -> ft))

          if childFields.nonEmpty
          then withBaseBound.withVals(childFields.map(ft => ft.name -> ft) *)
          else withBaseBound

        // already in element scope or non-path
        case _ => ctx
      }

    given ExprContext = argsCtx

    // === 2) Recursive method-call folding ===
    def loop(current: Fn[Any]): P[Either[DLCompileError, Fn[Any]]] =
      P(methodCall).flatMap {
        case Left((err, name, off)) =>
          // We got an error from parsing args/predicate
          val recvType = Utility.rhsType(current)
          println(s"[loop] methodCall error: $err, receiver=$recvType, method=$name")

          // Try to refine the error: if the receiver is invalid for this fn, report that instead
          val refined: DLCompileError = recvType match {
            case Some(rt) =>
              CompileFnRegistry.lookup(name) match {
                case Some(cfn) if !cfn.accepts(rt)(using ctx) =>
                  DLCompileError(off, s"Method '$name' cannot be applied to receiver of type ${rt.typeName}")
                case _ => err
              }
            case None => err
          }
          P(Pass(Left(refined)))

        case Right((name, args, off)) =>
          println(s"[loop] got method name: $name, args: $args, off: $off")
          CompileFnRegistry.lookup(name) match {
            case Some(cfn) =>
              println(s"[loop] found CompileFn for $name")

              // Early receiver check
              val recvType = Utility.rhsType(current)(using argsCtx).getOrElse(ScalarType("", "scala.Any"))
              if !cfn.accepts(recvType)(using argsCtx) then
                println(s"[loop] receiver type ${recvType.typeName} not accepted by $name")
                P(Pass(Left(DLCompileError(off, s"Method '$name' cannot be applied to receiver of type ${recvType.typeName}"))))
              else {
                // phase 1: build
                cfn.build(current, args)(using argsCtx) match
                  case Left(e) =>
                    println(s"[loop] build for $name failed: $e")
                    P(Pass(Left(e)))

                  case Right(fnBuilt) =>
                    // phase 2: validate
                    cfn.validate(fnBuilt.asInstanceOf)(using argsCtx) match
                      case Left(err) =>
                        println(s"[loop] validate for $name failed: $err")
                        P(Pass(Left(err)))
                      case Right(_) =>
                        println(s"[loop] $name succeeded: $fnBuilt")
                        loop(fnBuilt.asInstanceOf[Fn[Any]])
              }

            case None =>
              println(s"[loop] no CompileFn for $name")
              P(Pass(Left(DLCompileError(off, s"Unknown method: $name"))))
          }
      } | P(Pass(Right(current)))

    // === 3) Optional trailing index (unchanged) ===
    loop(base).flatMap {
      case Left(err)  => P(Pass(Left(err)))
      case Right(fn0) =>
        maybeIndex(fn0).map {
          case Left(e)   => Left(e)
          case Right(fn) => Right(fn)
        }
    }
  }

  def baseExpr[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P((standaloneFn | constant | pathFn).flatMap {
      case Right(fn) => methodChain(fn) // now fn is a Fn[Any]
      case l@Left(_) => P(Pass(l))
    } ~ WS0)

  // ---- Functions ----

  private def standaloneFn[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(identifier.! ~ "(" ~/ WS0 ~ valueExpr.rep(sep = "," ~/ WS0) ~ WS0 ~ ")")
      .map { case (name, argsRaw) =>
        val (errs, oks) = argsRaw.partitionMap(identity)
        if errs.nonEmpty then Left(errs.head)
        else
          CompileFnRegistry.lookup(name) match
            case Some(cfn) if cfn.standalone =>
              cfn.build(NoOpFn, oks.toList)(using ctx)
                .flatMap(fn => cfn.validate(fn)(using ctx).map(_ => fn.asInstanceOf[Fn[Any]]))
            case Some(_) =>
              Left(DLCompileError(0, s"$name cannot be used as a standalone function"))
            case None =>
              Left(DLCompileError(0, s"Unknown standalone function: $name"))
      }

  private def expectGetPath(arg: Fn[Any], name: String, off: Int): Either[DLCompileError, String] =
    arg match {
      case g: GetFn => Right(g.path) // already corrected by your path parser
      case other    => Left(DLCompileError(off, s"$name(...) expects a field path argument, got ${other.getClass.getSimpleName}"))
    }

  private def expectConstInt(arg: Fn[Any], name: String, off: Int): Either[DLCompileError, Int] =
    arg match {
      case ConstantFn(i: Int) => Right(i)
      case ConstantFn(x)      => Left(DLCompileError(off, s"$name(...) requires an integer literal, got ${x.getClass.getSimpleName}"))
      case other              => Left(DLCompileError(off, s"$name(...) requires an integer literal, got ${other.getClass.getSimpleName}"))
    }

  // ---- Collection Statements ----

  private trait CollectionMethodParser {
    def name: String

    def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult]
  }

  private def promoteToCollection(recv: Fn[Any])(using ctx: ExprContext): Fn[Any] = recv match
    case g @ GetFn(name,_,_) =>
      // If the bare name is in loop scope, prefer its collection binding `name[]`
      val inLoop = ctx.symbols.headOption.exists(_.contains(name))
      if inLoop then GetFn(s"$name[]", g.isOptional) else g
    case other => other

  def collectionStmt[$: P](using ctx: ExprContext): P[ParseStmtResult] =
    P(Index ~ path).flatMap { case (off0, basePath) =>
      // 1) Add a correctly-typed 'this' based on the base path
      val ctxWithThis = Utility.addThisType(basePath, ctx)

      // 2) Try to resolve the element type for scoping
      val maybeField = Utility.elementSchemaFor(basePath, ctxWithThis.schema)
      val childFields: List[FieldType] = maybeField match
        case Some(c: ClassType) => c.fields
        case Some(o: OptionType) => o.valueType match
          case c: ClassType => c.fields
          case _ => Nil
        case Some(l: ListType) => l.elementType match
          case c: ClassType => c.fields
          case _ => Nil
        case _ => Nil

      val ctxForArgs =
        if childFields.nonEmpty then ctxWithThis.pushScope(childFields)
        else ctxWithThis

      // Build a GetFn for the collection receiver
      val baseFn: Fn[Any] = GetFn(basePath, isOptional = Utility.isPathOptional(basePath, ctxWithThis))

      given ExprContext = ctxForArgs

      // require a first method
      val firstMethodNameP: P[String] =
        P(WS0 ~ "." ~ identifier.!).flatMap { name =>
          P(&("(")).map(_ => name)
        }

      def parseOneMethod(name: String, recv: Fn[Any]): P[ParseFnResult] =
        CompileFnRegistry.lookup(name) match
          case None =>
            P(Pass(Left(DLCompileError(off0, s"Unknown method: $name"))))

          case Some(cfn) =>
            // parse comma-separated args inside the parens
            P("(" ~/ WS0 ~ valueExpr.rep(sep = "," ~/ WS0) ~ WS0 ~ ")").map { argsRaw =>
              val (errs, oks) = argsRaw.partitionMap(identity)
              if errs.nonEmpty then Left(errs.head)
              else
                // phase 1: build
                cfn.build(recv, oks.toList)(using ctx) match
                  case Left(e) => Left(e)
                  case Right(fnBuilt) =>
                    // phase 2: validate
                    cfn.validate(fnBuilt.asInstanceOf)(using ctx).map(_ => fnBuilt.asInstanceOf[Fn[Any]])
            }

      firstMethodNameP.flatMap { firstMethodName =>
        for {
          firstResult <- parseOneMethod(firstMethodName, baseFn)
          moreNames <- P((WS0 ~ "." ~ identifier.!).flatMap(n => P(&("(")).map(_ => n))).rep
          restFns <- moreNames.foldLeft(Pass(Right(Nil)): P[Either[DLCompileError, List[Fn[Any]]]]) {
            case (accP, name) =>
              accP.flatMap {
                case Left(err) => P(Pass(Left(err)))
                case Right(acc) =>
                  parseOneMethod(name, baseFn).map {
                    case Left(err) => Left(err)
                    case Right(fn) => Right(acc :+ fn)
                  }
              }
          }
        } yield {
          for {
            f1 <- firstResult
            rxs <- restFns
          } yield {
            val all = f1 :: rxs
            val fn = if all.size == 1 then all.head else PolyFn(all)
            (ctx, MapStmt(basePath, fn))
          }
        }
      }
    }
}
