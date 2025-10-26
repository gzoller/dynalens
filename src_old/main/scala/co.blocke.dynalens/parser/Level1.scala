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

  private def identS[$: P]: P[String] =
    P(CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_").rep).!

  private def identU[$: P]: P[Unit] =
    P(CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_").rep)

  private def indexPart[$: P]: P[String] =
    P("[" ~ CharsWhileIn("0-9").! ~ "]").map(i => s"[$i]")

  private def segment[$: P]: P[String] =
    // ident + optional '?' + optional [index] / [] / {}
    P(identS.! ~ (indexPart).?).map {
      case (name, Some(suffix)) => s"$name$suffix"
      case (name, None) => s"$name"
    }

  def pathBase[$: P]: P[String] =
    P(segment ~ (!("." ~ identU ~ "(") ~ "." ~ segment).rep).map { case (head, tail) =>
      (head +: tail.toList).mkString(".")
    }

  // 1) Non-failing path parser that *returns* the semantic error
  private def pathEither[$: P](using ctx: ExprContext): P[Either[DLCompileError, String]] =
    P(Index ~ pathBase).map { case (offset, rawPath) =>
      // Ensure position accuracy for any potential DLCompileError
      val validation = Utility.getPathType(rawPath)(using ctx.copy(pos = offset))

      validation match
        case Left(err) =>
          // Preserve existing error type so Level2 callers work unchanged
          Left(err.copy(msg = s"Invalid path '$rawPath': ${err.msg}"))
        case Right(_) =>
          Right(rawPath)
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
        // Propagate pos from FastParse Index into ExprContext for all subsequent logic in this branch
        var currentCtx: ExprContext = ctx.copy(pos = offset)
        println(s"[pathFn] START head=$headRes tail=$tailRes")

        (headRes :: tailRes.toList).partitionMap(identity) match
          case (err :: _, _) =>
            println(s"[pathFn] ❌ parse error bubble: $err")
            P(Pass(Left(err)))

          case (Nil, okFns) =>
            val head = okFns.head
            val tail = okFns.tail

            // For each tail segment, after linking, update ExprContext with new receiver
            var acc: Fn[Any] = head
            tail.foreach { next =>
              acc = (acc, next) match {
                case (g1: GetFn, g2: GetFn)   => g2.copy(recv = g1)
                case (g1: GetFn, idx: IndexFn) => IndexFn(g1, idx.index, ctx.posStrFrom(offset))
                case (idx1: IndexFn, idx2: IndexFn) => IndexFn(idx1, idx2.index, ctx.posStrFrom(offset))
                case (idx: IndexFn, g2: GetFn) => g2.copy(recv = idx)
                case (a, n)               => n.asInstanceOf[Fn[Any]]
              }
              // If the new segment is a GetFn, update the context receiver
              acc match {
                case g: GetFn =>
                  currentCtx = currentCtx.withReceiverFromPath(g.path) match
                    case Right(updated) => updated
                    case Left(_)        => currentCtx
                case _ => // do nothing
              }
            }

            val base = acc

            println(s"[pathFn] offset=$offset")
            println(s"[pathFn] base=$base  (${base.getClass.getSimpleName})")
            println(s"[pathFn] ctx.receiver=${ctx.receiver}")
            println(s"[pathFn] ctx.schema.fields=${ctx.schema.fields.map(_.fieldName)}")

            base match
              case GetFn(path, _, _, _) =>
                println(s"[pathFn] evaluating GetFn path='$path' receiver?=${ctx.receiver.isDefined}")
                if path == "Nil" then
                  println(s"[pathFn] ✅ recognized Nil keyword")
                  P(Pass(Right(ConstantFn[List[Any]](Nil).asInstanceOf[Fn[Any]])))
                else if path == "this" && ctx.receiver.isEmpty then
                  println(s"[pathFn] 🚨 illegal 'this' detected — receiver=${ctx.receiver}")
                  // Use accurate position string
                  P(Pass(Left(DLCompileError(ctx.posStrFrom(offset),
                    "Use of 'this' with no receiver in scope"))))
                else {
                  // 1️⃣ Compute an effective receiver type that unwraps List
                  val effectiveRecvType =
                    ctx.receiver
                      .map(_.ftype)
                      .map(Utility.unwrapVal)
                      .flatMap {
                        case l: ListType => Some(l.elementType)
                        case other       => Some(other)
                      }

                  println(s"[pathFn] effectiveRecvType=$effectiveRecvType")

                  // 2️⃣ Build a combined field lookup
                  val fieldExists =
                    ctx.resolveSymbol(path).isDefined ||
                      effectiveRecvType.exists {
                        case ClassType(_, _, fields, _) => fields.exists(_.name == path)
                        case _ => false
                      } ||
                      ctx.schema.fields.exists(_.fieldName == path)

                  // 3️⃣ Validate
                  if !fieldExists && path != "this" then
                    P(Pass(Left(DLCompileError(ctx.posStrFrom(offset), s"Field '$path' does not exist"))))
                  else
                    // Use the latest ExprContext (with receiver) for methodChain
                    methodChain(base)(using summon[ParsingRun[Any]], currentCtx)
                }

              case other =>
                println(s"[pathFn] other node type = ${other.getClass.getSimpleName}")
                // Use the latest ExprContext (with receiver) for methodChain
                methodChain(base)(using summon[ParsingRun[Any]], currentCtx)
    }

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

  // parse optional fixed index and wrap.
  private def maybeIndex[$: P](fn: Fn[Any])(using ctx: ExprContext): P[ParseFnResult] =
    P((Index ~ "[" ~ CharsWhileIn("0-9").! ~ "]").?).map {
      case Some((off, iStr)) =>
        given ExprContext = ctx.copy(pos = off)
        println(s"[maybeIndex] attempting to index fn=$fn with index=$iStr")

        val baseTypeOpt = Utility.rhsType(fn)
        println(s"[maybeIndex] baseTypeOpt = $baseTypeOpt")

        baseTypeOpt match {
          case TypeResult.Known(v: ValType) =>
            v.valueType match {
              case lt if Utility.isIndexable(lt) =>
                println(s"[maybeIndex] (ValType) field type ${lt.typeName} IS indexable → creating IndexFn")
                val idxFn = IndexFn(fn, ConstantFn(iStr.toInt), ctx.posStr)
//                val newCtx = lt match {
//                  case l: ListType =>
//                    ctx.copy(receiver = Some(
//                      NamedReceiver(
//                        path = "",                          // synthetic / anonymous receiver
//                        ftype = FieldType.synthetic(l.elementType),
//                        fn = fn
//                      )
//                    ))
//                  case _ => ctx
//                }
                Right(idxFn)
              case lt =>
                println(s"[maybeIndex] (ValType) field type ${lt.typeName} is NOT indexable → returning Left")
                Left(DLCompileError(ctx.posStrFrom(off), s"Cannot index into non-list field '${fn match {
                  case g: GetFn => g.path
                  case _        => "value"
                }}' of type ${lt.typeName}"))
            }
          case TypeResult.Known(ft) if Utility.isIndexable(ft) =>
            println(s"[maybeIndex] field type ${ft.typeName} IS indexable → creating IndexFn")
            val idxFn = IndexFn(fn, ConstantFn(iStr.toInt), ctx.posStr)
//            val newCtx = ft match {
//              case l: ListType =>
//                ctx.copy(receiver = Some(
//                  NamedReceiver(
//                    path = "",                          // synthetic / anonymous receiver
//                    ftype = FieldType.synthetic(l.elementType),
//                    fn = fn
//                  )
//                ))
//              case _ => ctx
//            }
            Right(idxFn)
          case TypeResult.Known(ft) =>
            println(s"[maybeIndex] field type ${ft.typeName} is NOT indexable → returning Left")
            Left(DLCompileError(ctx.posStrFrom(off), s"Cannot index into non-list field '${fn match {
              case g: GetFn => g.path
              case _        => "value"
            }}' of type ${ft.typeName}"))
          case TypeResult.Unknown =>
            println(s"[maybeIndex] field type UNKNOWN for $fn → returning Left")
            Left(DLCompileError(ctx.posStrFrom(off), "Cannot determine type for indexed receiver"))
          case TypeResult.Error(e) =>
            Left(e)
        }
      case None =>
        println(s"[maybeIndex] no index found for fn=$fn → returning Right(fn)")
        Right(fn)
    }

  // Level1.scala (or wherever getPathType/pathFn lives)
//  private def effectiveReceiverType(using ctx: ExprContext): Option[FieldType] =
//    ctx.receiver
//      .map(_.ftype)
//      .map(Utility.unwrapVal)
//      .flatMap {
//        case l: ListType => Some(l.elementType)
//        case other       => Some(other)
//      }

  private def segmentFn[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(identU.!).flatMap { name =>
      val isOpt = Utility.isPathOptional(name, ctx)
      val recvFnOpt: Option[Fn[Any]] =
        ctx.receiver match
          case Some(n: NamedReceiver) => Some(n.fn)
          case Some(e: ElementReceiver) => Some(e.fn)
          case Some(m: MethodReceiver) => Some(m.fn)
          case _ => None
      val gf = GetFn(name, isOptional = isOpt, recvFnOpt.getOrElse(NoOpFn), ctx.posStr)
      maybeIndex(gf) // <- now returns ParseFnResult
    }

  def methodChain[$: P](base: Fn[Any])(using ctx: ExprContext): P[ParseFnResult] = {
    // === 1) Context setup (unchanged) ===
    val (finalizedCtx: ExprContext) =
      base match {
        // only for top-level schema paths, NOT for "this" or "this.*"
        case GetFn(p, _, _, _) if !p.startsWith("this") =>
          val maybeField: Option[FieldType] = Utility.elementSchemaFor(p, ctx.schema)

          val childFields: List[FieldType] = maybeField match {
            case Some(c: ClassType) => c.fields
            case Some(l: ListType)  => l.elementType match {
              case c: ClassType => c.fields
              case _            => Nil
            }
            case _ => Nil
          }

          val withRecv0 = ctx.withReceiverFromPath(p)

          val withBaseBound: Either[DLCompileError, ExprContext] =
            withRecv0.map { ctx2 =>
              maybeField.fold(ctx2)(ft => ctx2.withVals(p -> ft))
            }

          val finalizedCtx: ExprContext =
            withBaseBound match
              case Right(ctx2) =>
                if childFields.nonEmpty then
                  ctx2.withVals(childFields.map(ft => ft.name -> ft)*)
                else
                  ctx2
              case Left(_) => ctx
          finalizedCtx

        // already in element scope or non-path
        case _ => ctx
      }

    given ExprContext = finalizedCtx
    val argsCtx: ExprContext = finalizedCtx

    // === 2) Recursive method-call folding ===
    def loop(current: Fn[Any]): P[Either[DLCompileError, Fn[Any]]] =
      P(methodCall).flatMap {
        case Left((err, name, off)) =>
          given ExprContext = ctx.copy(pos = off)
          val recvTypeResult = Utility.rhsType(current)
          val recvType = recvTypeResult match {
            case TypeResult.Known(rt) => rt
            case _ => ScalarType("", "scala.Any")
          }
          println(s"[loop] methodCall error: $err, receiver=$recvType, method=$name")

          val refined: DLCompileError = recvTypeResult match {
            case TypeResult.Known(rt) =>
              CompileFnRegistry.lookup(name) match {
                case Some(cfn) if !cfn.accepts(
                  NamedReceiver(
                    path = "anon",
                    ftype = rt,
                    fn = NoOpFn
                  )
                )(using ctx) =>
                  DLCompileError(ctx.posStrFrom(off), s"Method '$name' cannot be applied to receiver of type ${rt.typeName}")
                case _ => err
              }
            case _ => err
          }
          P(Pass(Left(refined)))

        case Right((name, args, off)) =>
          println(s"[loop] got method name: $name, args: $args, off: $off")
          CompileFnRegistry.lookup(name) match {
            case Some(cfn) =>
              println(s"[loop] found CompileFn for $name")
              given ExprContext = argsCtx.copy(pos = off)

              // ---- NEW: prefer context/schema (recvFieldTypeOpt), then fall back to rhsType ----
              val recvFieldTypeOpt: Option[FieldType] = current match
                case g: GetFn =>
                  val fromCtx = ctx.symbols.collectFirst { case scope if scope.contains(g.path) => scope(g.path) }
                  fromCtx.orElse(Utility.elementSchemaFor(g.path, ctx.schema))
                case _ =>
                  Utility.rhsType(current) match
                    case TypeResult.Known(ft) => Some(ft)
                    case _ => None

              // Normalize ValType → its underlying FieldType (so accepts() can match OptionType/ListType/MapType)
              val recvType: FieldType =
                recvFieldTypeOpt
                  .orElse(
                    Utility.rhsType(current) match
                      case TypeResult.Known(ft) => Some(ft)
                      case _ => None
                  )
                  .map {
                    case vt: ValType => vt.valueType // <-- important: unwrap ValType
                    case ft: FieldType => ft
                  }
                  .getOrElse(ScalarType("", "scala.Any"))

              println(s"[loop] recvType(for accepts)=$recvType for method '$name'")

              val recv = NamedReceiver(
                path = current match {
                  case g: GetFn => g.path
                  case _ => "anon"
                },
                ftype = recvType,
                fn = current
              )

              if !cfn.accepts(recv)(using argsCtx) then
                println(s"[loop] receiver type ${recvType.typeName} not accepted by $name")
                P(Pass(Left(DLCompileError(ctx.posStrFrom(off),
                  s"Method '$name' cannot be applied to receiver of type ${recvType.typeName}"
                ))))
              else {
                cfn.build(recv, args)(using argsCtx) match
                  case Left(e) =>
                    println(s"[loop] build for $name failed: $e")
                    P(Pass(Left(e)))
                  case Right(fnBuilt) =>
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
              P(Pass(Left(DLCompileError(ctx.posStrFrom(off), s"Unknown method: $name"))))
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
    P(Index ~ identifier.! ~ "(" ~/ WS0 ~ valueExpr.rep(sep = "," ~/ WS0) ~ WS0 ~ ")")
      .map { case (off, name, argsRaw) =>
        CompileFnRegistry.lookup(name) match {
          case Some(cfn) =>
            val (errs, oks) = argsRaw.partitionMap(identity)
            if errs.nonEmpty then Left(errs.head)
            else {
              val buildResult: Either[DLCompileError, Fn[Any]] =
                cfn.build(
                  NamedReceiver(
                    path = "anon",
                    ftype = ScalarType("", "scala.Any"),
                    fn = NoOpFn
                  ),
                  oks.toList
                )(using ctx).asInstanceOf[Either[DLCompileError, Fn[Any]]]

              buildResult.flatMap { fnBuilt =>
                cfn.validate(fnBuilt.asInstanceOf)(using ctx) match
                  case Left(vErr) => Left(vErr)
                  case Right(_)   => Right(fnBuilt.asInstanceOf[Fn[Any]])
              }
            }
          case None =>
            Left(DLCompileError(ctx.posStrFrom(off), s"Unknown function: $name"))
        }
      }

  // ---- Collection Statements ----

//  private trait CollectionMethodParser {
//    def name: String
//
//    def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult]
//  }

//  private def promoteToCollection(recv: Fn[Any])(using ctx: ExprContext): Fn[Any] = recv match
//    case g @ GetFn(name,_,_, _) =>
//      // If the bare name is in loop scope, prefer its collection binding `name[]`
//      val inLoop = ctx.symbols.headOption.exists(_.contains(name))
//      if inLoop then GetFn(s"$name", g.isOptional, NoOpFn, ctx.posStr) else g
//    case other => other

  def collectionStmt[$: P](using ctx: ExprContext): P[ParseStmtResult] =
    P(Index ~ path).flatMap { case (off0, basePath) =>
      // 1) Add a correctly-typed 'this' based on the base path
      val ctxWithThisEither = Utility.addThisType(basePath, ctx)

      val (ctxWithThis, maybeField, childFields): (ExprContext, Option[FieldType], List[FieldType]) =
        ctxWithThisEither match
          case Left(err) =>
            println(s"[collectionStmt] ⚠️ addThisType failed: ${err.msg}")
            (ctx, None, Nil)
          case Right(ctxWT) =>
            val maybeField = Utility.elementSchemaFor(basePath, ctxWT.schema)
            val childFields: List[FieldType] = maybeField match
              case Some(c: ClassType) => c.fields
              case Some(l: ListType) =>
                l.elementType match
                  case c: ClassType => c.fields
                  case _            => Nil
              case Some(ft) if ft.isOptional =>
                ft match
                  case ClassType(_, _, fields, _) => fields
                  case ListType(_, elem, _, _) =>
                    elem match
                      case c: ClassType => c.fields
                      case _            => Nil
                  case MapType(_, _, value, _, _) =>
                    value match
                      case c: ClassType => c.fields
                      case _            => Nil
                  case _ => Nil
              case _ => Nil
            (ctxWT, maybeField, childFields)

      val ctxForArgs =
        if childFields.nonEmpty then ctxWithThis.pushScope(childFields)
        else ctxWithThis

      // Build a GetFn for the collection receiver
//      val baseFn: Fn[Any] = GetFn(
//        basePath,
//        isOptional = Utility.isPathOptional(basePath, ctxWithThis),
//        recv = NoOpFn,
//        posStr = ctx.posStr
//      )

      given ExprContext = ctxForArgs

      // require a first method
      val firstMethodNameP: P[String] =
        P(WS0 ~ "." ~ identifier.!).flatMap { name =>
          P(&("(")).map(_ => name)
        }

      def parseOneMethod(name: String): P[ParseFnResult] =
        CompileFnRegistry.lookup(name) match
          case None =>
            P(Pass(Left(DLCompileError(ctx.posStr, s"Unknown method: $name"))))
          case Some(cfn) =>
            P("(" ~/ WS0 ~ valueExpr.rep(sep = "," ~/ WS0) ~ WS0 ~ ")").map { argsRaw =>
              val (errs, oks) = argsRaw.partitionMap(identity)
              if errs.nonEmpty then Left(errs.head)
              else {
                val buildResult = cfn.build(
                  NamedReceiver(
                    path = "anon",
                    ftype = ScalarType("", "scala.Any"),
                    fn = NoOpFn
                  ),
                  oks.toList
                )(using ctx)
                buildResult match {
                  case Left(e) => Left(e)
                  case Right(fnBuilt) =>
                    cfn.validate(fnBuilt.asInstanceOf)(using ctx) match
                      case Left(vErr) => Left(vErr)
                      case Right(_)   => Right(fnBuilt.asInstanceOf[Fn[Any]])
                }
              }
            }

      firstMethodNameP.flatMap { firstMethodName =>
        for {
          firstResult <- parseOneMethod(firstMethodName)
          moreNames <- P((WS0 ~ "." ~ identifier.!).flatMap(n => P(&("(")).map(_ => n))).rep
          restFns <- moreNames.foldLeft(Pass(Right(Nil)): P[Either[DLCompileError, List[Fn[Any]]]]) {
            case (accP, name) =>
              accP.flatMap {
                case Left(err) => P(Pass(Left(err)))
                case Right(acc) =>
                  parseOneMethod(name).map {
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
            val fn: Fn[Any] =
              if all.size == 1 then all.head
              else PolyFn(NoOpFn, all, ctx.posStr).asInstanceOf[Fn[Any]]
            (ctx, MapStmt(basePath, fn, ctx.posStr))
          }
        }
      }
    }
}
