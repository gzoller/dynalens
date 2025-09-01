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

//
// Second level:
//   arithmetic support
//   blocks (statement and fn)
//   if support
//
trait Level2 extends Level1 with ValueExprModule:

  // ---- Boolean ----

  /** Helpers to combine boolean results left-to-right, short-circuiting on the first Left */
  private inline def andCombine(a: ParseBoolResult, b: ParseBoolResult): ParseBoolResult =
    (a, b) match
      case (Left(e), _) => Left(e)
      case (Right(_), Left(e)) => Left(e)
      case (Right(a1), Right(b1)) => Right(AndFn(a1, b1))

  private inline def orCombine(a: ParseBoolResult, b: ParseBoolResult): ParseBoolResult =
    (a, b) match
      case (Left(e), _) => Left(e)
      case (Right(_), Left(e)) => Left(e)
      case (Right(a1), Right(b1)) => Right(OrFn(a1, b1))

  /** atom := '(' booleanExpr ')' | comparisonExpr | booleanLiteral | toBoolean(arithmeticExpr) */
  private def booleanAtom[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(
      WS0 ~ (
        "(" ~/ booleanExpr ~ ")" |
          comparisonExpr |
          booleanLiteral.map(b => Right(b): ParseBoolResult) |
          arithmeticExpr.map(_.map(toBooleanFn.apply): ParseBoolResult)
        )
    )

  /** booleanExpr := booleanAnd ('||' booleanAnd)* */
  def booleanExpr[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(booleanAnd ~ (WS0 ~ "||" ~ WS0 ~ booleanAnd).rep).map { (first, rest) =>
      rest.foldLeft(first)(orCombine)
    }

  /** booleanAnd := booleanNot ('&&' booleanNot)* */
  private def booleanAnd[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(booleanNot ~ (WS0 ~ "&&" ~ WS0 ~ booleanNot).rep).map { (first, rest) =>
      rest.foldLeft(first)(andCombine)
    }

  /** booleanNot := '!' booleanNot | atom */
  private def booleanNot[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(("!" ~ WS0 ~ booleanNot).map {
      case Right(b) => Right(NotFn(b)): ParseBoolResult
      case Left(e) => Left(e)
    } | booleanAtom)

  /** comparisonExpr := arithmeticExpr (==|!=|>=|<=|>|<) arithmeticExpr */
  private def comparisonExpr[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(
      arithmeticExpr ~ WS0 ~
        StringIn("==", "!=", ">=", "<=", ">", "<").! ~
        WS0 ~ arithmeticExpr
    ).map { case (lE, op, rE) =>
      for {
        left  <- lE
        right <- rE
      } yield op match
        case "==" => EqualFn(left, right)
        case "!=" => NotEqualFn(left, right)
        case ">=" => GreaterThanOrEqualFn(left, right)
        case "<=" => LessThanOrEqualFn(left, right)
        case ">"  => GreaterThanFn(left, right)
        case "<"  => LessThanFn(left, right)
    }

  // ---- Arithmetic ----

  private def arithmeticExpr[$: P](using ctx: ExprContext): P[ParseFnResult] =
    arithmeticTerm

  private def arithmeticTerm[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(Index ~ arithmeticFactor ~ (WS0 ~ CharIn("+\\-").! ~ WS0 ~ arithmeticFactor).rep).map {
      case (off, first, rest) =>
        rest.foldLeft(first) {
          case (Left(e), _) => Left(e) // short-circuit error
          case (Right(acc), (op, right)) =>
            right match {
              case Left(e) => Left(e)
              case Right(r) =>
                op match
                  case "+" => Right(AddFn(acc, r))
                  case "-" => Right(SubtractFn(acc, r))
                  case _ =>
                    Left(DLCompileError(off, s"Unsupported arithmetic operator: $op"))
            }
        }
    }

  private def arithmeticFactor[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(Index ~ unaryMinus ~ (WS0 ~ CharIn("*/%").! ~ WS0 ~ unaryMinus).rep).map {
      case (off, first, rest) =>
        rest.foldLeft(first) {
          case (Left(e), _) => Left(e) // propagate first error
          case (Right(acc), (op, right)) =>
            right match {
              case Left(e) => Left(e)
              case Right(r) =>
                op match
                  case "*" => Right(MultiplyFn(acc, r))
                  case "/" => Right(DivideFn(acc, r))
                  case "%" => Right(ModuloFn(acc, r))
                  case _ =>
                    Left(DLCompileError(off, s"Unsupported arithmetic operator: $op"))
            }
        }
    }

  private def arithmeticAtom[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(
      baseExpr | // already P[ParseFnResult]
        numberLiteral | // P[ParseFnResult]
        stringLiteral | // P[ParseFnResult]
        ("(" ~/ valueExpr ~ ")").flatMap {
          case Right(expr) => methodChain(expr) // attach trailing .methods to parenthesized expr
          case left@Left(_) => P(Pass(left))
        }
    )

  // support unary minus: -x
  private def unaryMinus[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P("-" ~/ WS0 ~ arithmeticAtom).map {
      case Right(fn) => Right(NegateFn(fn))
      case Left(err) => Left(err)
    } | arithmeticAtom

  // ---- String Concat ----

  private def concatExpr[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(
      arithmeticExpr ~
        (WS0 ~ "::" ~ WS0 ~ arithmeticExpr).rep ~
        &(WS0 ~ !CharIn("=<>!")) // Lookahead to reject comparisons
    ).map { case (firstE, restE) =>
      val all: List[ParseFnResult] = firstE :: restE.toList

      val (errs, oks) = all.partitionMap(identity)
      if errs.nonEmpty then
        Left(errs.head) // propagate first error
      else {
        val fns: List[Fn[Any]] = oks
        if fns.lengthCompare(1) == 0 then
          Right(fns.head) // a :: nothing => just the first
        else
          Right(ConcatFn(fns)) // a :: b :: c ...
      }
    }

  // ---- valueExpr => Top-Level Expr ----

  def valueExpr[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(
      ifFn |
        blockFn | // optional block expression
        concatExpr | // includes arithmeticExpr -> baseExpr (path+methods)
        booleanExpr.map {
          case Right(b) => Right(b.asInstanceOf[Fn[Any]]) // widen BooleanFn -> Fn[Any]
          case Left(err) => Left(err)
        }
    )

  def statementSeq[$: P](using ctx0: ExprContext): P[List[ParseStmtResult]] = {
    def loop(currentCtx: ExprContext): P[List[ParseStmtResult]] =
      P {
        given ExprContext = currentCtx

        statement.flatMap {
          case err@Left(_) =>
            // Fail-fast, return singleton error list
            Pass.map(_ => List(err))

          case Right((newCtx, stmt)) =>
            loop(newCtx).map(rest => Right((newCtx, stmt)) :: rest)
        } | Pass.map(_ => Nil) // end of sequence
      }

    loop(ctx0)
  }

  private def blockFn[$: P](using ctx: ExprContext): P[ParseFnResult] =
    // Parse "{", WS, and the threaded statements first
    P("{" ~/ WS0 ~ {
      given ExprContext = ctx;
      statementSeq
    }).flatMap { stmtResults =>
      // Fold statements: keep the latest threaded context and collect statements
      val folded: Either[DLCompileError, (ExprContext, List[Statement])] =
        stmtResults.foldLeft[Either[DLCompileError, (ExprContext, List[Statement])]](Right(ctx -> Nil)) {
          case (Left(e), _) => Left(e)
          case (_, Left(e)) => Left(e)
          case (Right((_, acc)), Right((newCtx, s))) =>
            // newCtx already contains all prior updates; no merge needed
            Right(newCtx -> (acc :+ s))
        }

      folded match
        // If statements failed, still consume the closing '}' so parser stays in sync
        case Left(err) =>
          P(WS0 ~ "}").map(_ => Left(err): ParseFnResult)

        case Right((finalCtx, stmts)) =>
          // Now parse the final expression under the *final* threaded context
          given ExprContext = finalCtx

          P(valueExpr).flatMap {
            case Left(e) =>
              P(WS0 ~ "}").map(_ => Left(e): ParseFnResult)

            case Right(fn) =>
              P(WS0 ~ "}").map(_ => Right(BlockFn(stmts, fn): Fn[Any]))
          }
    }

  // block { ... } as a *statement* block
  private def blockStmt[$: P](using ctx: ExprContext): P[ParseStmtResult] =
    P(
      "{" ~/ WS0 ~ {
        given ExprContext = ctx; statementSeq
      } ~
        WS0 ~ "}"
    ).map { stmtResults =>
      val folded: Either[DLCompileError, (ExprContext, List[Statement])] =
        stmtResults.foldLeft[Either[DLCompileError, (ExprContext, List[Statement])]](Right(ctx -> Nil)) {
          case (Left(e), _) => Left(e)
          case (_, Left(e)) => Left(e)
          case (Right((_, acc)), Right((newCtx, stmt))) =>
            // newCtx is already the threaded context after this stmt
            Right(newCtx -> (acc :+ stmt))
        }

      folded match {
        case Left(err) => Left(err)
        case Right((finalCtx, ss)) => Right((finalCtx, BlockStmt(ss)))
      }
    }

  private def statement[$: P](using ctx: ExprContext): P[ParseStmtResult] =
    P(
      WS0 ~ (
        valDecl |
        ifStmt |
        mapStmt |
        updateStmt |
        blockStmt |
        collectionStmt(booleanExpr)
      ) ~ WS0
    )

  // val x = <expr>
  private def valDecl[$: P](using ctx: ExprContext): P[ParseStmtResult] =
    P("val" ~/ WS ~ identifier.! ~ WS0 ~ "=" ~ WS0 ~ Index ~ valueExpr).map {
      case (name, offset, Right(vfn)) =>
        Utility.rhsType(vfn) match
          case Some(symT) =>
            // record the symbol in the lexical context; leave typeInfo (schema) untouched
            val newCtx = ctx.withVals(name -> symT)
            Right((newCtx, ValStmt(name, vfn)))
          case None =>
            Left(DLCompileError(offset,
              s"Unable to infer type for val '$name' from RHS: ${vfn.getClass.getSimpleName}"))

      case (_, _, Left(err)) =>
        Left(err)
    }

  // '=': always assignment
  private def updateStmt[$: P](using ctx: ExprContext): P[ParseStmtResult] =
    P(Index ~ pathBase ~ WS0 ~ "=" ~/ WS0 ~ Index).flatMap { case (pathOff, rawPath, rhsOff) =>
      CorrectPath.rewritePath(rawPath, pathOff) match {
        case Left(err) => P(Pass(Left(err)))
        case Right(cleanPath) =>
          val ctxForRhs = Utility.addThisType(cleanPath, ctx)

          given ExprContext = ctxForRhs

          P(valueExpr ~ WS0).map {
            case Left(e) => Left(e)
            case Right(vfn) =>
              val lhsSym = Utility.getPathType(cleanPath)
              val effLhs = Utility.effectiveLhsForAssignment(lhsSym, cleanPath) // <-- relax here
              Utility.rhsType(vfn) match {
                case None =>
                  Left(DLCompileError(rhsOff, s"Unable to infer type of RHS: ${vfn.getClass.getSimpleName}"))
                case Some(rhsSym) =>
                  if (Utility.areTypesCompatible(effLhs, rhsSym))
                    Right((ctx, UpdateStmt(cleanPath, vfn)))
                  else
                    Left(DLCompileError(rhsOff, s"Type mismatch: cannot assign $rhsSym to $lhsSym at $cleanPath"))
              }
          }
      }
    }

  // Parses: ( <expr> , <expr> )
  private def pairExpr[$: P](using ctx: ExprContext): P[Either[DLCompileError, (Fn[Any], Fn[Any])]] =
    P("(" ~/ WS0 ~ valueExpr ~ WS0 ~ "," ~ WS0 ~ valueExpr ~ WS0 ~ ")").map {
      case (Left(e1), _)  => Left(e1)
      case (_, Left(e2))  => Left(e2)
      case (Right(a: Fn[Any] @unchecked), Right(b: Fn[Any] @unchecked)) =>
        Right((a, b))
    }

  // exactly like blockFn but forcing the final expression to be pairExpr
  private def blockPairFn[$: P](using ctx0: ExprContext): P[ParseFnResult] =
    P("{" ~/ WS0).flatMap { _ =>
      given ExprContext = ctx0
      statementSeq.flatMap { stmtsE =>
        val folded =
          stmtsE.foldLeft[Either[DLCompileError, (ExprContext, List[Statement])]](Right(ctx0 -> Nil)) {
            case (Left(err), _)                               => Left(err)
            case (_, Left(err))                               => Left(err)
            case (Right((accCtx, ss)), Right((newCtx, stmt))) => Right(accCtx.merge(newCtx) -> (ss :+ stmt))
          }

        folded match {
          case Left(e) => P(Pass(Left(e)))
          case Right((finalCtx, ss)) =>
            given ExprContext = finalCtx
            P(pairExpr ~ WS0 ~ "}").map {
              case Left(err)           => Left(err)
              case Right(pair) =>
                val (kFn, vFn) = pair   // already (Fn[Any], Fn[Any]) from pairExpr
                Right(BlockFn(ss, Tuple2Fn(kFn, vFn)): Fn[Any])
            }
        }
      }
    }

  // '=>': always map
  private def mapStmt[$: P](using ctx: ExprContext): P[ParseStmtResult] =
    P(Index ~ pathBase ~ WS0 ~ "=>" ~/ WS0 ~ Index).flatMap { case (pathOff, rawPath, rhsOff) =>
      CorrectPath.rewritePath(rawPath, pathOff) match
        case Left(err) => P(Pass(Left(err)))

        case Right(cleanPath) =>
          val lhsSym: SymbolType = Utility.getPathType(cleanPath)(using ctx)
          val hasList: Boolean   = Utility.hasListSegment(cleanPath)

          // Start from original ctx
          val baseRhsCtx: ExprContext =
            lhsSym match {
              case SymbolType.Map | SymbolType.OptionalMap =>
                ctx.withReceiver(Utility.mapEntryReceiverFor(cleanPath)(using ctx))
              case _ if hasList =>
                // element receiver + loop scopes you already compute
                val recvCtx = ctx.withReceiverFromPath(cleanPath)
                val loopMap = Utility.loopScopesFor(cleanPath, ctx.typeInfo) // you already have this
                recvCtx.pushScope(loopMap)
              case SymbolType.OptionalScalar =>
                ctx.withVals("this" -> SymbolType.Scalar)
              case _ =>
                ctx
            }

          // push the nearest container field map so bare names like `shipments` resolve
          val containerScope = Utility.containerFieldsFor(cleanPath, ctx.typeInfo)
          val ctxForRhs =
            if (containerScope.nonEmpty) baseRhsCtx.pushScope(containerScope)
            else baseRhsCtx

          given ExprContext = ctxForRhs

          lhsSym match
            case SymbolType.Map | SymbolType.OptionalMap =>
              val pairAsFn: P[Either[DLCompileError, Fn[Any]]] =
                P(
                  blockPairFn |
                    pairExpr.map {
                      case Left(e)       => Left(e)
                      case Right((k, v)) => Right(Tuple2Fn(k, v): Fn[Any])
                    }
                )

              P(pairAsFn ~ WS0).map {
                case Left(e)        => Left(e)
                case Right(bodyFn)  => Right((ctx, MapStmt(cleanPath, bodyFn)))
              }

            case _ =>
              P(valueExpr ~ WS0).map {
                case Left(e) => Left(e)
                case Right(vfn) =>
                  val isListLike = lhsSym == SymbolType.List || lhsSym == SymbolType.OptionalList
                  val normalizedLhs =
                    if (isListLike) Utility.addWildcardToListLike(cleanPath) else cleanPath
                  val body: Fn[?] =
                    if (isListLike) LoopFn(vfn) else vfn
                  Right((ctx, MapStmt(normalizedLhs, body)))
              }
    }

  private def ifStmt[$: P](using ctx: ExprContext): P[ParseStmtResult] =
    P(
      "if" ~/ WS ~ booleanExpr ~ WS0 ~
        "then" ~ WS0 ~ statement ~
        (WS0 ~ "else" ~ WS0 ~ statement).?
    ).map {
      case (condRes, thenRes, elseOptRes) =>
        val base: Either[DLCompileError, (BooleanFn, (ExprContext, Statement))] =
          for
            c <- condRes
            t <- thenRes
          yield (c, t)

        elseOptRes match
          case None =>
            base.map { case (c, (_, tStmt)) =>
              (ctx, IfStmt(c, tStmt, None))
            }

          case Some(er) =>
            for
              (c, (_, tStmt)) <- base
              (_, eStmt) <- er
            yield (ctx, IfStmt(c, tStmt, Some(eStmt)))
    }

  private def ifFn[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(
      "if" ~/ WS ~ booleanExpr ~ WS0 ~
        "then" ~/ WS0 ~ valueExpr ~ WS0 ~
        "else" ~/ WS0 ~ valueExpr
    ).map {
      case (condRes, thenRes, elseRes) =>
        for
          c <- condRes
          t <- thenRes
          e <- elseRes
        yield IfFn(c, t, e)
    }