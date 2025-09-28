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
      case (Left(e), _)           => Left(e)
      case (Right(_), Left(e))    => Left(e)
      case (Right(a1), Right(b1)) => Right(AndFn(a1, b1))

  private inline def orCombine(a: ParseBoolResult, b: ParseBoolResult): ParseBoolResult =
    (a, b) match
      case (Left(e), _)           => Left(e)
      case (Right(_), Left(e))    => Left(e)
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
    P(booleanAnd ~ (WS0 ~ "||" ~ WS0 ~ booleanAnd).rep).map {
      case (first, rest) =>
        rest.foldLeft(first) {
          case (Left(e), _) => Left(e)
          case (Right(acc), Right(rhs)) => COrFn.build(List(acc, rhs))
          case (Right(_), Left(e)) => Left(e)
          case (Left(e), Right(_)) => Left(e)
        }
    }

  /** booleanAnd := booleanNot ('&&' booleanNot)* */
  private def booleanAnd[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(booleanNot ~ (WS0 ~ "&&" ~ WS0 ~ booleanNot).rep).map {
      case (first, rest) =>
        rest.foldLeft(first) {
          case (Left(e), _) => Left(e)
          case (Right(acc), Right(rhs)) => CAndFn.build(List(acc, rhs))
          case (Right(_), Left(e)) => Left(e)
          case (Left(e), Right(_)) => Left(e)
        }
    }

  /** booleanNot := '!' booleanNot | atom */
  private def booleanNot[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(("!" ~ WS0 ~ booleanNot).map {
      case Right(b) => CNotFn.build(List(b))
      case Left(e) => Left(e)
    } | booleanAtom)

  /** comparisonExpr := arithmeticExpr (==|!=|>=|<=|>|<) arithmeticExpr */
  private def comparisonExpr[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(arithmeticExpr ~ WS0 ~ StringIn("==", "!=", ">=", "<=", ">", "<").! ~ WS0 ~ arithmeticExpr)
      .map { case (lE, op, rE) =>
        for {
          left  <- lE
          right <- rE
          _ = {
            println(s"[comparisonExpr] building DeferredCompare op=$op")
            println(s"[comparisonExpr] left=$left, left.recv=${left.recv}")
            println(s"[comparisonExpr] right=$right, right.recv=${right.recv}")
            println(s"[comparisonExpr] ctx.receiver=${ctx.receiver}")
          }
        } yield DeferredCompare(op, left, right)
      }
//  private def comparisonExpr[$: P](using ctx: ExprContext): P[ParseBoolResult] =
//    P(
//      arithmeticExpr ~ WS0 ~
//        StringIn("==", "!=", ">=", "<=", ">", "<").! ~
//        WS0 ~ arithmeticExpr
//    ).flatMap { case (lE, op, rE) =>
//      (lE, rE) match
//        case (Right(left), Right(right)) =>
//          op match
//            case "==" => P(Pass(Right(EqualFn(left, right): BooleanFn)))
//            case "!=" => P(Pass(Right(NotEqualFn(left, right): BooleanFn)))
//            case ">=" => P(Pass(Right(GreaterThanOrEqualFn(left, right): BooleanFn)))
//            case "<=" => P(Pass(Right(LessThanOrEqualFn(left, right): BooleanFn)))
//            case ">"  => P(Pass(CGreaterThanFn.build(List(left, right))))
//            case "<"  => P(Pass(Right(LessThanFn(left, right): BooleanFn)))
//
//        case (Left(e), _) => P(Pass(Left(e)))
//        case (_, Left(e)) => P(Pass(Left(e)))
//    }

  // ---- Arithmetic ----

  private def arithmeticExpr[$: P](using ctx: ExprContext): P[ParseFnResult] =
    arithmeticTerm

  private def arithmeticTerm[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(Index ~ arithmeticFactor ~ (WS0 ~ CharIn("+\\-").! ~ WS0 ~ arithmeticFactor).rep).map {
      case (off, first, rest) =>
        rest.foldLeft(first) {
          case (Left(e), _) => Left(e) // stop on earlier error
          case (Right(acc), (op, rightE)) =>
            rightE.flatMap { r =>
              op match
                case "+" => CAddFn.build(List(acc, r))
                case "-" => CSubtractFn.build(List(acc, r))
            }
        }
    }

  private def arithmeticFactor[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(Index ~ unaryMinus ~ (WS0 ~ CharIn("*/%").! ~ WS0 ~ unaryMinus).rep).map {
      case (off, first, rest) =>
        rest.foldLeft(first) {
          case (Left(e), _) => Left(e)
          case (Right(acc), (op, rightE)) =>
            rightE.flatMap { r =>
              op match
                case "*" => CMultiplyFn.build(List(acc, r))
                case "/" => CDivideFn.build(List(acc, r))
                case "%" => CModuloFn.build(List(acc, r))
            }
        }
    }

  private def arithmeticAtom[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(
      baseExpr | // already P[ParseFnResult]
        numberLiteral | // P[ParseFnResult]
        stringLiteral | // P[ParseFnResult]
        ("(" ~/ valueExpr ~ ")").flatMap {
          case Right(expr)    => methodChain(expr) // attach trailing .methods to parenthesized expr
          case left @ Left(_) => P(Pass(left))
        }
    )

  // support unary minus: -x
  private def unaryMinus[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P("-" ~/ WS0 ~ arithmeticAtom).flatMap {
      case Right(fn)  => P(Pass(CNegateFn.build(List(fn))))
      case Left(err)  => P(Pass(Left(err)))
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
      if errs.nonEmpty then Left(errs.head) // propagate first error
      else {
        val fns: List[Fn[Any]] = oks
        if fns.lengthCompare(1) == 0 then Right(fns.head) // a :: nothing => just the first
        else Right(ConcatFn(fns)) // a :: b :: c ...
      }
    }

  // ---- valueExpr => Top-Level Expr ----

  def valueExpr[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(
      ifFn |
        blockFn |
        concatExpr | // this already handles arithmetic and path+method stuff
        booleanExpr
    ).flatMap {
      case Left(err)   => P(Pass.map(_ => Left(err)))
      case Right(base) => maybeCaseTail(base)
    }

  def statementSeq[$: P](using ctx0: ExprContext): P[List[ParseStmtResult]] = {
    def loop(currentCtx: ExprContext): P[List[ParseStmtResult]] =
      P {
        given ExprContext = currentCtx

        statement.flatMap {
          case err @ Left(_) =>
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
          case (Left(e), _)                          => Left(e)
          case (_, Left(e))                          => Left(e)
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
          case (Left(e), _)                             => Left(e)
          case (_, Left(e))                             => Left(e)
          case (Right((_, acc)), Right((newCtx, stmt))) =>
            // newCtx is already the threaded context after this stmt
            Right(newCtx -> (acc :+ stmt))
        }

      folded match {
        case Left(err)             => Left(err)
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
        val maybeFt = Utility.rhsType(vfn)
        maybeFt match
          case Some(ft: FieldType) =>
            // ensure ScalarType carries the val name, so later assignments match cleanly
            val ftNamed = ft match
              case s: ScalarType if s.name.isEmpty => s.copy(name = name)
              case other                           => other
            val valFt = ValType(name, ftNamed, ftNamed.typeName)
            val newCtx = ctx.withVals(name -> valFt)
            Right((newCtx, ValStmt(name, vfn)))

          case None =>
            val reason = vfn match
              case g: GetFn =>
                s"Unknown field path '${g.path}'"
              case m: Fn[?] =>
                val recvTypeStr =
                  m.recv.flatMap(Utility.rhsType)
                    .map(_.typeName)
                    .getOrElse("unknown")
                s"Method '${m.methodName}' cannot be applied to receiver of type $recvTypeStr"
              case _ =>
                vfn.getClass.getSimpleName
            Left(DLCompileError(offset, reason))

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
              println(s"[updateStmt] lhsSym for $cleanPath = $lhsSym")

              val effLhs = Utility.effectiveLhsForAssignment(lhsSym, cleanPath)

              Utility.rhsType(vfn) match {
                case None =>
                  Left(DLCompileError(rhsOff, s"Unable to infer type of RHS: ${vfn.getClass.getSimpleName}"))

                case Some(rhsSym0) =>
                  // --- unwrap ValType on RHS for assignment comparison ---
                  val effRhs: FieldType = rhsSym0 match {
                    case v: ValType =>
                      println(s"[updateStmt] RHS is ValType -> unwrapping to ${v.valueType}")
                      v.valueType
                    case other => other
                  }

                  println(s"[updateStmt] effLhs = $effLhs")
                  println(s"[updateStmt] effRhs = $effRhs")

                  val ok = Utility.areTypesCompatible(effLhs, effRhs)
                  println(s"[updateStmt] areTypesCompatible? $ok")

                  if ok then Right((ctx, UpdateStmt(cleanPath, vfn)))
                  else {
                    val lStr = Utility.prettyFieldType(effLhs)
                    val rStr = Utility.prettyFieldType(effRhs)
                    Left(DLCompileError(rhsOff, s"Type mismatch: cannot assign $rStr to $lStr at $cleanPath"))
                  }
              }
          }
      }
    }

  // Parses: ( <expr> , <expr> )
  private def pairExpr[$: P](using ctx: ExprContext): P[Either[DLCompileError, (Fn[Any], Fn[Any])]] =
    P("(" ~/ WS0 ~ valueExpr ~ WS0 ~ "," ~ WS0 ~ valueExpr ~ WS0 ~ ")").map {
      case (Left(e1), _) => Left(e1)
      case (_, Left(e2)) => Left(e2)
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
              case Left(err) => Left(err)
              case Right(pair) =>
                val (kFn, vFn) = pair // already (Fn[Any], Fn[Any]) from pairExpr
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
          // new API: returns Option[FieldType]
          val lhsFt: FieldType = Utility.getPathType(cleanPath)(using ctx)
          val hasList: Boolean         = Utility.hasListSegment(cleanPath)

          // Start from original ctx and branch on the actual FieldType
          val baseRhsCtx: ExprContext = lhsFt match
            case m: MapType =>
              ctx.withReceiver(Utility.mapEntryReceiverFor(cleanPath)(using ctx))

            case o: OptionType if o.valueType.isInstanceOf[MapType] =>
              ctx.withReceiver(Utility.mapEntryReceiverFor(cleanPath)(using ctx))

            case l: ListType =>
              ctx.withReceiverFromPath(cleanPath)

            case o: OptionType if o.valueType.isInstanceOf[ListType] =>
              ctx.withReceiverFromPath(cleanPath)

            case o: OptionType if o.valueType.isInstanceOf[ScalarType] =>
              ctx.withVals("this" -> o.valueType)

            case _ =>
              ctx

          // If you still need container-level fields for completions, implement a new helper
          // based on ClassType traversal. For now simply:
          val containerScope: Map[String, FieldType] =
            Utility.containerFieldsFor(ctx.schema, cleanPath)
              .map(ft => ft.name -> ft)
              .toMap

          val ctxForRhs =
            if containerScope.nonEmpty then
              baseRhsCtx.pushScope(containerScope.values.toList)
            else
              baseRhsCtx

          given ExprContext = ctxForRhs

          lhsFt match
            // LHS is a Map or an Option[Map]
            case m: MapType =>
              val pairAsFn: P[Either[DLCompileError, Fn[Any]]] =
                P(
                  blockPairFn |
                    pairExpr.map {
                      case Left(e)       => Left(e)
                      case Right((k, v)) => Right(Tuple2Fn(k, v): Fn[Any])
                    }
                )

              P(pairAsFn ~ WS0).map {
                case Left(e)       => Left(e)
                case Right(bodyFn) => Right((ctx, MapStmt(cleanPath, bodyFn)))
              }

            case o: OptionType if o.valueType.isInstanceOf[MapType] =>
              val pairAsFn: P[Either[DLCompileError, Fn[Any]]] =
                P(
                  blockPairFn |
                    pairExpr.map {
                      case Left(e)       => Left(e)
                      case Right((k, v)) => Right(Tuple2Fn(k, v): Fn[Any])
                    }
                )

              P(pairAsFn ~ WS0).map {
                case Left(e)       => Left(e)
                case Right(bodyFn) => Right((ctx, MapStmt(cleanPath, bodyFn)))
              }

            case _ =>
              P(valueExpr ~ WS0).map {
                case Left(e) => Left(e)
                case Right(vfn) =>
                  // true if the LHS is a List or an Option of List
                  val isListLike = lhsFt match {
                    case _: ListType => true
                    case OptionType(_, inner: ListType, _) => true
                    case _ => false
                  }

                  val normalizedLhs =
                    if isListLike then Utility.addWildcardToListLike(cleanPath)
                    else cleanPath

                  val body: Fn[?] =
                    if isListLike then LoopFn(vfn)
                    else vfn

                  Right((ctx, MapStmt(normalizedLhs, body)))
              }
    }

  private def ifStmt[$: P](using ctx: ExprContext): P[ParseStmtResult] =
    P(
      "if" ~/ WS ~ booleanExpr ~ WS0 ~
        "then" ~ WS0 ~ statement ~
        (WS0 ~ "else" ~ WS0 ~ statement).?
    ).map { case (condRes, thenRes, elseOptRes) =>
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
    ).map { case (condRes, thenRes, elseRes) =>
      for
        c <- condRes
        t <- thenRes
        e <- elseRes
      yield IfFn(c, t, e)
    }

  private def defaultCase[$: P](using ctx: ExprContext): P[Either[DLCompileError, (String, Fn[Any])]] =
    P("default" ~ WS0 ~ "->" ~ WS0 ~ valueExpr).map {
      case Left(e)   => Left(e)
      case Right(fn) => Right("__default__" -> fn)
    }

  private def normalCase[$: P](using ctx: ExprContext): P[Either[DLCompileError, (Any, Fn[Any])]] =
    P(literalValue ~ WS0 ~ "->" ~ WS0 ~ valueExpr).map {
      case (Left(e), _)            => Left(e)
      case (_, Left(e))            => Left(e)
      case (Right(key), Right(fn)) => Right(key -> fn)
    }

  // Parse whole block: { line (\n line)* [\n default -> expr] }
  private def caseBlock[$: P](using ctx: ExprContext): P[Either[DLCompileError, (Vector[(Any, Fn[Any])], Option[Fn[Any]])]] =
    P(
      "{" ~ WS0 ~
        normalCase.rep(sep = WS0) ~
        defaultCase.? ~
        WS0 ~ "}"
    ).map { (caseLines, maybeDefault) =>
      val errors = caseLines.collect { case Left(e) => e }
      if errors.nonEmpty then Left(errors.head)
      else
        val regularCases = caseLines.collect { case Right((k, v)) => (k, v) }

        maybeDefault match
          case Some(Left(e)) =>
            Left(e)
          case Some(Right((key, fn))) if key != "__default__" =>
            // Should never happen, but defensive
            Left(DLCompileError(implicitly[ParsingRun[?]].index, s"Expected 'default', found: $key"))
          case Some(Right((_, fn))) =>
            Right((regularCases.toVector, Some(fn)))
          case None =>
            Right((regularCases.toVector, None))
    }

  private def permissiveMode[$: P]: P[Boolean] =
    P("(" ~ WS0 ~ "permissive" ~ WS0 ~ ")").map(_ => true).?.map(_.getOrElse(false))

  // after you produce a base: P(valueExprCore).flatMap { base => ... }
  private def maybeCaseTail[$: P](base: Fn[Any])(using ctx: ExprContext): P[Either[DLCompileError, Fn[Any]]] =
    P(WS0 ~ "case" ~ WS0 ~ permissiveMode ~ WS0 ~ caseBlock).?.map {
      case None =>
        Right(base)

      case Some((permissive, Right((pairs, df)))) =>
        val bad = pairs.collectFirst {
          case (p, _)
              if !(p.isInstanceOf[String] || p.isInstanceOf[Boolean] || p.isInstanceOf[Byte] ||
                p.isInstanceOf[Short] || p.isInstanceOf[Int] || p.isInstanceOf[Long] ||
                p.isInstanceOf[Float] || p.isInstanceOf[Double]) =>
            s"case pattern must be a literal (string/number/boolean), got: ${p.getClass.getSimpleName}"
        }

        bad match {
          case Some(msg) =>
            Left(DLCompileError(implicitly[ParsingRun[?]].index, msg))
          case None =>
            Right(CaseWhenFn(base, pairs, df, permissive))
        }

      case Some((_, Left(e))) =>
        Left(e)
    }
