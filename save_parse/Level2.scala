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

import fastparse.*
import NoWhitespace.*
import fn.*

//
// Second level:
//   arithmetic support
//   blocks (statement and fn)
//   if support
//
trait Level2 extends Level1 with ValueExprModule:

  // ---- Boolean ----

  /** Helpers to combine boolean results left-to-right, short-circuiting on the first Left */
  private inline def andCombine(a: ParseBoolResult, b: ParseBoolResult)(using ctx: ExprContext): ParseBoolResult =
    (a, b) match
      case (Left(e), _) => Left(e)
      case (Right(_), Left(e)) => Left(e)
      case (Right(a1), Right(b1)) =>
        CompileFnRegistry.lookup("&&") match
          case Some(cfn) =>
            cfn.build(NoOpFn, List(a1.asInstanceOf[Fn[Any]], b1.asInstanceOf[Fn[Any]])) match
              case Left(err) => Left(err)
              case Right(fn) =>
                cfn.validate(fn)(using ctx).map(_ => fn.asInstanceOf[BooleanFn])
          case None =>
            Left(DLCompileError(0, "Missing CompileFn for &&"))

  private inline def orCombine(a: ParseBoolResult, b: ParseBoolResult)(using ctx: ExprContext): ParseBoolResult =
    (a, b) match
      case (Left(e), _) => Left(e)
      case (Right(_), Left(e)) => Left(e)
      case (Right(a1), Right(b1)) =>
        CompileFnRegistry.lookup("||") match
          case Some(cfn) =>
            cfn.build(NoOpFn, List(a1.asInstanceOf[Fn[Any]], b1.asInstanceOf[Fn[Any]])) match
              case Left(err) => Left(err)
              case Right(fn) =>
                cfn.asInstanceOf[CompileFn[Fn[Any]]]
                  .validate(fn.asInstanceOf[Fn[Any]])(using ctx)
                  .map(_ => fn.asInstanceOf[BooleanFn])
          case None =>
            Left(DLCompileError(0, "Missing CompileFn for ||"))

  /** atom := '(' booleanExpr ')' | comparisonExpr | booleanLiteral | ToBoolean(arithmeticExpr) */
  private def booleanAtom[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(
      WS0 ~ (
        "(" ~/ booleanExpr ~ ")" |
          comparisonExpr |
          booleanLiteral.map(b => Right(b): ParseBoolResult) |
          arithmeticExpr.map(_.map(ToBooleanFn.apply): ParseBoolResult)
      )
    )

  /** booleanExpr := booleanAnd ('||' booleanAnd)* */
  def booleanExpr[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(booleanAnd ~ (WS0 ~ "||" ~ WS0 ~ booleanAnd).rep).map {
      case (first, rest) =>
        rest.foldLeft(first) {
          case (Left(e), _) => Left(e)
          case (Right(acc), Right(rhs)) =>
            // Phase 1: build
            COrFn.build(NoOpFn, List(acc.asInstanceOf[Fn[Any]], rhs.asInstanceOf[Fn[Any]])) match
              case Left(err) => Left(err)
              case Right(fn) =>
                // Phase 2: validate
                COrFn.validate(fn)(using ctx).map(_ => fn)
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
          case (Right(acc), Right(rhs)) =>
            // Phase 1: build
            CAndFn.build(NoOpFn, List(acc.asInstanceOf[Fn[Any]], rhs.asInstanceOf[Fn[Any]])) match
              case Left(err) => Left(err)
              case Right(fn) =>
                // Phase 2: validate
                CAndFn.validate(fn)(using ctx).map(_ => fn)
          case (Right(_), Left(e)) => Left(e)
          case (Left(e), Right(_)) => Left(e)
        }
    }

  /** booleanNot := '!' booleanNot | atom */
  private def booleanNot[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(
      ("!" ~ WS0 ~ booleanNot).map {
        case Right(b) =>
          // Phase 1: build
          CNotFn.build(NoOpFn, List(b.asInstanceOf[Fn[Any]])) match
            case Left(err) => Left(err)
            case Right(fn) =>
              // Phase 2: validate
              CNotFn.validate(fn)(using ctx).map(_ => fn)
        case Left(e) => Left(e)
      } | booleanAtom
    )

  /** comparisonExpr := arithmeticExpr (==|!=|>=|<=|>|<) arithmeticExpr */
  private def comparisonExpr[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(arithmeticExpr ~ WS0 ~ StringIn("==", "!=", ">=", "<=", ">", "<").! ~ WS0 ~ arithmeticExpr)
      .map { case (lE, op, rE) =>
        // Pick the comparison function first
        val cfn: CompileFn[?] = op match
          case ">"  => CGreaterThanFn
          case ">=" => CGreaterThanOrEqualFn
          case "<"  => CLessThanFn
          case "<=" => CLessThanOrEqualFn
          case "==" => CEqualFn
          case "!=" => CNotEqualFn

        for
          left  <- lE
          right <- rE
          built <- cfn
            .build(NoOpFn, List(left, right))
            .asInstanceOf[Either[DLCompileError, BooleanFn]]
          _     <- cfn.validate(built)(using ctx)
        yield built
      }

  // ---- Arithmetic ----

  private def arithmeticExpr[$: P](using ctx: ExprContext): P[ParseFnResult] =
    arithmeticTerm

  private def arithmeticTerm[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(Index ~ arithmeticFactor ~ (WS0 ~ CharIn("+\\-").! ~ WS0 ~ arithmeticFactor).rep).map {
      case (off, first, rest) =>
        def containsIllegalThis(fn: Fn[?]): Boolean =
          fn match
            case GetFn("this", _, _) if ctx.receiver.isEmpty => true
            case _ => false

        // Check top-level illegal use immediately
        first match
          case Right(fn) if containsIllegalThis(fn) =>
            Left(DLCompileError(off, "Use of 'this' with no receiver in scope"))
          case _ =>
            rest.foldLeft(first) {
              case (Left(e), _) => Left(e)
              case (Right(acc), (op, rightE)) =>
                rightE.flatMap { r =>
                  // Catch illegal RHS
                  if containsIllegalThis(r) then
                    Left(DLCompileError(off, "Use of 'this' with no receiver in scope"))
                  else
                    op match
                      case "+" =>
                        for
                          built <- CPlusFn.build(NoOpFn, List(acc, r))
                          _ <- CPlusFn.validate(built)(using ctx)
                        yield built

                      case "-" =>
                        for
                          built <- CMinusFn.build(NoOpFn, List(acc, r))
                          _ <- CMinusFn.validate(built)(using ctx)
                        yield built
                }
            }
    }

  private def arithmeticFactor[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(arithmeticAtom ~ (WS0 ~ CharIn("*/%").! ~ WS0 ~ arithmeticAtom).rep).map {
      case (first, rest) =>
        def containsIllegalThis(fn: Fn[?]): Boolean =
          fn match
            case GetFn("this", _, _) if ctx.receiver.isEmpty => true
            case _ => false

        first match
          case Right(fn) =>
            if containsIllegalThis(fn) then
              println(s"[arithFactor] ❌ Illegal top-level 'this' detected")
              Left(DLCompileError(0, "Use of 'this' with no receiver in scope"))
            else
              rest.foldLeft(Right(fn): ParseFnResult) {
                case (Left(e), _) => Left(e)
                case (Right(acc), (op, rightE)) =>
                  rightE.flatMap { r =>
                    if containsIllegalThis(r) then
                      println(s"[arithFactor] ❌ Illegal RHS 'this' detected")
                      Left(DLCompileError(0, "Use of 'this' with no receiver in scope"))
                    else
                      op match
                        case "*" =>
                          for
                            built <- CMultiplyFn.build(NoOpFn, List(acc, r))
                            _ <- CMultiplyFn.validate(built)(using ctx)
                          yield built
                        case "/" =>
                          for
                            built <- CDivideFn.build(NoOpFn, List(acc, r))
                            _ <- CDivideFn.validate(built)(using ctx)
                          yield built
                        case "%" =>
                          for
                            built <- CModulusFn.build(NoOpFn, List(acc, r))
                            _ <- CModulusFn.validate(built)(using ctx)
                          yield built
                  }
              }

          case Left(err) =>
            println(s"[arithFactor] ❌ Early error: $err")
            Left(err)
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
      case Right(fn) =>
        (for {
          built <- CMinusFn.build(NoOpFn, List(fn))   // CMinusFn handles unary
          _     <- CMinusFn.validate(built)(using ctx)
        } yield built) match
          case ok @ Right(_) => P(Pass(ok))
          case err @ Left(_) => P(Pass(err))

      case Left(err) =>
        P(Pass(Left(err)))
    } | arithmeticAtom

  // ---- String Concat ----

  private def consExpr[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(
      arithmeticExpr ~ (WS0 ~ "::" ~ WS0 ~ arithmeticExpr).rep ~
        &(WS0 ~ !CharIn("=<>!")) // prevent mis-parsing into comparison
    ).map { case (firstE, restE) =>
      // if there was no "::", just return lhs unchanged
      if restE.isEmpty then
        firstE
      else {
        val all = firstE :: restE.toList
        val (errs, oks) = all.partitionMap(identity)

        if errs.nonEmpty then Left(errs.head)
        else {
          val fns: List[Fn[Any]] = oks.map {
            case GetFn("Nil", _, _) =>
              ConstantFn[List[Any]](Nil).asInstanceOf[Fn[Any]]
            case other => other
          }

          val consTree: Fn[Any] =
            fns.reduceRight[Fn[Any]] { (h, t) =>
              ConsFn(h, t).asInstanceOf[Fn[Any]]
            }

          for {
            _ <- CConsFn.validate(consTree)(using ctx)
          } yield consTree
        }
      }
    }

  // ---- valueExpr => Top-Level Expr ----

  def valueExpr[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(
      ifFn |
        blockFn |
        consExpr | // this already handles arithmetic and path+method stuff
        booleanExpr
    ).flatMap {
      case Left(err)   => P(Pass.map(_ => Left(err)))
      case Right(base) => maybeCaseTail(base.asInstanceOf[Fn[Any]])
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
          collectionStmt
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

  /** Ensure that any `[i]` on the LHS is applied to a list or Option[List[_]]. */
  private def enforceIndexedBaseIsCollection(path: String, off: Int)
                                            (using ctx: ExprContext): Either[DLCompileError, Unit] = {
    import co.blocke.dynalens.Path

    val parts = Path.parsePath(path)

    // Find the *first* indexed segment and reconstruct its base path prefix
    val idxPos = parts.indexWhere {
      case _: Path.IndexedField => true
      case _ => false
    }

    if (idxPos < 0) Right(()) // no indexing present
    else {
      val baseParts = parts.take(idxPos) :+ parts(idxPos) // include the indexed field’s name
      val baseName = baseParts.last match {
        case Path.IndexedField(n, _, _) => n
        case Path.Field(n, _)           => n
      }

      // Build the dotted prefix up to (and including) the indexed field name
      val basePath = baseParts.map {
        case Path.Field(n, _)           => n
        case Path.IndexedField(n, _, _) => n
      }.mkString(".")

      val rawType = Utility.getPathType(basePath)
      println(s"[enforceIndexedBaseIsCollection] basePath=$basePath rawType=$rawType (${rawType.getClass.getName})")
      val effectiveType = rawType match
        case o: OptionType if o.valueType.isInstanceOf[ListType] =>
          o.valueType.asInstanceOf[ListType]
        case other => other

      println(s"[enforceIndexedBaseIsCollection] basePath=$basePath  rawType=$rawType  effective=$effectiveType")

      effectiveType match {
        case _: ListType => Right(())
        case _           => Left(DLCompileError(off, s"Cannot index into non-list field '$baseName'"))
      }
    }
  }

  // '=': always assignment
  private def updateStmt[$: P](using ctx: ExprContext): P[ParseStmtResult] =
    P(Index ~ pathBase ~ WS0 ~ "=" ~/ WS0 ~ Index).flatMap { case (pathOff, rawPath, rhsOff) =>
      println(s"[updateStmt] starting parse, raw path: $rawPath, rewritten: ${CorrectPath.rewritePath(rawPath, pathOff)}")

      CorrectPath.rewritePath(rawPath, pathOff) match {
        case Left(err) => P(Pass(Left(err)))
        case Right(cleanPath) =>
          enforceIndexedBaseIsCollection(cleanPath, pathOff) match {
            case Left(e) => P(Pass(Left(e)))
            case Right(_) =>

              // --- enrich context for RHS (so `this` and nested fields are valid) ---
              val ctxForRhs = Utility.addThisType(cleanPath, ctx)

              given ExprContext = ctxForRhs

              P(valueExpr ~ WS0).map {
                case Left(e) => Left(e)

                case Right(rhsFn) =>
                  // --- Check for illegal use of 'this' outside collection/map context ---
                  if Utility.containsThis(rhsFn) && ctx.receiver.isEmpty then
                    Left(DLCompileError(rhsOff, "Use of 'this' with no receiver in scope"))
                  else {
                    // --- Infer effective LHS type ---
                    val lhsFieldType: FieldType = Utility.getPathType(cleanPath)
                    val effLhs: FieldType = Utility.effectiveLhsForAssignment(lhsFieldType, cleanPath)

                    // --- Infer RHS type ---
                    val effRhsOpt: Option[FieldType] = rhsFn match {
                      case GetFn(sym, _, _) =>
                        ctx.symbols.collectFirst {
                          case scope if scope.contains(sym) =>
                            scope(sym) match {
                              case vt: ValType => vt.valueType
                              case ft: FieldType => ft
                            }
                        }.orElse(Utility.rhsType(rhsFn)(using ctx))

                      case _ =>
                        Utility.rhsType(rhsFn)(using ctx)
                    }

                    effRhsOpt match {
                      case None =>
                        Left(DLCompileError(rhsOff, s"Unable to infer type of RHS: ${rhsFn.getClass.getSimpleName}"))

                      case Some(effRhs) =>
                        if Utility.areTypesCompatible(effLhs, effRhs) then
                          Right((ctx, UpdateStmt(cleanPath, rhsFn)))
                        else {
                          val lhsMsg = Utility.prettyFieldType(lhsFieldType) // declared type for message
                          val rhsMsg = Utility.prettyFieldType(effRhs)

                            // If you want a trailing '?' when LHS is optional:
                          val pathForMsg =
                            lhsFieldType match
                              case _: OptionType => s"$cleanPath"
                              case _             => cleanPath
                          println(s"[updateStmt] LHS raw type: ${Utility.getPathType(cleanPath)}")
                          println(s"[updateStmt] LHS effective type: $effLhs")
                          Left(DLCompileError(
                            rhsOff,
                            s"Type mismatch: cannot assign $rhsMsg to $lhsMsg at $pathForMsg"
                          ))
                        }
                    }
                  }
              }
          }
      }
    }

  // Parses: ( <expr> , <expr> )
  private def pairExpr[$: P](using ctx: ExprContext)
  : P[Either[DLCompileError, (Fn[Any], Fn[Any])]] =
    P("(" ~/ WS0 ~ valueExpr ~ WS0 ~ "," ~ WS0 ~ valueExpr ~ WS0 ~ ")").map {
      case (Left(e1), _) => Left(e1)
      case (_, Left(e2)) => Left(e2)
      case (Right(k: Fn[Any] @unchecked), Right(v: Fn[Any] @unchecked)) =>
        Right((k, v))
    }

  // exactly like blockFn but forcing the final expression to be pairExpr
  private def blockPairFn[$: P](using ctx0: ExprContext): P[ParseFnResult] =
    P("{" ~/ WS0).flatMap { _ =>
      given ExprContext = ctx0

      statementSeq.flatMap { stmtsE =>
        val folded =
          stmtsE.foldLeft[Either[DLCompileError, (ExprContext, List[Statement])]](Right(ctx0 -> Nil)) {
            case (Left(err), _) => Left(err)
            case (_, Left(err)) => Left(err)
            case (Right((accCtx, ss)), Right((newCtx, stmt))) =>
              Right(accCtx.merge(newCtx) -> (ss :+ stmt))
          }

        folded match {
          case Left(e) => P(Pass(Left(e)))

          case Right((finalCtx, ss)) =>
            given ExprContext = finalCtx

            P(pairExpr ~ WS0 ~ "}").map {
              case Left(err) => Left(err)
              case Right((kFn, vFn)) =>
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
                  val body: Fn[?] =
                    if isListLike then LoopFn(vfn)
                    else vfn
                  Right((ctx, MapStmt(cleanPath, body)))
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
      for {
        cond <- condRes
        thenB <- thenRes
        elseB <- elseRes
        _ = {
          println(s"[ifFn] cond parsed as: $cond, rhsType=${Utility.rhsType(cond)}")
          println(s"[ifFn] then branch: $thenB, rhsType=${Utility.rhsType(thenB)}")
          println(s"[ifFn] else branch: $elseB, rhsType=${Utility.rhsType(elseB)}")
        }
        built <- CIfFn.build(NoOpFn, List(cond.asInstanceOf[Fn[Any]], thenB, elseB))
        _     <- CIfFn.validate(built)(using ctx)
      } yield built.asInstanceOf[Fn[Any]]
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
