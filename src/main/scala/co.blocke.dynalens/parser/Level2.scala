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
import cfn.*
import fn.*
import co.blocke.dynalens.ScalarType

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
        CAndFn.build(
          NamedReceiver("&&", ScalarType("", "scala.Boolean"), NoOpFn),
          List(a1.asInstanceOf[Fn[Any]], b1.asInstanceOf[Fn[Any]])
        ) match
          case Left(err) => Left(err)
          case Right(fn) =>
            CAndFn.validate(fn)(using ctx).map(_ => fn.asInstanceOf[BooleanFn])

  private inline def orCombine(a: ParseBoolResult, b: ParseBoolResult)(using ctx: ExprContext): ParseBoolResult =
    (a, b) match
      case (Left(e), _) => Left(e)
      case (Right(_), Left(e)) => Left(e)
      case (Right(a1), Right(b1)) =>
        COrFn.build(
          NamedReceiver("||", ScalarType("", "scala.Boolean"), NoOpFn),
          List(a1.asInstanceOf[Fn[Any]], b1.asInstanceOf[Fn[Any]])
        ) match
          case Left(err) => Left(err)
          case Right(fn) =>
            COrFn.validate(fn)(using ctx).map(_ => fn.asInstanceOf[BooleanFn])

  /** atom := '(' booleanExpr ')' | comparisonExpr | booleanLiteral | ToBoolean(arithmeticExpr) */
  private def booleanAtom[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(
      WS0 ~ (
        "(" ~/ booleanExpr ~ ")" |
          comparisonExpr |
          booleanLiteral.map(b => Right(b): ParseBoolResult) |
          arithmeticExpr.map(_.map(fn => ToBooleanFn(fn, ctx.posStr)): ParseBoolResult)
      )
    )

  /** booleanExpr := booleanAnd ('||' booleanAnd)* */
  def booleanExpr[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(booleanAnd ~ (WS0 ~ "||" ~ WS0 ~ booleanAnd).rep).map {
      case (first, rest) =>
        rest.foldLeft(first) {
          case (Left(e), _) => Left(e)
          case (Right(acc), Right(rhs)) =>
            orCombine(Right(acc), Right(rhs))
          case (Right(_), Left(e)) => Left(e)
        }
    }

  /** booleanAnd := booleanNot ('&&' booleanNot)* */
  private def booleanAnd[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(booleanNot ~ (WS0 ~ "&&" ~ WS0 ~ booleanNot).rep).map {
      case (first, rest) =>
        rest.foldLeft(first) {
          case (Left(e), _) => Left(e)
          case (Right(acc), Right(rhs)) =>
            andCombine(Right(acc), Right(rhs))
          case (Right(_), Left(e)) => Left(e)
        }
    }

  /** booleanNot := '!' booleanNot | atom */
  private def booleanNot[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(
      ("!" ~ WS0 ~ booleanNot).map {
        case Right(b) =>
          CNotFn.build(NamedReceiver("!", ScalarType("", "scala.Boolean"), NoOpFn), List(b.asInstanceOf[Fn[Any]])) match
            case Left(e) => Left(e)
            case Right(fn) =>
              CNotFn.validate(fn)(using ctx).map(_ => fn)
        case Left(e) => Left(e)
      } | booleanAtom
    )

  /** comparisonExpr := arithmeticExpr (==|!=|>=|<=|>|<) arithmeticExpr */
  private def comparisonExpr[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(arithmeticExpr ~ WS0 ~ StringIn("==", "!=", ">=", "<=", ">", "<").! ~ WS0 ~ arithmeticExpr)
      .map { case (lE, op, rE) =>
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
            .build(NamedReceiver(op, ScalarType("", "scala.Boolean"), NoOpFn), List(left, right))
            .asInstanceOf[Either[DLCompileError, BooleanFn]]
          _     <- cfn.validate(built)(using ctx)
        yield built
      }

  // ---- Arithmetic ----

  // arithmeticExpr := arithmeticTerm (('+'|'-') arithmeticTerm)*
  private def arithmeticExpr[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(Index ~ arithmeticTerm ~ (WS0 ~ CharIn("+\\-").! ~ WS0 ~ arithmeticTerm).rep).map {
      case (off, first, rest) =>
        first match
          case Right(fn) if containsIllegalThis(fn) =>
            Left(DLCompileError(ctx.posStrFrom(off), "Use of 'this' with no receiver in scope"))
          case _ =>
            rest.foldLeft(first) {
              case (Left(e), _) => Left(e)
              case (Right(acc), (op, rightE)) =>
                rightE.flatMap { r =>
                  if containsIllegalThis(r) then
                    Left(DLCompileError(ctx.posStrFrom(off), "Use of 'this' with no receiver in scope"))
                  else
                    val cfn = op match
                      case "+" => CPlusFn
                      case "-" => CMinusFn
                    cfn
                      .build(NamedReceiver(op, ScalarType("", "scala.Double"), NoOpFn), List(acc, r))
                      .flatMap(fn => cfn.validate(fn)(using ctx).map(_ => fn.asInstanceOf[Fn[Any]]))
                }
            }
    }

  // Utility for 'this' enforcement in arithmetic
  private def containsIllegalThis(fn: Fn[?])(using ctx: ExprContext): Boolean =
    fn match
      case GetFn("this", _, _, _) if ctx.receiver.isEmpty => true
      case _ => false

  // arithmeticTerm := arithmeticFactor (('*'|'/'|'%') arithmeticFactor)*
  private def arithmeticTerm[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(Index ~ arithmeticFactor ~ (WS0 ~ CharIn("*/%").! ~ WS0 ~ arithmeticFactor).rep).map {
      case (off, first, rest) =>
        first match
          case Right(fn) if containsIllegalThis(fn) =>
            Left(DLCompileError(ctx.posStrFrom(off), "Use of 'this' with no receiver in scope"))
          case _ =>
            rest.foldLeft(first) {
              case (Left(e), _) => Left(e)
              case (Right(acc), (op, rightE)) =>
                rightE.flatMap { r =>
                  if containsIllegalThis(r) then
                    Left(DLCompileError(ctx.posStrFrom(off), "Use of 'this' with no receiver in scope"))
                  else
                    val cfn = op match
                      case "*" => CMultiplyFn
                      case "/" => CDivideFn
                      case "%" => CModulusFn
                    cfn
                      .build(NamedReceiver(op, ScalarType("", "scala.Double"), NoOpFn), List(acc, r))
                      .flatMap(fn => cfn.validate(fn)(using ctx).map(_ => fn.asInstanceOf[Fn[Any]]))
                }
            }
    }

  // arithmeticFactor := unaryMinus | arithmeticAtom
  private def arithmeticFactor[$: P](using ctx: ExprContext): P[ParseFnResult] =
    unaryMinus

  // arithmeticAtom := baseExpr | numberLiteral | stringLiteral | '(' valueExpr ')' [.methodChain]
  private def arithmeticAtom[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(
      baseExpr |
      numberLiteral |
      stringLiteral |
      ("(" ~/ valueExpr ~ ")").flatMap {
        case Right(expr)    => methodChain(expr)
        case left @ Left(_) => P(Pass(left))
      }
    )

  // support unary minus: -x
  private def unaryMinus[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(Index ~ "-" ~/ WS0 ~ arithmeticAtom).flatMap {
      case (off, Right(fn)) =>
        given ExprContext = ctx.copy(pos = off)
        val result: ParseFnResult =
          for {
            built <- CMinusFn.build(NamedReceiver("-", ScalarType("", "scala.Double"), NoOpFn), List(fn))
            _     <- CMinusFn.validate(built)(using ctx)
          } yield built.asInstanceOf[Fn[Any]]
        P(Pass(result))
      case (_, Left(err)) =>
        P(Pass(Left(err)))
    } | arithmeticAtom

  // ---- String Concat ----

  private def consExpr[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(
      Index ~ arithmeticExpr ~ (WS0 ~ "::" ~ WS0 ~ arithmeticExpr).rep ~
        &(WS0 ~ !CharIn("=<>!")) // prevent mis-parsing into comparison
    ).map { case (off, firstE, restE) =>
      given ExprContext = ctx.copy(pos = off)
      // if there was no "::", just return lhs unchanged
      if restE.isEmpty then
        firstE
      else {
        val all = firstE :: restE.toList
        val (errs, oks) = all.partitionMap(identity)

        if errs.nonEmpty then Left(errs.head)
        else {
          val fns: List[Fn[Any]] = oks.map {
            case GetFn("Nil", _, _, _) =>
              ConstantFn[List[Any]](Nil).asInstanceOf[Fn[Any]]
            case other => other
          }

          val consTree: Fn[Any] =
            fns.reduceRight[Fn[Any]] { (h, t) =>
              ConsFn(h, t, ctx.posStr).asInstanceOf[Fn[Any]]
            }

          for {
            _ <- CConsFn.validate(consTree)
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

          P(Index ~ valueExpr).flatMap {
            case (_, Left(e)) =>
              P(WS0 ~ "}").map(_ => Left(e): ParseFnResult)

            case (off, Right(fn)) =>
              P(WS0 ~ "}").map(_ => Right(BlockFn(stmts, fn, ctx.posStrFrom(off)): Fn[Any]))
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
        Utility.rhsType(vfn) match
          // ---- FieldType found ----
          case TypeResult.Known(ft: FieldType) =>
            // ensure ScalarType carries the val name, so later assignments match cleanly
            val ftNamed = ft match
              case s: ScalarType if s.fieldName.isEmpty => s.copy(fieldName = name)
              case other                                => other
            val valFt = ValType(name, ftNamed, ftNamed.typeName)
            val newCtx = ctx.withVals(name -> valFt)
            Right((newCtx, ValStmt(name, vfn)))

          // ---- Explicit type error (propagate) ----
          case TypeResult.Error(err) =>
            Left(err.copy(posStr = ctx.posStrFrom(offset)))

          // ---- Unknown type (construct error manually) ----
          case TypeResult.Unknown =>
            val reason = vfn match
              case g: GetFn =>
                s"Unknown field path '${g.path}'"
              case m: Fn[?] =>
                val recvTypeStr = Utility.rhsType(m.recv) match
                  case TypeResult.Known(ft) => ft.typeName
                  case _                    => "unknown"
                s"Method '${m.methodName}' cannot be applied to receiver of type $recvTypeStr"
              case null =>
                "Unexpected null function"
            Left(DLCompileError(ctx.posStrFrom(offset), reason))

      // ---- Parse error ----
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
      case _                    => false
    }

    if (idxPos < 0) Right(()) // no indexing present
    else {
      val baseParts = parts.take(idxPos) :+ parts(idxPos) // include the indexed field’s name
      val baseName = baseParts.last match {
        case Path.IndexedField(n, __) => n
        case Path.Field(n)           => n
      }

      // Build the dotted prefix up to (and including) the indexed field name
      val basePath = baseParts.map {
        case Path.Field(n)           => n
        case Path.IndexedField(n, _) => n
      }.mkString(".")

      Utility.getPathType(basePath)(using ctx) match {
        case Left(err) =>
          println(s"[enforceIndexedBaseIsCollection] basePath=$basePath error: ${err.msg}")
          Left(DLCompileError(ctx.posStrFrom(off), s"Unknown field path '$basePath': ${err.msg}"))
        case Right(rawType) =>
          // unwrap Option or other wrappers if you’ve introduced a unified “effectiveTypeOf” helper
          val effectiveType = rawType match
            case v: ValType => v.valueType
            case other      => other

          println(s"[enforceIndexedBaseIsCollection] basePath=$basePath  rawType=$rawType  effective=$effectiveType")

          effectiveType match {
            case _: ListType => Right(())
            case _           => Left(DLCompileError(ctx.posStrFrom(off),
              s"Cannot index into non-list field '$baseName'"))
          }
      }
    }
  }

  // '=': always assignment
  private def updateStmt[$: P](using ctx0: ExprContext): P[ParseStmtResult] =
    P(pathBase ~ WS0 ~ "=" ~/ WS0 ~ Index).flatMap { case (rawPath, rhsOff) =>
      println(s"[updateStmt] starting parse, raw path: $rawPath, type: ${Utility.getPathType(rawPath)}")

      Utility.getPathType(rawPath) match {
        case Left(err) => P(Pass(Left(err)))
        case Right(lhsFieldType) =>
          // --- Infer effective LHS type (effLhs is now in scope for compatibility and printlns) ---
          val effLhs: FieldType = lhsFieldType match {
            case v: ValType => v.valueType
            case other      => other
          }
          enforceIndexedBaseIsCollection(rawPath, rhsOff)(using ctx0) match {
            case Left(e) => P(Pass(Left(e)))
            case Right(_) =>
              // --- enrich context for RHS (so `this` and nested fields are valid) ---
              Utility.addThisType(rawPath, ctx0) match {
                case Left(err) =>
                  P(Pass(Left(err)))
                case Right(ctxForRhs) =>
                  given ExprContext = ctxForRhs
                  P(valueExpr ~ WS0).map {
                    case Left(e) => Left(e)
                    case Right(rhsFn) =>
                      // --- Check for illegal use of 'this' outside collection/map context ---
                      if Utility.containsThis(rhsFn) && ctx0.receiver.isEmpty then
                        Left(DLCompileError(ctx0.posStrFrom(rhsOff), "Use of 'this' with no receiver in scope"))
                      else {
                        // --- Infer RHS type ---
                        val effRhsResult: Either[DLCompileError, FieldType] = rhsFn match {
                          case GetFn(sym, _, _, _) =>
                            ctxForRhs.symbols.collectFirst {
                              case scope if scope.contains(sym) =>
                                scope(sym) match {
                                  case vt: ValType   => vt.valueType
                                  case ft: FieldType => ft
                                }
                            }
                            .map(Right(_))
                            .getOrElse {
                              Utility.rhsType(rhsFn)(using ctxForRhs) match
                                case TypeResult.Known(ft: FieldType)       => Right(ft)
                                case TypeResult.Error(err: DLCompileError) => Left(err)
                                case TypeResult.Unknown                    => Left(DLCompileError(ctxForRhs.posStr, "Unknown RHS type"))
                            }
                          case _ =>
                            Utility.rhsType(rhsFn)(using ctxForRhs) match
                              case TypeResult.Known(ft: FieldType)       => Right(ft)
                              case TypeResult.Error(err: DLCompileError) => Left(err)
                              case TypeResult.Unknown                    => Left(DLCompileError(ctxForRhs.posStr, "Unknown RHS type"))
                        }
                        effRhsResult match {
                          case Left(err) =>
                            Left(DLCompileError(ctx0.posStrFrom(rhsOff), s"Unable to infer type of RHS: ${err.msg}"))
                          case Right(effRhs) =>
                            // Inline type compatibility logic:
                            val compatible =
                              (effLhs.typeName == effRhs.typeName) ||
                              ((effLhs.isInstanceOf[ScalarType] && effRhs.isInstanceOf[ScalarType]) &&
                                (effLhs.typeName == effRhs.typeName))
                            if compatible then
                              Right((ctx0, UpdateStmt(rawPath, rhsFn, ctx0.posStrFrom(rhsOff))))
                            else {
                              val lhsMsg = Utility.prettyFieldType(lhsFieldType) // declared type for message
                              val rhsMsg = Utility.prettyFieldType(effRhs)
                              println(s"[updateStmt] LHS raw type: $lhsFieldType")
                              println(s"[updateStmt] LHS effective type: $effLhs")
                              Left(DLCompileError(
                                ctx0.posStrFrom(rhsOff),
                                s"Type mismatch: cannot assign $rhsMsg to $lhsMsg at $rawPath"
                              ))
                            }
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
    P(Index ~ "(" ~/ WS0 ~ valueExpr ~ WS0 ~ "," ~ WS0 ~ valueExpr ~ WS0 ~ ")").map {
      case (_, Left(e1), _) => Left(e1)
      case (_, _, Left(e2)) => Left(e2)
      case (off, Right(k: Fn[Any] @unchecked), Right(v: Fn[Any] @unchecked)) =>
        if Utility.containsThis(k) && ctx.receiver.isEmpty then
          Left(DLCompileError(ctx.posStrFrom(off), "Use of 'this' with no receiver in scope in pair key"))
        else if Utility.containsThis(v) && ctx.receiver.isEmpty then
          Left(DLCompileError(ctx.posStrFrom(off), "Use of 'this' with no receiver in scope in pair value"))
        else
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
              Right(newCtx -> (ss :+ stmt))
          }
        folded match {
          case Left(e) => P(Pass(Left(e)))
          case Right((finalCtx, ss)) =>
            given ExprContext = finalCtx
            P(pairExpr ~ WS0 ~ "}").map {
              case Left(err) => Left(err)
              case Right((kFn, vFn)) =>
                Right(BlockFn(ss, Tuple2Fn(kFn, List(vFn), ctx0.posStr), ctx0.posStr): Fn[Any])
            }
        }
      }
    }

  // '=>' map statement (comprehensions)
  private def mapStmt[$: P](using ctx0: ExprContext): P[ParseStmtResult] =
    P(pathBase ~ WS0 ~ "=>" ~/ WS0 ~ Index).flatMap { case (rawPath, rhsOff) =>
      Utility.getPathType(rawPath)(using ctx0) match {
        case Left(err) => P(Pass(Left(err)))
        case Right(_) =>
          Utility.getPathType(rawPath)(using ctx0) match {
            case Left(err) => P(Pass(Left(err)))
            case Right(lhsFt) =>
              // Compute baseRhsCtx depending on the LHS type
              val baseRhsCtx: ExprContext = lhsFt match {
                case m: MapType =>
                  ctx0.withReceiver(Utility.mapEntryReceiverFor(rawPath)(using ctx0))
                case v: ValType if v.valueType.isInstanceOf[MapType] =>
                  ctx0.withReceiver(Utility.mapEntryReceiverFor(rawPath)(using ctx0))
                case l: ListType =>
                  val ctxWithRecv = ctx0.withReceiverFromPath(rawPath) match {
                    case Right(newCtx) => newCtx
                    case Left(_)       => ctx0
                  }
                  ctxWithRecv
                case v: ValType if v.valueType.isInstanceOf[ListType] =>
                  val ctxWithRecv = ctx0.withReceiverFromPath(rawPath) match {
                    case Right(newCtx) => newCtx
                    case Left(_)       => ctx0
                  }
                  ctxWithRecv
                case v: ValType if v.valueType.isInstanceOf[ScalarType] =>
                  ctx0.withVals("this" -> v.valueType)
                case _ =>
                  ctx0
              }
              val ctxForRhs = baseRhsCtx
              given ExprContext = ctxForRhs
              lhsFt match {
                // LHS is a Map or ValType wrapping a Map
                case _: MapType |
                     (_: ValType) if lhsFt.isInstanceOf[ValType] && lhsFt.asInstanceOf[ValType].valueType.isInstanceOf[MapType] =>
                  val pairAsFn: P[Either[DLCompileError, Fn[Any]]] =
                    P(
                      blockPairFn |
                        pairExpr.map {
                          case Left(e)       => Left(e)
                          case Right((k, v)) => Right(Tuple2Fn(k, List(v), ctx0.posStr): Fn[Any])
                        }
                    )
                  P(pairAsFn ~ WS0).map {
                    case Left(e)       => Left(e)
                    case Right(bodyFn) => Right((ctx0, MapStmt(rawPath, bodyFn, ctx0.posStrFrom(rhsOff))))
                  }
                case _ =>
                  P(valueExpr ~ WS0).map {
                    case Left(e) => Left(e)
                    case Right(vfn) =>
                      // true if the LHS is a List or a ValType wrapping a List
                      val isListLike = lhsFt match {
                        case _: ListType => true
                        case v: ValType if v.valueType.isInstanceOf[ListType] => true
                        case _ => false
                      }
                      val body: Fn[?] =
                        if isListLike then LoopFn(vfn, ctx0.posStrFrom(rhsOff))
                        else vfn
                      Right((ctx0, MapStmt(rawPath, body.asInstanceOf[Fn[Any]], ctx0.posStrFrom(rhsOff))))
                  }
              }
          }
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
        built <- CIfFn.build(NamedReceiver("if", ScalarType("", "scala.Boolean"), NoOpFn), List(cond.asInstanceOf[Fn[Any]], thenB, elseB))
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
            Left(DLCompileError(ctx.posStrFrom(implicitly[ParsingRun[?]].index), s"Expected 'default', found: $key"))
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
            Left(DLCompileError(ctx.posStrFrom(implicitly[ParsingRun[?]].index), msg))
          case None =>
            Right(CaseWhenFn(base, pairs, df, permissive, ctx.posStrFrom(implicitly[ParsingRun[?]].index)))
        }

      case Some((_, Left(e))) =>
        Left(e)
    }
