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

  /** atom := '(' booleanExpr ')' | comparisonExpr | booleanLiteral | ToBoolean(arithmeticExpr) */
  private def booleanAtom[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(
      Index ~ WS0 ~ (
        "(" ~/ booleanExpr ~ ")" |
          comparisonExpr |
          booleanLiteral.map(b => Right(b): ParseBoolResult) |
          arithmeticExpr
        )
    ).map { (off, expr) =>
      expr match
        case Left(err) => Left(err)

        case Right(fn) =>
          Utility.rhsType(fn) match
            // Already Boolean
            case TypeResult.Known(ScalarType(_, "scala.Boolean", _)) =>
              fn match
                case bf: BooleanFn => Right(bf)
                case other         => Right(ToBooleanFn(other.asInstanceOf[Fn[Any]], ctx.posStrFrom(off)))

            // Anything else — coerce via ToBooleanFn wrapper and include position
            case TypeResult.Known(_) | TypeResult.Unknown =>
              Right(ToBooleanFn(fn.asInstanceOf[Fn[Any]], ctx.posStrFrom(off)))

            // Propagate compile-time type errors
            case TypeResult.Error(e) =>
              Left(e.copy(posStr = ctx.posStrFrom(off)))
    }

  /** booleanExpr := booleanAnd ('||' booleanAnd)* */
  def booleanExpr[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(Index ~ booleanAnd.rep(sep = WS0 ~ "||" ~ WS0)).map { (off,terms) =>
      given ExprContext = ctx.copy(pos = off)
      if terms.isEmpty then Left(DLCompileError(ctx.posStr, "Empty boolean expression"))
      else {
        val oks = terms.collect { case Right(fn) => fn }
        if oks.isEmpty then terms.head
        else if oks.size == 1 then Right(oks.head.asInstanceOf[BooleanFn])
        else {
          val lhs = oks.head.asInstanceOf[Fn[Any]]
          val rhs = oks.tail.map(_.asInstanceOf[Fn[Any]])
          // build OR chain left-associatively
          val combined = rhs.foldLeft[Either[DLCompileError, BooleanFn]](Right(lhs.asInstanceOf[BooleanFn])) {
            case (Left(e), _) => Left(e)
            case (Right(acc), next) =>
              COrFn
                .build(NamedReceiver("||", ScalarType("", "scala.Boolean"), acc.asInstanceOf[Fn[Any]]), List(next))
                .flatMap { built =>
                  COrFn.validate(built).map(_ => built.asInstanceOf[BooleanFn])
                }
          }
          combined
        }
      }
    }

  /** booleanAnd := booleanNot ('&&' booleanNot)* */
  private def booleanAnd[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(Index ~ booleanNot.rep(sep = WS0 ~ "&&" ~ WS0)).map { (off,terms) =>
      given ExprContext = ctx.copy(pos = off)
      if terms.isEmpty then Left(DLCompileError(ctx.posStr, "Empty boolean expression"))
      else {
        val oks = terms.collect { case Right(fn) => fn }
        if oks.isEmpty then terms.head
        else if oks.size == 1 then Right(oks.head.asInstanceOf[BooleanFn])
        else {
          val lhs = oks.head.asInstanceOf[Fn[Any]]
          val rhs = oks.tail.map(_.asInstanceOf[Fn[Any]])
          // build AND chain left-associatively so each build has one rhs
          val combined = rhs.foldLeft[Either[DLCompileError, BooleanFn]](Right(lhs.asInstanceOf[BooleanFn])) {
            case (Left(e), _) => Left(e)
            case (Right(acc), next) =>
              CAndFn
                .build(NamedReceiver("&&", ScalarType("", "scala.Boolean"), acc.asInstanceOf[Fn[Any]]), List(next))
                .flatMap { built =>
                  CAndFn.validate(built).map(_ => built.asInstanceOf[BooleanFn])
                }
          }
          combined
        }
      }
    }

  /** booleanNot := '!' booleanNot | atom */
  private def booleanNot[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    P(
      (Index ~ "!" ~ WS0 ~ booleanNot).map {
        case (off, Right(b)) =>
          given ExprContext = ctx.copy(pos = off)
          b match
            case bf: BooleanFn =>
              CNotFn.build(
                NamedReceiver("!", ScalarType("", "scala.Boolean"), NoOpFn),
                List(bf.asInstanceOf[Fn[Any]])
              ) match
                case Left(err) => Left(err)
                case Right(fn) =>
                  // Use ctx.posStrFrom(off) if you want to embed source location later
                  CNotFn.validate(fn).map(_ => fn.asInstanceOf[BooleanFn])
        case (_, Left(e)) => Left(e)
      } | booleanAtom
    )

  /** comparisonExpr := arithmeticExpr (==|!=|>=|<=|>|<) arithmeticExpr */
  private def comparisonExpr[$: P](using ctx: ExprContext): P[ParseBoolResult] =
    println(s"[comparisonExpr] ENTER: parsing lhs/rhs around operator")
    P(Index ~ !StringIn("if", "then", "else") ~ arithmeticExpr ~ WS0 ~ StringIn("==", "!=", ">=", "<=", ">", "<").! ~ WS0 ~ arithmeticExpr)
      .map { case (off, lE, op, rE) =>
        given ExprContext = ctx.copy(pos = off)
        val cfn: CompileFn = op match
          case ">"  => CGreaterThanFn
          case ">=" => CGreaterThanOrEqualFn
          case "<"  => CLessThanFn
          case "<=" => CLessThanOrEqualFn
          case "==" => CEqualFn
          case "!=" => CNotEqualFn

        println(s"[comparisonExpr] op=$op lE=$lE rE=$rE")
        for
          left  <- lE
          right <- rE
          _ <- Right({
            println(s"[comparisonExpr] op=$op")
            println(s"[comparisonExpr]  left=${left.getClass.getSimpleName} (${left})")
            println(s"[comparisonExpr]  right=${right.getClass.getSimpleName} (${right})")
            println(s"[comparisonExpr]  about to call ${cfn.getClass.getSimpleName}.build with args: ${List(left, right)}")
          })
          built <- cfn
            .build(NamedReceiver(op, ScalarType("", "scala.Boolean"), left), List(right))
            .asInstanceOf[Either[DLCompileError, BooleanFn]]
          _     <- cfn.validate(built)(using ctx)
        yield built
      }

  // ---- Arithmetic ----

  // arithmeticExpr := arithmeticTerm (('+'|'-') arithmeticTerm)*
  private def arithmeticExpr[$: P](using ctx: ExprContext): P[ParseFnResult] =
    println(s"[Level2:arithmeticExpr] ENTER ctx=${ctx.hashCode()}")
    P(Index ~ arithmeticTerm ~ (WS0 ~ CharIn("+\\-").! ~ WS0 ~ arithmeticTerm).rep).map {
      case (off, first, rest) =>
        given ExprContext = ctx.copy(pos = off)
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
                    // Debug prints for operator, function class names, and inferred types
                    println(s"[Level2:arithmeticExpr] op='$op'  recv=${acc.getClass.getSimpleName}  rhs=${r.getClass.getSimpleName}")
                    println(s"[Level2:arithmeticExpr] recvType=${acc.resultType}")
                    println(s"[Level2:arithmeticExpr] rhsType=${Utility.rhsType(r)}")
                    val recvType =
                      acc.resultType match
                        case s if s == null || s.typeName == "scala.Any" =>
                          Utility.rhsType(acc) match
                            case TypeResult.Known(ft) => ft
                            case _                    => ScalarType("", "scala.Any", false)
                        case other => other
                    val rhsFieldType = Utility.rhsType(r)
                    println(s"[Level2:arithmeticExpr] recv=$acc (${recvType})")
                    println(s"[Level2:arithmeticExpr] rhs=$r (${rhsFieldType})")
                    cfn
                      .build(NamedReceiver(op, recvType, acc), List(r))
                      .flatMap(fn => cfn.validate(fn)(using ctx).map(_ => fn.asInstanceOf[Fn[Any]]))
                }
            }
    }

  // Utility for 'this' enforcement in arithmetic
  private def containsIllegalThis(fn: Fn[?])(using ctx: ExprContext): Boolean =
    fn match
      case GetFn("this", _, _, _, _) if ctx.receiver.isEmpty => true
      case _ => false

  // arithmeticTerm := arithmeticFactor (('*'|'/'|'%') arithmeticFactor)*
  private def arithmeticTerm[$: P](using ctx: ExprContext): P[ParseFnResult] =
    println("[Level2:arithmeticTerm] ENTER")
    P(Index ~ arithmeticFactor ~ (WS0 ~ CharIn("*/%").! ~ WS0 ~ arithmeticFactor).rep).map {
      case (off, first, rest) =>
        given ExprContext = ctx.copy(pos = off)
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
                    val recvType = acc.resultType
                    cfn
                      .build(NamedReceiver(op, recvType, acc), List(r))
                      .flatMap(fn => cfn.validate(fn)(using ctx).map(_ => fn))
                }
            }
    }

  // arithmeticFactor := unaryMinus | arithmeticAtom
  private def arithmeticFactor[$: P](using ctx: ExprContext): P[ParseFnResult] =
    println("[Level2:arithmeticFactor] ENTER")
    unaryMinus

  // arithmeticAtom := baseExpr | numberLiteral | stringLiteral | '(' valueExpr ')' [.methodChain]
  private def arithmeticAtom[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(
      // --- PATCH: resolve GetFn field type eagerly in baseExpr ---
      baseExprWithFieldType |
      numberLiteral |
      stringLiteral |
      ("(" ~/ valueExpr ~ ")").flatMap {
        case Right(expr)    => methodChain(expr)
        case left @ Left(_) => P(Pass(left))
      }
    )

  private def baseExprWithFieldType[$: P](using ctx: ExprContext): P[ParseFnResult] =
    baseExprRaw.map {
      case g@GetFn(path, isOptional, recv, pos, _) =>
        // 1) Try local val or symbol scope
        val fromSymbols: Option[FieldType] =
          ctx.symbols.collectFirst {
            case scope if scope.contains(path) =>
              scope(path) match
                case v: ValType => v.valueType
                case f: FieldType => f
          }

        // 2) Fall back to schema-based inference — look up the field by name
        val fieldT = fromSymbols.getOrElse {
          ctx.resolveSchemaFor(recv) match
            case ct: ClassType =>
              ct.fields.find(_.name == path).getOrElse(ScalarType(path, "scala.Any", false))
            case ft: FieldType => ft
        }

        Right(GetFn(path, isOptional, recv, pos, Some(fieldT)))

      case other => Right(other)
    }

  // PATCH: baseExprRaw now returns ParseFnResult directly and flattens to Fn[Any]
  private def baseExprRaw[$: P](using ctx: ExprContext): P[Fn[Any]] =
    baseExpr.map {
      case Right(fn: Fn[Any] @unchecked) => fn
      case _                             => NoOpFn
    }

  // support unary minus: -x
  private def unaryMinus[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(Index ~ "-" ~/ WS0 ~ arithmeticAtom).flatMap {
      case (off, Right(fn)) =>
        given ExprContext = ctx.copy(pos = off)
        val result: ParseFnResult =
          for {
            built <- CMinusFn.build(NamedReceiver("-", ScalarType("", "scala.Double"), NoOpFn), List(fn))
            _     <- CMinusFn.validate(built)(using ctx)
          } yield built
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
            case GetFn("Nil", _, _, _, _) =>
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

  private def mapExpr[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P(Index ~ pathFn ~ WS0 ~ "=>" ~/ WS0 ~ Index).flatMap { case (pathOff, rawPathRes, rhsOff) =>
      rawPathRes match
        case Left(err) => P(Pass(Left(err)))
        case Right(fn: GetFn) =>
          val rawPath = fn.path
          val baseSchema =
            fn.recv match
              case RootFn | NoOpFn => ctx.resolveSchemaFor(RootFn)
              case other => ctx.resolveSchemaFor(other)
          util.PathUtil.getPathType(rawPath, baseSchema) match
            case Left(err) =>
              P(Pass(Left(DLCompileError(ctx.posStrFrom(rhsOff), s"Unable to resolve path type: $err"))))
            case Right(lhsFieldType) =>
              val rhsCtx = Utility.addThisType(rawPath, ctx).getOrElse(ctx)

              given ExprContext = rhsCtx

              P(valueExpr).map {
                case Left(e) => Left(e)
                case Right(rhsFn) =>
                  val bodyFn = rhsFn
                  Right(MapFn(fn, bodyFn, ctx.posStrFrom(rhsOff)): Fn[Any])
              }

        case Right(otherFn) =>
          P(Pass(Left(DLCompileError(ctx.posStrFrom(pathOff),
            s"Expected a path (GetFn) before '=>', got ${otherFn.getClass.getSimpleName}"))))
    }

  // ---- valueExpr => Top-Level Expr ----
  def valueExpr[$: P](using ctx: ExprContext): P[ParseFnResult] =
    println("[Level2:valueExpr] ENTER")
    P(
      ifFn |
        blockFn |
        mapExpr |
        consExpr | // arithmetic, path, etc.
        booleanExpr
    ).flatMap {
      case Left(err) => P(Pass.map(_ => Left(err)))
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
          updateStmt |
          blockStmt
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
              case s: ScalarType if s.name.isEmpty => s.copy(name = name)
              case other                                => other
            val valFt = ValType(name, ftNamed, ftNamed.typeName)
            val newCtx = ctx.withVals(name -> valFt)
            Right((newCtx, ValStmt(name, vfn)))

          // ---- Explicit type error (propagate) ----
          case TypeResult.Error(err) =>
            Left(err.copy(posStr = ctx.posStr, msg = s"In val '$name': ${err.msg}"))

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
  private def enforceIndexedBaseIsCollection(path: String, off: Int, pathFn: GetFn)
                                            (using ctx: ExprContext): Either[DLCompileError, Unit] = {
    import co.blocke.dynalens.Path
    import co.blocke.dynalens.PathElement

    val parts = Path.parsePath(path)

    // Find the *first* indexed segment and reconstruct its base path prefix
    val idxPos = parts.indexWhere {
      case PathElement(_, Some(_)) => true
      case _                       => false
    }

    if (idxPos < 0) Right(()) // no indexing present
    else {
      val baseParts = parts.take(idxPos) :+ parts(idxPos) // include the indexed field’s name
      val baseName = baseParts.last match {
        case PathElement(Some(n), _) => n
        case PathElement(None, _)    => "" // fallback, should not occur for real fields
      }

      // Build the dotted prefix up to (and including) the indexed field name
      val basePath = baseParts.map {
        case PathElement(Some(n), _) => n
        case PathElement(None, _)    => "" // fallback, should not occur for real fields
      }.mkString(".")

      val baseSchema =
        pathFn.recv match
          case RootFn | NoOpFn => ctx.resolveSchemaFor(RootFn)
          case other           => ctx.resolveSchemaFor(other)
      util.PathUtil.getPathType(basePath, baseSchema) match {
        case Left(err) =>
          println(s"[enforceIndexedBaseIsCollection] basePath=$basePath error: ${err}")
          Left(DLCompileError(ctx.posStrFrom(off), s"Unknown field path '$basePath': ${err}"))
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
    println(s"[updateStmt:trace] ENTER with ctx0=${ctx0.hashCode()}")
    P(pathFn ~ WS0 ~ "=" ~/ WS0 ~ Index).flatMap {
      case (Right(pathFn: GetFn), rhsOff) =>
        val rawPath = pathFn.path
        // Compute baseSchema for the path type resolution
        val baseSchema =
          pathFn.recv match
            case RootFn | NoOpFn => ctx0.schema
            case other => ctx0.resolveSchemaFor(other)
        println(s"[updateStmt:trace] after baseSchema resolution, about to enforceIndexedBaseIsCollection, rawPath=$rawPath baseSchema=$baseSchema")

        util.PathUtil.getPathType(rawPath, baseSchema) match {
          case Left(err) => P(Pass(Left(DLCompileError(ctx0.posStrFrom(rhsOff), s"Unable to resolve path type: $err"))))
          case Right(lhsFieldType) =>
            // --- Infer effective LHS type (effLhs is now in scope for compatibility and printlns) ---
            val effLhs: FieldType = lhsFieldType match {
              case v: ValType => v.valueType
              case other      => other
            }
            println(s"[updateStmt:trace] about to call Utility.addThisType for path=$rawPath, ctx0.receiver=${ctx0.receiver}")
            enforceIndexedBaseIsCollection(rawPath, rhsOff, pathFn)(using ctx0) match {
              case Left(e) => P(Pass(Left(e)))
              case Right(_) =>
                // --- enrich context for RHS (so `this` and nested fields are valid) ---
                Utility.addThisType(rawPath, ctx0) match {
                  case Left(err) =>
                    println(s"[updateStmt:trace] Utility.addThisType returned Left: ${err}")
                    P(Pass(Left(err)))
                  case Right(ctxForRhs) =>
                    println(s"[updateStmt:trace] Utility.addThisType returned Right ctxForRhs=${ctxForRhs.hashCode()} receiver=${ctxForRhs.receiver}")
                    given ExprContext = ctxForRhs
                    println(s"[updateStmt:trace] about to parse RHS valueExpr")
                    P(valueExpr ~ WS0).map {
                      case Left(e) => Left(e)
                      case Right(rhsFn) =>
                        println(s"[updateStmt:trace] RHS parse result: ${rhsFn}")
                        // --- Check for illegal use of 'this' outside collection/map context ---
                        if Utility.containsThis(rhsFn) && ctx0.receiver.isEmpty then
                          Left(DLCompileError(ctx0.posStrFrom(rhsOff), "Use of 'this' with no receiver in scope"))
                        else {
                          // --- Infer RHS type ---
                          val effRhsResult: Either[DLCompileError, FieldType] = rhsFn match {
                            case GetFn(sym, _, _, _, _) =>
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
                              // Inline type compatibility logic (relaxed numeric assignment):
                              val lhsIsNumeric = effLhs.typeName.startsWith("scala.Int") ||
                                effLhs.typeName.startsWith("scala.Long") ||
                                effLhs.typeName.startsWith("scala.Float") ||
                                effLhs.typeName.startsWith("scala.Double")
                              val rhsIsNumeric = effRhs.typeName.startsWith("scala.Int") ||
                                effRhs.typeName.startsWith("scala.Long") ||
                                effRhs.typeName.startsWith("scala.Float") ||
                                effRhs.typeName.startsWith("scala.Double")
                              println(s"[updateStmt:trace] type check lhs=${effLhs.typeName}, rhs=${effRhs.typeName}, lhsIsNumeric=$lhsIsNumeric, rhsIsNumeric=$rhsIsNumeric")
                              val compatible =
                                effLhs.typeName == effRhs.typeName ||
                                (lhsIsNumeric && rhsIsNumeric)

                              if compatible then
                                println(s"[updateStmt:trace] about to build UpdateStmt for path=$rawPath with effLhs=${effLhs.typeName}")
                                Right((ctx0, UpdateStmt(rawPath, rhsFn, ctx0.posStrFrom(rhsOff), effLhs)))
                              else {
                                val lhsMsg = Utility.prettyFieldType(lhsFieldType)
                                val rhsMsg = Utility.prettyFieldType(effRhs)
                                println(s"[updateStmt:trace] TYPE MISMATCH DETECTED: lhs=${effLhs.typeName}, rhs=${effRhs.typeName}")
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
      case (Right(_), rhsOff) =>
        P(Pass(Left(DLCompileError(ctx0.posStrFrom(rhsOff), "Expected a GetFn path on LHS"))))
      case (Left(err), _) =>
        P(Pass(Left(err)))
    }

  private def ifStmt[$: P](using ctx: ExprContext): P[ParseStmtResult] =
    P(
      "if" ~/ WS ~ (("(" ~/ booleanExpr ~ ")") | booleanExpr) ~ WS0 ~
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
    P( Index ~
      "if" ~/ WS ~ (
        ("(" ~/ booleanExpr ~ ")") | booleanExpr
      ) ~ WS0 ~
        "then" ~/ WS0 ~ valueExpr ~ WS0 ~
        "else" ~/ WS0 ~ valueExpr
    ).map { case (pos, condRes, thenRes, elseRes) =>
      for
        c <- condRes
        t <- thenRes
        e <- elseRes
      yield IfFn(c, t, e, ctx.posStrFrom(pos))
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
    P( Index ~
      "{" ~ WS0 ~
        normalCase.rep(sep = WS0) ~
        defaultCase.? ~
        WS0 ~ "}"
    ).map { (pos, caseLines, maybeDefault) =>
      val errors = caseLines.collect { case Left(e) => e }
      if errors.nonEmpty then Left(errors.head)
      else
        val regularCases = caseLines.collect { case Right((k, v)) => (k, v) }

        maybeDefault match
          case Some(Left(e)) =>
            Left(e)
          case Some(Right((key, fn))) if key != "__default__" =>
            // Should never happen, but defensive
            Left(DLCompileError(ctx.posStrFrom(pos), s"Expected 'default', found: $key"))
          case Some(Right((_, fn))) =>
            Right((regularCases.toVector, Some(fn)))
          case None =>
            Right((regularCases.toVector, None))
    }

  private def permissiveMode[$: P]: P[Boolean] =
    P("(" ~ WS0 ~ "permissive" ~ WS0 ~ ")").map(_ => true).?.map(_.getOrElse(false))

  // after you produce a base: P(valueExprCore).flatMap { base => ... }
  private def maybeCaseTail[$: P](base: Fn[Any])(using ctx: ExprContext): P[Either[DLCompileError, Fn[Any]]] =
    P(Index ~ WS0 ~ "case" ~ WS0 ~ permissiveMode ~ WS0 ~ caseBlock).?.map {
      case None =>
        Right(base)

      case Some((pos, permissive, Right((pairs, df)))) =>
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
            Right(CaseWhenFn(base, pairs, df, permissive, ctx.posStrFrom(pos)))
        }

      case Some((_, _, Left(e))) =>
        Left(e)
    }

