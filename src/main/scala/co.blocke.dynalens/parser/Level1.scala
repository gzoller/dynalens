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

  private def optSuffix[$: P]: P[String] =
    P("?".!.?).map(_.getOrElse(""))

  private def segment[$: P]: P[String] =
    P(identS ~ (wildcardIndex | indexPart | curlyBraces).? ~ optSuffix).map {
      case (name, Some(suffixPart), suf) => s"$name$suffixPart$suf"
      case (name, None, suf)             => s"$name$suf"
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
    P(
      Index ~
        segmentFn ~
        // IMPORTANT: stop before .method( ... )
        (!("." ~ identU ~ "(") ~ "." ~ segmentFn).rep
    ).flatMap { case (offset, head, tail) =>
      val base: Fn[Any] = tail.foldLeft(head) {
        case (g1: GetFn, g2: GetFn) =>
          val combinedPath = s"${g1.path}.${g2.path}"
          GetFn(combinedPath, isOptional = Utility.isPathOptional(combinedPath, ctx))

        case (idx: IndexFn, g2: GetFn) =>
          // keep the index, but we still represent the full string path for GetFn
          val combinedPath = s"${Utility.pathString(idx)}.${g2.path}"
          GetFn(combinedPath, isOptional = Utility.isPathOptional(combinedPath, ctx))

        case (g1: GetFn, idx: IndexFn) =>
          IndexFn(g1, idx.index)

        case (idx1: IndexFn, idx2: IndexFn) =>
          IndexFn(idx1, idx2.index)
      }
      println(s"[pathFn] offset=$offset")
      println(s"[pathFn] head=$head")
      println(s"[pathFn] tail=$tail")
      println(s"[pathFn] base=$base")
      println(s"[pathFn] ctx.schema.fields=${ctx.schema.fields.map(_.name)}")
      // Hand off to the method-call parser so .filter(...).distinct.sortDesc.limit(3) gets built
      methodChain(base)
    }

  // Parses: "." ident "(" args ")"
  private def methodCall[$: P](using ctx: ExprContext): P[Either[DLCompileError, (String, List[Fn[Any]], Int)]] =
    P(
      Index ~ // capture offset *before* the dot
        WS0 ~ "." ~ identifier.! ~
        "(" ~/ WS0 ~
        valueExpr.rep(sep = "," ~/ WS0) ~
        WS0 ~ ")"
    ).map { case (off, name, argsRaw) =>
      val (errs, oks) = argsRaw.partitionMap(identity)
      if errs.nonEmpty then Left(errs.head)
      else
        println(s"[methodCall] matched .$name(...) with ${argsRaw.size} args at offset $off")
        Right((name, oks.toList, off))
    }

  // parse optional fixed index and wrap.
  private def maybeIndex[$: P](fn: Fn[Any]): P[Fn[Any]] =
    P("[" ~ CharsWhileIn("0-9").! ~ "]").?.map {
      case Some(iStr) => IndexFn(fn, iStr.toInt)
      case None       => fn
    }

  private def segmentFn[$: P](using ctx: ExprContext): P[Fn[Any]] =
    P(identU.!).flatMap { name =>
      val isOpt = Utility.isPathOptional(name, ctx)
      // attach current collection parent (if any) as the receiver
      val recv: Option[Fn[Any]] = ctx.receiver.flatMap(_.parentFn)
      maybeIndex(GetFn(name, isOptional = isOpt, recv))
    }
//  private def segmentFn[$: P](using ctx: ExprContext): P[Fn[Any]] =
//    P(identU.!).flatMap { name =>
//      val isOpt = Utility.isPathOptional(name, ctx)
//      maybeIndex(GetFn(name, isOptional = isOpt))
//    }

  // If helpful, define the builder type somewhere central:
  // type MethodBuilder = (Fn[Any], List[Fn[Any]], Int) => Either[DLCompileError, Fn[Any]]

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
        case Left(err) =>
          println(s"[loop] methodCall error: $err")
          P(Pass(Left(err)))
        case Right((name, args, off)) =>
          println(s"[loop] got method name: $name, args: $args, off: $off")
          methodFunctions.get(name) match {
            case Some(build) =>
              println(s"[loop] found builder for $name")
              build(current, args, off, argsCtx) match {
                case Left(e) =>
                  println(s"[loop] builder for $name failed: $e")
                  P(Pass(Left(e)))
                case Right(n) =>
                  println(s"[loop] builder for $name succeeded: $n")
                  loop(n)
              }
            case None =>
              println(s"[loop] no builder for $name")
              P(Pass(Left(DLCompileError(off, s"Unknown method: $name"))))
          }
      } | P(Pass(Right(current)))

    // === 3) Optional trailing index (unchanged) ===
    loop(base).flatMap {
      case Left(err) => P(Pass(Left(err)))
      case Right(fn) => maybeIndex(fn).map(Right(_))
    }
  }

  def baseExpr[$: P](using ctx: ExprContext): P[ParseFnResult] =
    P((standaloneFn.map(Right(_)) | constant | pathFn).flatMap {
      case Right(fn)   => methodChain(fn)
      case l @ Left(_) => P(Pass(l))
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
      case other    => Left(DLCompileError(off, s"$name(...) expects a field path argument, got ${other.getClass.getSimpleName}"))
    }

  private def expectBoolean(arg: Fn[Any], name: String, off: Int)
                           (using ctx: ExprContext): Either[DLCompileError, BooleanFn] = {
    println(s"[expectBoolean] incoming Fn = $arg, recv? ${
      arg match
        case g: GetFn => g.recv
        case _ => "n/a"
    }")
    arg match {
      case b: BooleanFn => Right(b)
      case other => Left(DLCompileError(off,
        s"$name(...) requires a boolean expression, got ${other.getClass.getSimpleName}"))
    }
  }
//  private def expectBoolean(arg: Fn[Any], name: String, off: Int): Either[DLCompileError, BooleanFn] =
//    arg match {
//      case b: BooleanFn => Right(b)
//      case other        => Left(DLCompileError(off, s"$name(...) requires a boolean expression, got ${other.getClass.getSimpleName}"))
//    }

  private def expectConstInt(arg: Fn[Any], name: String, off: Int): Either[DLCompileError, Int] =
    arg match {
      case ConstantFn(i: Int) => Right(i)
      case ConstantFn(x)      => Left(DLCompileError(off, s"$name(...) requires an integer literal, got ${x.getClass.getSimpleName}"))
      case other              => Left(DLCompileError(off, s"$name(...) requires an integer literal, got ${other.getClass.getSimpleName}"))
    }

  private def checkArgs(
      fnName: String,
      args: List[Fn[Any]],
      required: Int,
      offset: Int
  ): Either[DLCompileError, Unit] =
    if args.length != required then Left(DLCompileError(offset, s"Function $fnName() expected $required argument(s), got ${args.length}"))
    else Right(())

  private val methodFunctions: Map[String, (Fn[Any], List[Fn[Any]], Int, ExprContext) => Either[DLCompileError, Fn[Any]]] = Map(
    M_MIN -> { (recv, _, _, _) => Right(MinFn(recv)) },
    M_MAX -> { (recv, _, _, _) => Right(MaxFn(recv)) },
    M_SUM -> { (recv, _, _, _) => Right(SumFn(recv)) },
    M_AVG -> { (recv, _, _, _) => Right(AvgFn(recv)) },
    M_MEDIAN -> { (recv, _, _, _) => Right(MedianFn(recv)) },
    M_ABS -> { (recv, _, _, _) => Right(AbsFn(recv)) },
    M_STARTSWITH -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_STARTSWITH, args, 1, off)
      } yield StartsWithFn(recv, args.head)
    },
    M_ENDSWITH -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_ENDSWITH, args, 1, off)
      } yield EndsWithFn(recv, args.head)
    },
    M_CONTAINS -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_CONTAINS, args, 1, off)
      } yield ContainsFn(recv, args.head)
    },
    M_KEYS -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_KEYS, args, 0, off)
      } yield KeysFn(recv)
    },
    M_VALUES -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_VALUES, args, 0, off)
      } yield ValuesFn(recv)
    },
    M_GET -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_GET, args, 1, off)
      } yield MapGetFn(recv, args.head)
    },
    M_EQUALSIGNORECASE -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_EQUALSIGNORECASE, args, 1, off)
      } yield EqualsIgnoreCaseFn(recv, args.head)
    },
    M_MATCHESREGEX -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_MATCHESREGEX, args, 1, off)
      } yield MatchesRegexFn(recv, args.head)
    },
    M_ELSE -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_ELSE, args, 1, off)
      } yield ElseFn(recv, args.head)
    },
    M_ISDEFINED -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_ISDEFINED, args, 0, off)
      } yield IsDefinedFn(recv)
    },
    M_LEN -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_LEN, args, 0, off)
      } yield LenFn(recv)
    },
    M_TOUPPERCASE -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_TOUPPERCASE, args, 0, off)
      } yield ToUpperFn(recv)
    },
    M_TOLOWERCASE -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_TOLOWERCASE, args, 0, off)
      } yield ToLowerFn(recv)
    },
    M_TRIM -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_TRIM, args, 0, off)
      } yield TrimFn(recv)
    },
    M_TEMPLATE -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_TEMPLATE, args, 0, off)
      } yield {
        val varMap = recv match
          case ConstantFn(s: String) =>
            TemplateUtils.extractVariables(s).map(v => v -> GetFn(v, isOptional = false)).toMap
          case _ =>
            Map.empty[String, Fn[Any]] // template is not a constant, so defer resolution
        InterpolateFn(recv, varMap)
      }
    },
    M_SUBSTR -> { (recv, args, off, _) =>
      if args.isEmpty then Left(DLCompileError(off, s"Function substr() expected at least 1 argument, but found none"))
      else
        val start = args.head.as[Int]
        val endOpt = args.lift(1).map(_.as[Int])
        Right(SubstringFn(recv, start, endOpt))
    },
    M_REPLACE -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_REPLACE, args, 2, off)
      } yield ReplaceFn(recv, args.head, args(1))
    },
    M_DATEFMT -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_DATEFMT, args, 1, off)
      } yield FormatDateFn(recv, args.head.as[String])
    },
    M_TODATE -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_TODATE, args, 1, off)
      } yield ParseDateFn(recv, args.head.as[String])
    },
    M_SORTASC -> { (recv, args, off, _) =>
      args match {
        case Nil =>
          Right(SortAscFn(recv, None))
        case a1 :: Nil =>
          expectGetPath(a1, M_SORTASC, off).map(p => SortAscFn(recv, Some(p)))
        case _ =>
          Left(DLCompileError(off, s"Function sortAsc() expected 0 or 1 argument(s), got ${args.length}"))
      }
    },
    M_SORTDESC -> { (recv, args, off, _) =>
      args match {
        case Nil =>
          Right(SortDescFn(recv, None))
        case a1 :: Nil =>
          expectGetPath(a1, M_SORTDESC, off).map(p => SortDescFn(recv, Some(p)))
        case _ =>
          Left(DLCompileError(off, s"Function sortDesc() expected 0 or 1 argument(s), got ${args.length}"))
      }
    },
    M_FILTER -> { (recv: Fn[Any], args: List[Fn[Any]], off: Int, ctx: ExprContext) =>
      println(s"[M_FILTER] incoming recv = $recv")

      for {
        _ <- checkArgs(M_FILTER, args, 1, off)

        // determine per-item type
        elemType   = Utility.elementTypeOf(recv)(using ctx)
        elemFields = elemType match
          case c: ClassType => c.fields.map(ft => ft.name -> ft).toMap
          case o: OptionType if o.valueType.isInstanceOf[ClassType] =>
            o.valueType.asInstanceOf[ClassType].fields.map(ft => ft.name -> ft).toMap
          case _ => Map.empty

        // --- NEW: build the receiver with parentFn AND push a scope for bare names like "qty"
        rcvr = Receiver("this", elemFields, elemType, parentFn = Some(recv))
        ctxWithRecvAndScope = {
          val childFields: List[FieldType] = elemType match
            case c: ClassType                               => c.fields
            case o: OptionType if o.valueType.isInstanceOf[ClassType] =>
              o.valueType.asInstanceOf[ClassType].fields
            case _                                          => Nil

          // expose element field names in the top scope
          (ctx.withReceiver(rcvr)).pushScope(childFields)
        }

        // just to verify:
        _ = println(s"[M_FILTER] top-scope symbols = ${ctxWithRecvAndScope.symbols.headOption.map(_.keys.toList)}")

          // ask the deferred node to compile itself *under the enriched ctx*
          compiled <- (args.head match
          case d: DeferredCompare => d.compile()(using ctxWithRecvAndScope)
          case b: BooleanFn       => Right(b)
          case other              => Left(DLCompileError(off,
            s"filter(...) requires a boolean expression, got ${other.getClass.getSimpleName}"))
          )

        // attach the collection-level receiver to the compiled predicate
        predWithRecv = compiled.withReceiver(recv)

      } yield {
        println(s"[M_FILTER] compiled predicate = $predWithRecv")
        FilterFn(recv, predWithRecv)
      }
    },
    //    M_FILTER -> { (recv: Fn[Any], args: List[Fn[Any]], off: Int, ctx: ExprContext) =>
//      println(s"[M_FILTER] raw args.head = ${args.head}")
//      for {
//        _ <- checkArgs(M_FILTER, args, 1, off)
//
//        // 1. Determine the element (per-item) type of the collection
//        elemType   = Utility.elementTypeOf(recv)(using ctx)
//        elemFields = elemType match
//          case c: ClassType => c.fields.map(ft => ft.name -> ft).toMap
//          case o: OptionType if o.valueType.isInstanceOf[ClassType] =>
//            o.valueType.asInstanceOf[ClassType].fields.map(ft => ft.name -> ft).toMap
//          case _ => Map.empty[String, FieldType]
//
//        // 2. Build a Receiver representing a single element
//        receiver = Receiver("this", elemFields, elemType)
//        _ = println(s"[M_FILTER] created receiver = $receiver")
//
//        // 3. Extend compile-time context with that receiver
//        given ExprContext = ctx.withReceiver(receiver)
//
//        // 4. Parse and type-check the predicate in this enriched context
//        // inside M_FILTER
//        pred <- args.head match
//          case c: ComparePlaceholder =>
//            c.compile()(using ctx)  // dispatch to CGreaterThanFn, etc.
//          case other =>
//            Left(DLCompileError(off,
//              s"filter(...) requires a boolean comparison, got ${other.getClass.getSimpleName}"))
//        _    = println(s"[M_FILTER] parsed predicate with receiver = $pred")
//
//        // 5. Also attach the collection-level Fn so 'this' inside pred is bound
//        predWithReceiver = pred.withReceiver(recv)
//      } yield FilterFn(recv, predWithReceiver)
//    },
    M_DISTINCT -> { (recv, args, off, _) =>
      args match {
        case Nil =>
          Right(DistinctFn(recv, None))
        case a1 :: Nil =>
          expectGetPath(a1, M_DISTINCT, off).map(p => DistinctFn(recv, Some(p)))
        case _ =>
          Left(DLCompileError(off, s"Function distinct() expected 0 or 1 argument(s), got ${args.length}"))
      }
    },
    M_LIMIT -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_LIMIT, args, 1, off)
        n <- expectConstInt(args.head, M_LIMIT, off) // must be an int literal
      } yield LimitFn(recv, n)
    },
    M_REVERSE -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_REVERSE, args, 0, off)
      } yield ReverseFn(recv)
    },
    M_CLEAN -> { (recv, args, off, _) =>
      for {
        _ <- checkArgs(M_CLEAN, args, 0, off)
      } yield CleanFn(recv)
    }
  )

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

  def collectionStmt[$: P](booleanExpr: ExprContext ?=> P[ParseBoolResult])(using ctx: ExprContext): P[ParseStmtResult] = {

    case object SortAscMethod extends CollectionMethodParser {
      val name: String = M_SORTASC

      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        P(Index ~ pathBase.?).map {
          case (_, None) =>
            // sortAsc() — natural ordering on the collection itself
            Right(SortAscFn(inner, None))

          case (offset, Some(rawKey)) =>
            // sortAsc(field) — rewrite/validate (possibly relative) key
            CorrectPath.rewritePath(rawKey, offset) match {
              case Left(err)       => Left(err)
              case Right(cleanKey) => Right(SortAscFn(inner, Some(cleanKey)))
            }
        }
    }

    case object SortDescMethod extends CollectionMethodParser {
      val name: String = M_SORTDESC

      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        P(Index ~ pathBase.?).map {
          case (_, None) =>
            Right(SortDescFn(inner, None))

          case (offset, Some(rawKey)) =>
            CorrectPath.rewritePath(rawKey, offset) match {
              case Left(err)       => Left(err)
              case Right(cleanKey) => Right(SortDescFn(inner, Some(cleanKey)))
            }
        }
    }

    case object FilterMethod extends CollectionMethodParser {
      val name: String = M_FILTER

      private def baseReceiver(fn: Fn[Any]): Fn[Any] =
        fn match
          case IdentityFn if fn.recv.nonEmpty => fn.recv.get.asInstanceOf[Fn[Any]]
          case _ => fn

      def parseFn[$: P](inner: Fn[Any])(using ctx: ExprContext): P[ParseFnResult] = {
        println(s"[FilterMethod] inner received = $inner, inner.recv = ${inner.recv}")

        // 1. Resolve the actual source (unwrap IdentityFn if present)
        val trueSource = baseReceiver(inner)
        println(s"[FilterMethod] trueSource = $trueSource, recv = ${trueSource.recv}")

        // 2. Determine element type using the original ctx, not the yet-to-be-enriched one
        val elemType = Utility.elementTypeOf(trueSource)(using ctx)
        println(s"[FilterMethod] elemType = $elemType")

        // 3. Collect element fields (for ClassType, Option[ClassType], etc.)
        val elemFields = elemType match
          case c: ClassType => c.fields.map(ft => ft.name -> ft).toMap
          case o: OptionType if o.valueType.isInstanceOf[ClassType] =>
            o.valueType.asInstanceOf[ClassType].fields.map(ft => ft.name -> ft).toMap
          case _ => Map.empty

        // 4. Build the per-element receiver with parentFn
        val receiver = Receiver("this", elemFields, elemType, parentFn = Some(inner))
        println(s"[FilterMethod] receiver = $receiver")

        // 5. Enrich the compile-time context with that receiver
        given ExprContext = ctx.withReceiver(receiver)

        // 6. Parse the predicate as a boolean expression
        booleanExpr.map {
          case Right(pred: BooleanFn) =>
            println(s"[FilterMethod] booleanExpr returned pred = $pred")
            // Attach collection receiver first
            val withRcvr = pred.withReceiver(inner)

            // If predicate is DeferredCompare, compile it to a concrete BooleanFn
            val finalPred: Either[DLCompileError, BooleanFn] =
              withRcvr match
                case dc: DeferredCompare =>
                  println(s"[FilterMethod] compiling DeferredCompare = $dc")
                  dc.compile()
                case b =>
                  println(s"[FilterMethod] predicate is already BooleanFn = $b")
                  Right(b)

            finalPred.map { compiled =>
              println(s"[FilterMethod] compiled predicate = $compiled")
              FilterFn(inner, compiled): Fn[Any]
            }

          case Left(err) =>
            println(s"[FilterMethod] booleanExpr error = $err")
            Left(err)
        }
      }
    }

    case object DistinctMethod extends CollectionMethodParser {
      val name: String = M_DISTINCT

      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        // optional field path
        P(Index ~ pathBase.?).map {
          case (_, None) =>
            // distinct() with no field ⇒ distinct by whole element
            Right(DistinctFn(inner, None))

          case (off, Some(rawKey)) =>
            // distinct(field) ⇒ rewrite/validate the (possibly relative) key
            CorrectPath.rewritePath(rawKey, off) match {
              case Left(err)       => Left(err)
              case Right(cleanKey) => Right(DistinctFn(inner, Some(cleanKey)))
            }
        }
    }

    case object LimitMethod extends CollectionMethodParser {
      val name: String = M_LIMIT

      def parseFn[$: P](inner: Fn[Any])(using ctx: ExprContext): P[ParseFnResult] =
        P(Index ~ number).map { case (off, n) =>
          if n >= 0 then Right(LimitFn(inner, n))
          else Left(DLCompileError(off, s"limit(...) requires a non-negative integer, found $n"))
        }

      private def number[$: P]: P[Int] =
        P(CharsWhileIn("0-9", min = 1).!).map(_.toInt)
    }

    case object ReverseMethod extends CollectionMethodParser {
      val name = M_REVERSE

      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        P(Index ~ (("(" ~ WS0 ~ ")").?)).map { _ =>
          val recv = promoteToCollection(inner)
          Right(ReverseFn(recv))
        }
    }

    case object CleanMethod extends CollectionMethodParser {
      val name = M_CLEAN

      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        P(Index ~ (("(" ~ WS0 ~ ")").?)).map { _ =>
          val recv = promoteToCollection(inner)
          Right(CleanFn(recv))
        }
    }

    case object MapToMethod extends CollectionMethodParser {
      val name = "mapTo"

      def parseFn[$: P](inner: Fn[Any])(using ExprContext): P[ParseFnResult] =
        // We capture the current offset for good error messages
        P(Index ~ stringLiteral).map { case (idx, strRes) =>
          strRes match {
            case Right(ConstantFn(mapName: String)) =>
              Right(MapFwdFn(mapName, inner)) // success: build the function

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
            Right(MapRevFn(mapName, inner))
          case (offset, Right(_)) =>
            Left(DLCompileError(offset, "mapFrom(...) requires a string literal argument"))
          case (_, Left(err)) =>
            Left(err) // propagate any error produced by stringLiteral
        }
    }

    val collectionMethodRegistry: Map[String, CollectionMethodParser] =
      List(SortAscMethod, SortDescMethod, FilterMethod, DistinctMethod, LimitMethod, ReverseMethod, CleanMethod, MapToMethod, MapFromMethod)
        .map(m => m.name -> m)
        .toMap

    def parseMethodArgs[$: P](parser: CollectionMethodParser, recv: Fn[?])(using ExprContext): P[ParseFnResult] =
      P("(" ~/ WS0 ~ {
        println(s"[parseMethodArgs] passing recv into ${parser.name}: $recv")
        parser.parseFn(IdentityFn(recv))
      } ~ WS0 ~ ")")

    def lookupMethod[$: P](name: String): P[Either[DLCompileError, CollectionMethodParser]] =
      P(Index).map { off =>
        collectionMethodRegistry
          .get(name)
          .toRight(DLCompileError(off, s"Unknown collection method: $name"))
      }

    P(Index ~ path).flatMap { case (off0, basePath) =>
      // 1) Add a correctly-typed 'this' based on the base path (sets receiver + 'this' symbol)
      val ctxWithThis = Utility.addThisType(basePath, ctx)

      // 2) elemSchema is now the single FieldType (if any) at basePath
      val maybeField: Option[FieldType] = Utility.elementSchemaFor(basePath, ctxWithThis.schema)

      // Extract the actual child fields if the field is a class or wraps a class
      val childFields: List[FieldType] = maybeField match {
        case Some(c: ClassType) => c.fields
        case Some(o: OptionType) =>
          o.valueType match {
            case c: ClassType => c.fields
            case _ => Nil
          }
        case Some(l: ListType) =>
          l.elementType match {
            case c: ClassType => c.fields
            case _ => Nil
          }
        case _ => Nil
      }

      // Only push a new scope if there are nested fields to expose
      val ctxForArgs =
        if childFields.nonEmpty then ctxWithThis.pushScope(childFields)
        else ctxWithThis

      // Build the actual collection receiver Fn for this basePath
      val baseFn: Fn[?] = GetFn(basePath, isOptional = Utility.isPathOptional(basePath, ctxWithThis))

      given ExprContext = ctxForArgs

      // resolve + parse a single method’s args into a Fn
      def parseOneMethod(name: String, recv: Fn[?]): P[ParseFnResult] =
        for {
          resolved <- lookupMethod(name)
          fnRes <- resolved match {
            case Left(err)  => P(Pass(Left(err)))
            case Right(par) => parseMethodArgs(par, recv)
          }
        } yield fnRes

      // --- REQUIRE a first method: ".name(" ---
      val firstMethodNameP: P[String] =
        P(WS0 ~ "." ~ identifier.!).flatMap { name =>
          P(&("(")).map(_ => name)
        }

      firstMethodNameP.flatMap { firstMethodName =>
        for {
          firstResult <- parseOneMethod(firstMethodName, baseFn)

          // zero+ additional methods, each must also be followed by '('
          moreNames <- P((WS0 ~ "." ~ identifier.!).flatMap { n =>
            P(&("(")).map(_ => n)
          }).rep

          restFns <- moreNames.foldLeft(Pass(Right(Nil): ParseFnListResult): P[ParseFnListResult]) { case (accP, methodName) =>
            accP.flatMap {
              case left @ Left(_) => P(Pass(left)) // keep first error
              case Right(accum) =>
                parseOneMethod(methodName, baseFn).map {
                  case Left(err) => Left(err)
                  case Right(fn) => Right(accum :+ fn)
                }
            }
          }
        } yield
        // Combine into a single Fn, then into a MapStmt, and wrap as ParseStmtResult
        (for {
          f1 <- firstResult
          rxs <- restFns
        } yield {
          val all = f1 :: rxs
          val fn = if all.size == 1 then all.head else PolyFn(all)
          (ctx, MapStmt(basePath, fn)) // <- keep outer ctx here (don’t leak ctxForArgs)
        }): ParseStmtResult
      }
    }
  }
}
