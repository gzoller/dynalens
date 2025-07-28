package co.blocke.dynalens
package parser

import fastparse.*, NoWhitespace.*

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

  def path[$: P]: P[(String, Option[String])] = {

    def fullPath[$: P]: P[String] =
      P(CharsWhileIn("a-zA-Z0-9_.[]").rep(1).!) //.map{h=>println("S: "+h);h}

    // Non-consuming lookahead: detects if next char is '('
    def isFunc[$: P]: P[Boolean] =
      P(
        &("(").map(_ => true)
          | Pass.map(_ => false)
      )

    // Combine an
    // //.map{x => println("S2: "+x);x}d post-process
    (fullPath ~ isFunc).map {
      case (raw, hasFunc) =>
        println(s"Raw: $raw  HasFunc: $hasFunc")
        if hasFunc then
          val lastDot = raw.lastIndexOf('.')
          if lastDot == -1 then
            (raw, None)
          else
            val base = raw.take(lastDot)
            val fn = raw.drop(lastDot + 1)
            (base, Some(fn))
        else
          (raw, None)
    }
  }

  private def pathFn[$: P](valueExpr: => P[Fn[Any]]): P[Fn[Any]] =
    P(path).flatMap {
      case (rawPath, maybeMethod) =>
        val baseFn = GetFn(rawPath)

        maybeMethod match
          case None => P(Pass(baseFn))
          case Some(methodName) =>
            methodFunctions.get(methodName) match
              case Some(fnBuilder) =>
                P(methodArgs(valueExpr).map(args => fnBuilder(baseFn, args)))
              case None =>
                throw new RuntimeException(s"Unknown method: $methodName")
    }

  private def methodArgs[$: P](valueExpr: => P[Fn[Any]]): P[List[Fn[Any]]] =
    P("(" ~ valueExpr.rep(sep = "," ~ WS0) ~ ")" ~ WS0).map(_.toList)

  private def methodCall[$: P](valueExpr: => P[Fn[Any]]): P[(String, List[Fn[Any]])] =
    P("." ~ identifier.! ~ methodArgs(valueExpr))
      .map { case (name, args) => (name, args.toList) }

  def baseExpr[$: P](valueExpr: => P[Fn[Any]]): P[Fn[Any]] =
    def methodChain[$: P](base: Fn[Any]): P[Fn[Any]] =
      P(methodCall(valueExpr).rep).map { chain =>
        chain.foldLeft(base) { case (inner, (fnName, args)) =>
          methodFunctions.get(fnName) match
            case Some(fnBuilder) => fnBuilder(inner, args).asInstanceOf[Fn[Any]]
            case None => throw new RuntimeException(s"Unknown method: $fnName")
        }
      }
    P(constant | pathFn(valueExpr) | standaloneFn).flatMap(methodChain) ~ WS0


  // ---- Functions ----

  def standaloneFn[$: P]: P[Fn[Any]] =
    P(
      StringIn("now", "uuid") ~ "(" ~ ")"
    ).!.map {
      case "now" => NowFn
      case "uuid" => UUIDFn
    }

  private def checkArgs(fnName: String, args: List[Fn[Any]], required: Int): Unit =
    if args.length != required then
      throw new RuntimeException(s"Function ${fnName}() expected $required argument(s), got ${args.length}")

  val methodFunctions: Map[String, (Fn[Any], List[Fn[Any]]) => Fn[?]] = Map(
    "startsWith" -> { (recv, args) =>
      checkArgs("startsWith", args, 1)
      StartsWithFn(recv.as[String], args.head.as[String])
    },
    "endsWith" -> { (recv, args) =>
      checkArgs("endsWith", args, 1)
      EndsWithFn(recv.as[String], args.head.as[String])
    },
    "contains" -> { (recv, args) =>
      checkArgs("contains", args, 1)
      ContainsFn(recv.as[String], args.head.as[String])
    },
    "equalsIgnoreCase" -> { (recv, args) =>
      checkArgs("equalsIgnoreCase", args, 1)
      EqualsIgnoreCaseFn(recv.as[String], args.head.as[String])
    },
    "matchesRegex" -> { (recv, args) =>
      checkArgs("matchesRegex", args, 1)
      MatchesRegexFn(recv.as[String], args.head.as[String])
    },
    "len" -> { (recv, _) =>
      LengthFn(recv)
    },
    "toUpperCase" -> { (recv, _) =>
      ToUpperFn(recv)
    },
    "toLowerCase" -> { (recv, _) =>
      ToLowerFn(recv)
    },
    "trim" -> { (recv, _) =>
      TrimFn(recv)
    },
    "template" -> { (recv, _) =>
      val varMap = recv match
        case ConstantFn(s: String) =>
          TemplateUtils.extractVariables(s).map(v => v -> GetFn(v)).toMap
        case _ =>
          Map.empty[String, Fn[Any]] // template is not a constant, so defer resolution
      InterpolateFn(recv, varMap)
    },
    "substr" -> { (recv, args) =>
      if args.isEmpty then throw new RuntimeException("substr() requires at least 1 argument")
      val start = args.head.as[Int]
      val endOpt = args.lift(1).map(_.as[Int])
      SubstringFn(recv, start, endOpt)
    },
    "replace" -> { (recv, args) =>
      checkArgs("replace", args, 2)
      ReplaceFn(recv, args.head, args(1))
    },
    "dateFmt" -> { (recv, args) =>
      checkArgs("dateFmt", args, 1)
      FormatDateFn(recv, args.head.as[String])
    },
    "toDate" -> { (recv, args) =>
      checkArgs("toDate", args, 1)
      ParseDateFn(recv, args.head.as[String])
    }
  )

  // ---- Collection Statements ----

  def collectionStmt[$: P](valueExpr: => P[Fn[Any]]): P[Statement] =
    P(path).flatMap {
      case (basePath, Some(fnName)) =>
        P(methodArgs(valueExpr) ~ methodCall(valueExpr).rep).map {
          case (firstArgs, restMethods) =>
            val fullChain = (fnName, firstArgs) :: restMethods.toList
            val composedFn = fullChain.foldLeft[Fn[Any]](IdentityFn) {
              case (innerFn, (methodName, args)) =>
                collectionStatementFns.get(methodName) match
                  case Some(fnBuilder) => fnBuilder(innerFn, args)
                  case None => throw new RuntimeException(s"Unknown collection method: $methodName")
            }
            MapStmt(basePath, composedFn)
        }

      case (_, None) =>
        Fail.opaque("Collection statement must have at least one method")
    }

val collectionStatementFns: Map[String, (Fn[Any], List[Fn[Any]]) => Fn[Any]] = Map(
  "filter" -> {
    case (_, List(pred: BooleanFn)) => FilterFn(pred)
  },
  "sortAsc" -> {
    case (_, List(GetFn(path))) => SortFn(Some(path))
    case (_, Nil) => SortFn(None)
    case (_, args) => throw RuntimeException(s"Invalid arguments for sortAsc: $args")
  },
  "sortDesc" -> {
    case (_, List(GetFn(path))) => SortFn(Some(path), asc = false)
    case (_, Nil) => SortFn(None, asc = false)
    case (_, args) => throw RuntimeException(s"Invalid arguments for sortDesc: $args")
  },
  "distinct" -> {
    case (_, List(GetFn(path))) => DistinctFn(Some(path))
    case (_, List(ConstantFn(s: String))) => DistinctFn(Some(s))
    case (_, args) => throw RuntimeException(s"Invalid arguments for distinct: $args")
  },
  "limit" -> {
    case (_, List(ConstantFn(i: Int))) => LimitFn(i)
    case (_, args) => throw RuntimeException(s"Invalid arguments for limit: $args")
  },
  "reverse" -> {
    case (_, Nil) => ReverseFn()
    case (_, args) => throw RuntimeException(s"reverse does not take arguments: $args")
  },
  "clean" -> {
    case (_, Nil) => CleanFn()
    case (_, args) => throw RuntimeException(s"clean does not take arguments: $args")
  }
)

//  def collectionStmt[$: P](valueExpr: => P[Fn[Any]]): P[Statement] =
//    P(collectionStmtPathAndMethods(valueExpr)).map {
//      case (collectionPath, methodCalls) =>
//        println(s"[collectionStmt] path: $collectionPath")
//        println(s"[collectionStmt] methods: $methodCalls")
//        val composedFn = methodCalls.foldLeft[Fn[Any]](IdentityFn) {
//          case (innerFn, (methodName, args)) =>
//            collectionStatementFns.get(methodName) match
//              case Some(fnBuilder) => fnBuilder(innerFn, args)
//              case None => throw new RuntimeException(s"Unknown collection method: $methodName")
//        }
//        MapStmt(collectionPath, composedFn)
//    }