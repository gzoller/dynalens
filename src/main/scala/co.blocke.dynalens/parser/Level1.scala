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
  def path[$: P]: P[String] = {  // path with no following ws allowed

    def dotField[$: P]: P[String] =
      P(&("." ~ identifier ~ "(").!.?).flatMap {
        // Prevent matching function calls like `.foo(...)`
        case Some(_) => Fail
        case None => P("." ~/ identifier).map("." + _)
      }.log("DOT_FIELD")

    def arrayIndex[$: P]: P[String] =
      P("[" ~/ CharsWhileIn("0-9").! ~ "]").map("[" + _ + "]").log("ARRAY_INDEX")

    def arrayAll[$: P]: P[String] =
      P("[" ~ "]").map(_ => "[]").log("ARRAY_ALL")

    def segment[$: P]: P[String] =
      P(arrayAll | arrayIndex | dotField)

    P(
      // Prevent this rule from running if we're about to enter a method chain
      !("." ~ collectionMethodName ~ "(") ~
        (arrayAll | arrayIndex | dotField).rep(1, sep = ".").!.map { p =>
          println(s"PATH CAPTURED: $p")
          p
        }
    ).log("PATH")
//    P(identifier.! ~ segment.rep).map { case (head, tail) =>
//      tail.foldLeft(head)(_ + _)
//    }.log("PATH")
  }


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

  // Greedily captures a path ending in at least one [] and parses chained method calls.
  def collectionMethodName[$: P]: P[String] =
    P(StringIn("filter", "sortAsc", "sortDesc", "distinct", "limit", "reverse", "clean").!)

  def collectionStmtPathAndMethods[$: P](valueExpr: => P[Fn[Any]]): P[(String, List[(String, List[Fn[Any]])])] = {

//    def methodCall[$: P]: P[(String, List[Fn[Any]])] =
//      P("." ~ collectionMethodName.!.map(name => {
//        println("meth name: " + name); name
//      }) ~
//        "(" ~/
//        valueExpr.rep(sep = "," ~ WS0).map { args =>
//          println("Args: " + args.map(_.getClass.getSimpleName).mkString(", "))
//          args.toList
//        } ~
//        ")"
//      ).map { (name, args) =>
//        println(s"[methodCall] parsed method '$name' with args: " + args.map(_.getClass.getSimpleName).mkString(", "))
//        (name, args)
//      }

//    def methodCall[$: P]: P[(String, List[Fn[Any]])] =
//      P("." ~ identifier.!.map { f => println("fn id: " + f); f } ~
//        "(" ~/ CharsWhile(_ != ')').!.map { v => println("RAW V: " + v); GreaterThanFn(GetFn("this.qty"), ConstantFn(4)) }.rep(sep = "," ~ WS0) ~ ")" ~ WS0)
//        .map { case (name, args) =>
//          println("HEY: " + name)
//          (name, args.toList)
//        }
    def methodCall[$: P]: P[(String, List[Fn[Any]])] =
      P("." ~ identifier.!.map { f => println("fn id: " + f); f } ~
        "(" ~/ valueExpr.map { v => println("V: " + v); v }.rep(sep = "," ~ WS0) ~ ")" ~ WS0)
        .map { case (name, args) =>
          println(s"[methodCall.map] name=$name, args=${args.map(_.getClass.getSimpleName).mkString(",")}")
          (name, args.toList)
        }

    def methodCall[$: P]: P[(String, List[Fn[Any]])] =
      P("." ~ identifier.! ~ "(" ~/ valueExpr.rep(sep = "," ~ WS0) ~ ")" ~ WS0)
        .map { case (name, args) => (name, args.toList) }
      
    def collectionPath[$: P]: P[String] =
      P(CharsWhile(_ != '.').! ~ &("." ~ collectionMethodName ~ "("))
        .map(_.trim)
        .flatMap { path =>
          if path.endsWith("[]") then Pass(path)
          else Fail.opaque("collectionPath must end with []")
        }
        .log("COLLECTION_PATH")

//    P(Index ~ collectionPath.map{z=>println("Captured path: "+z);z} ~ Index).flatMap {
//      case (start, path, end) =>
//        // println(s"COLLECT path: $path from $start to $end")
//        P(methodCalls).map(methods => (path, methods))
//    }
    P(collectionPath.map{z=>println("Captured path: "+z);z}.opaque("collectionPath done") ~ methodCalls)
      .map { case (p, methods) =>
        println(s"[collectionStmt] Got: path=$p, methods=$methods")
        CollectionStmt(p, methods)
      }
//    P(collectionPath.map{z=>println("Captured path: "+z);z} ~ methodCalls)
  }

  def collectionStmt[$: P](valueExpr: P[Fn[Any]]): P[Statement] =
    P(collectionStmtPathAndMethods(valueExpr)).map {
      case (collectionPath, methodCalls) =>
        println(s"[collectionStmt] path: $collectionPath")
        println(s"[collectionStmt] methods: $methodCalls")
        val composedFn = methodCalls.foldLeft[Fn[Any]](IdentityFn) {
          case (innerFn, (methodName, args)) =>
            collectionStatementFns.get(methodName) match
              case Some(fnBuilder) => fnBuilder(innerFn, args)
              case None => throw new RuntimeException(s"Unknown collection method: $methodName")
        }
        MapStmt(collectionPath, composedFn)
    }

  val collectionStatementFns: Map[String, (Fn[Any], List[Any]) => Fn[Any]] = Map(
    "filter" -> { case (_, List(pred: BooleanFn)) => FilterFn(pred) },
    "sortAsc" -> { case (_, List(field: String)) => SortFn(Some(field)) },
    "sortDesc" -> { case (_, List(field: String)) => SortFn(Some(field), asc = false) },
    "distinct" -> { case (_, List(field: String)) => DistinctFn(Some(field)) },
    "limit" -> { case (_, List(value: Int)) => LimitFn(value) },
    "reverse" -> { case (_, Nil) => ReverseFn() },
    "clean" -> { case (_, Nil) => CleanFn() }
  )