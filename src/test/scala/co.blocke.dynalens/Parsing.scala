package co.blocke.dynalens

import zio._
import zio.test._
import zio.test.Assertion._

import DynaLens.*
import parser.Parser

object Parsing extends ZIOSpecDefault:

  def spec = suite("Parsing Tests") (
    test("Simple val assignment script test") {
      val script =
        """
          |  val x = 42
          |  val y = x + 8
          |""".stripMargin
      val compiledScript = Parser.parseScript(script)
      val expectedCompiled = """BlockStmt(List(ValStmt(x,ConstantFn(42)), ValStmt(y,AddFn(GetFn(x),ConstantFn(8)))))"""
      val expectedResult = """top -> Item(abc,2,5)
                             |x -> 42
                             |y -> 50""".stripMargin + "\n"
      val inst = Item("abc",2,5)
      val a = dynalens[Item]
      for{
        (x,newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == inst && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Path and operator precedence") {
      val script =
        """
          |  val x = 3 + items[1].num * 2
          |  val y = (3 + items[1].num) * 2
          |""".stripMargin
      val compiledScript = Parser.parseScript(script)
      val expectedCompiled = """BlockStmt(List(ValStmt(x,AddFn(ConstantFn(3),MultiplyFn(GetFn(items[1].num),ConstantFn(2)))), ValStmt(y,MultiplyFn(AddFn(ConstantFn(3),GetFn(items[1].num)),ConstantFn(2)))))"""
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,2,5), Item(xyz,1,7)),1)
                             |x -> 17
                             |y -> 20""".stripMargin + "\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      val a = dynalens[Shipment]
      for{
        (x,newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == inst && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Map statement") {
      val script =
        """
          |  items[].num = 99/3
          |""".stripMargin
      val compiledScript = Parser.parseScript(script)
      val expectedCompiled = """BlockStmt(List(MapStmt(items[].num,DivideFn(ConstantFn(99),ConstantFn(3)))))"""
      val expectedResult = "top -> Shipment(aaa,List(Item(abc,2,33), Item(xyz,1,33)),1)\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      val a = dynalens[Shipment]
      for{
        (x,newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa",List(Item("abc",2,33), Item("xyz",1,33)),1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Update statement") {
      val script =
        """
          |  items[0].num = 1
          |""".stripMargin
      val compiledScript = Parser.parseScript(script)
      val expectedCompiled = """BlockStmt(List(UpdateStmt(items[0].num,ConstantFn(1))))"""
      val expectedResult = "top -> Shipment(aaa,List(Item(abc,2,1), Item(xyz,1,7)),1)\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      val a = dynalens[Shipment]
      for{
        (x,newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa",List(Item("abc",2,1), Item("xyz",1,7)),1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("None-empty statement BlockFn") {
      val script =
        """
          |  items[0].num = {
          |    val z = "wow"
          |    val y = 1
          |    y
          |  }
          |""".stripMargin
      val compiledScript = Parser.parseScript(script)
      val expectedCompiled = """BlockStmt(List(UpdateStmt(items[0].num,BlockFn(List(ValStmt(z,ConstantFn(wow)), ValStmt(y,ConstantFn(1))),GetFn(y)))))"""
      val expectedResult = "top -> Shipment(aaa,List(Item(abc,2,1), Item(xyz,1,7)),1)\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      val a = dynalens[Shipment]
      for{
        (x,newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa",List(Item("abc",2,1), Item("xyz",1,7)),1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("None-empty statement BlockFn") {
      val script =
        """
          |  items[0].num = {
          |    1
          |  }
          |""".stripMargin
      val compiledScript = Parser.parseScript(script)
      val expectedCompiled = """BlockStmt(List(UpdateStmt(items[0].num,BlockFn(List(),ConstantFn(1)))))"""
      val expectedResult = "top -> Shipment(aaa,List(Item(abc,2,1), Item(xyz,1,7)),1)\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      val a = dynalens[Shipment]
      for{
        (x,newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa",List(Item("abc",2,1), Item("xyz",1,7)),1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("If statement-no else") {
      val script =
        """
          |  if items[0].num >= 0 then
          |    items[0].num = 12
          |  if items[1].num < 5 then
          |    items[1].num = 99
          |""".stripMargin
      val compiledScript = Parser.parseScript(script)
      val expectedCompiled = """BlockStmt(List(IfStmt(GreaterThanOrEqualFn(GetFn(items[0].num),ConstantFn(0)),UpdateStmt(items[0].num,ConstantFn(12)),None), IfStmt(LessThanFn(GetFn(items[1].num),ConstantFn(5)),UpdateStmt(items[1].num,ConstantFn(99)),None)))"""
      val expectedResult = "top -> Shipment(aaa,List(Item(abc,2,12), Item(xyz,1,7)),1)\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      val a = dynalens[Shipment]
      for{
        (x,newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa",List(Item("abc",2,12), Item("xyz",1,7)),1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("If statement-with else") {
      val script =
        """
          |  if items[0].num >= 0 then
          |    items[0].num = 12
          |  if items[1].num < 5 then
          |    items[1].num = 99
          |  else {
          |    val z = -1
          |    items[1].num = z
          |  }
          |""".stripMargin
      val compiledScript = Parser.parseScript(script)
      val expectedCompiled = """BlockStmt(List(IfStmt(GreaterThanOrEqualFn(GetFn(items[0].num),ConstantFn(0)),UpdateStmt(items[0].num,ConstantFn(12)),None), IfStmt(LessThanFn(GetFn(items[1].num),ConstantFn(5)),UpdateStmt(items[1].num,ConstantFn(99)),Some(BlockStmt(List(ValStmt(z,ConstantFn(-1)), UpdateStmt(items[1].num,GetFn(z))))))))"""
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,2,12), Item(xyz,1,-1)),1)
                             |z -> -1""".stripMargin + "\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      val a = dynalens[Shipment]
      for{
        (x,newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa",List(Item("abc",2,12), Item("xyz",1,-1)),1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("If fn") {
      val script =
        """
          |  val z = if items[0].num >= 0 then 2 else 3
          |  items[].qty = items.qty * z
          |""".stripMargin
      val compiledScript = Parser.parseScript(script)
      val expectedCompiled = """BlockStmt(List(ValStmt(z,IfFn(GreaterThanOrEqualFn(GetFn(items[0].num),ConstantFn(0)),ConstantFn(2),ConstantFn(3))), MapStmt(items[].qty,MultiplyFn(GetFn(items.qty),GetFn(z)))))"""
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,4,5), Item(xyz,2,7)),1)
                             |z -> 2""".stripMargin + "\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      val a = dynalens[Shipment]
      for{
        (x,newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa",List(Item("abc",4,5), Item("xyz",2,7)),1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("If fn with blockFn") {
      val script =
        """
          | # My little script comment
          |  items[].qty = if items.qty < 2 then 0 else {
          |    val y = items.num  # A comment
          |    y * 2
          |  }
          |""".stripMargin
      val compiledScript = Parser.parseScript(script)
      val expectedCompiled = """BlockStmt(List(MapStmt(items[].qty,IfFn(LessThanFn(GetFn(items.qty),ConstantFn(2)),ConstantFn(0),BlockFn(List(ValStmt(y,GetFn(items.num))),MultiplyFn(GetFn(y),ConstantFn(2)))))))"""
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,10,5), Item(xyz,0,7)),1)""" + "\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      val a = dynalens[Shipment]
      for{
        (x,newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa",List(Item("abc",10,5), Item("xyz",0,7)),1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("And, or, and Not") {
      val script =
        """
          |  val x = if items[0].num >= 2 && !(items[0].num > 10) || items[1].num == 8 then 7 else 1
          |  val y = true
          |  val z = !y
          |  if z then
          |    num = 22
          |  else
          |    num = x
          |""".stripMargin
      val compiledScript = Parser.parseScript(script)
      val expectedCompiled = """BlockStmt(List(ValStmt(x,IfFn(OrFn(AndFn(GreaterThanOrEqualFn(GetFn(items[0].num),ConstantFn(2)),NotFn(GreaterThanFn(GetFn(items[0].num),ConstantFn(10)))),EqualFn(GetFn(items[1].num),ConstantFn(8))),ConstantFn(7),ConstantFn(1))), ValStmt(y,ConstantFn(true)), ValStmt(z,NotFn(GetFn(y))), IfStmt(GetFn(z),UpdateStmt(num,ConstantFn(22)),Some(UpdateStmt(num,GetFn(x))))))"""
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,2,5), Item(xyz,1,7)),7)
                             |x -> 7
                             |y -> true
                             |z -> false""".stripMargin + "\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      val a = dynalens[Shipment]
      for{
        (x,newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),7) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("String functions work") {
      val script =
        """
          |  val x = "FooBar  "
          |  val y = x.trim().toLowerCase().contains("oob")
          |  val z = x.toUpperCase()
          |  val q = x.matchesRegex("F.*?B.*")
          |""".stripMargin
      val compiledScript = Parser.parseScript(script)
      val expectedCompiled = """BlockStmt(List(ValStmt(x,ConstantFn(FooBar  )), ValStmt(y,ContainsFn(ToLowerFn(TrimFn(GetFn(x))),ConstantFn(oob))), ValStmt(z,ToUpperFn(GetFn(x))), ValStmt(q,MatchesRegexFn(GetFn(x),ConstantFn(F.*?B.*)))))"""
      def normalize(str: String): Set[String] = str.trim.linesIterator.map(_.trim).filter(_.nonEmpty).toSet
      val expectedResult = """top -> Item(abc,2,5)
                             |x -> FooBar
                             |y -> true
                             |z -> FOOBAR
                             |q -> true
                             |""".stripMargin + "\n"
      val inst = Item("abc",2,5)
      val a = dynalens[Item]
      for{
        (x,newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == inst && normalize(resultStr) == normalize(expectedResult) && compiledScript.toString == expectedCompiled)
    },
    test("More String functions work") {
      val script =
        """
          |  val x = "FooBar"
          |  val y = x.substr(3)
          |  val s = 1
          |  val e = 3
          |  val z = x.substr(s,e)
          |  val q = "YooHoo"
          |  val r = q.replace("oo","aa")
          |""".stripMargin
      val compiledScript = Parser.parseScript(script)
      val expectedCompiled = """BlockStmt(List(ValStmt(x,ConstantFn(FooBar)), ValStmt(y,SubstringFn(GetFn(x),ConstantFn(3),None)), ValStmt(s,ConstantFn(1)), ValStmt(e,ConstantFn(3)), ValStmt(z,SubstringFn(GetFn(x),GetFn(s),Some(GetFn(e)))), ValStmt(q,ConstantFn(YooHoo)), ValStmt(r,ReplaceFn(GetFn(q),ConstantFn(oo),ConstantFn(aa)))))"""
      def normalize(str: String): Set[String] = str.trim.linesIterator.map(_.trim).filter(_.nonEmpty).toSet
      val expectedResult = """top -> Item(abc,2,5)
                             |x -> FooBar
                             |y -> Bar
                             |s -> 1
                             |e -> 3
                             |z -> oo
                             |q -> YooHoo
                             |r -> YaaHaa
                             |""".stripMargin + "\n"
      val inst = Item("abc",2,5)
      val a = dynalens[Item]
      for{
        (x,newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == inst && normalize(resultStr) == normalize(expectedResult) && compiledScript.toString == expectedCompiled)
    },
    test("Even more String functions work") {
      val script =
        """
          |  val x = "Mike"
          |  val y = 12.45
          |  val t = "Hello {x}, your balance is ${y}"
          |  val u = t.interpolate()
          |""".stripMargin
      val compiledScript = Parser.parseScript(script)
      val expectedCompiled = """BlockStmt(List(ValStmt(x,ConstantFn(Mike)), ValStmt(y,ConstantFn(12.45)), ValStmt(t,ConstantFn(Hello {x}, your balance is ${y})), ValStmt(u,InterpolateFn(GetFn(t),Map()))))"""
      def normalize(str: String): Set[String] = str.trim.linesIterator.map(_.trim).filter(_.nonEmpty).toSet
      val expectedResult = """top -> Item(abc,2,5)
                             |x -> Mike
                             |y -> 12.45
                             |t -> Hello {x}, your balance is ${y}
                             |u -> Hello Mike, your balance is $12.45""".stripMargin + "\n"
      val inst = Item("abc",2,5)
      val a = dynalens[Item]
      for{
        (x,newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == inst && normalize(resultStr) == normalize(expectedResult) && compiledScript.toString == expectedCompiled)
    },
    test("String literals work") {
      val script =
        """
          |  val x = "Mike"
          |  val y = 12.4
          |  val z = "Blip".contains("li")
          |  val u = "Hello {x}, your balance is {y%.2f}".interpolate()
          |  val q = "wow " :: y*2 :: number
          |""".stripMargin
      val compiledScript = Parser.parseScript(script)
      val expectedCompiled = """BlockStmt(List(ValStmt(x,ConstantFn(Mike)), ValStmt(y,ConstantFn(12.4)), ValStmt(z,ContainsFn(ConstantFn(Blip),ConstantFn(li))), ValStmt(u,InterpolateFn(ConstantFn(Hello {x}, your balance is {y%.2f}),Map(x -> GetFn(x), y -> GetFn(y)))), ValStmt(q,ConcatFn(List(ConstantFn(wow ), MultiplyFn(GetFn(y),ConstantFn(2)), GetFn(number))))))"""
      def normalize(str: String): Set[String] = str.trim.linesIterator.map(_.trim).filter(_.nonEmpty).toSet
      val expectedResult = """top -> Item(abc,2,5)
                             |x -> Mike
                             |y -> 12.4
                             |z -> true
                             |q -> wow 24.8abc
                             |u -> Hello Mike, your balance is 12.40""".stripMargin + "\n"
      val inst = Item("abc",2,5)
      val a = dynalens[Item]
      for{
        (x,newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == inst && normalize(resultStr) == normalize(expectedResult) && compiledScript.toString == expectedCompiled)
    },
    test("Path expressions in string interpolation works") {
      val script =
        """
          |  val x = "Thingy = {items[1].num%.3f}".interpolate()
          |""".stripMargin
      val compiledScript = Parser.parseScript(script)
      val expectedCompiled = """BlockStmt(List(ValStmt(x,InterpolateFn(ConstantFn(Thingy = {items[1].num%.3f}),Map(items[1].num -> GetFn(items[1].num))))))"""
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,2,5), Item(xyz,1,7)),1)
                             |x -> Thingy = 7.000""".stripMargin + "\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      val a = dynalens[Shipment]
      for{
        (x,newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == inst && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Length function work") {
      val script =
        """
          |  val x = items[].len()
          |  val y = "Foobar".len()
          |""".stripMargin
      val compiledScript = Parser.parseScript(script)
      val expectedCompiled = """BlockStmt(List(ValStmt(x,LengthFn(GetFn(items[]))), ValStmt(y,LengthFn(ConstantFn(Foobar)))))"""
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,2,5), Item(xyz,1,7)),1)
                             |x -> 2
                             |y -> 6""".stripMargin + "\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      val a = dynalens[Shipment]
      for{
        (x,newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == inst && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("'this' must work in map()") {
      val script =
        """
          |  items[].qty = this * 3 // this references qty
          |""".stripMargin
      val compiledScript = Parser.parseScript(script)
      val expectedCompiled = """BlockStmt(List(MapStmt(items[].qty,MultiplyFn(GetFn(this),ConstantFn(3)))))"""
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,6,5), Item(xyz,3,7)),1)""".stripMargin + "\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      val a = dynalens[Shipment]
      for{
        (x,newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa",List(Item("abc",6,5), Item("xyz",3,7)),1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("filter must work") {
      val script =
        """
          |  items[].filter(this.qty > 4)
          |""".stripMargin
      val compiledScript = Parser.parseScript(script)
      val expectedCompiled = """BlockStmt(List(MapStmt(items[],FilterFn(GreaterThanFn(GetFn(this.qty),ConstantFn(4))))))"""
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,9,5)),1)""".stripMargin + "\n"
      val inst = Shipment("aaa",List(Item("abc",9,5), Item("xyz",1,7)),1)
      val a = dynalens[Shipment]
      for{
        (x,newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa",List(Item("abc",9,5)),1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("BiMap conversion must work") {

      val numbers = Map("abc"->"p123", "xyz"->"p456")
      val withRegistry = (new BiMapRegistry()).register("testmap", BiMap.fromMap(numbers))
      val inst = Shipment("aaa",List(Item("abc",9,5), Item("xyz",1,7)),1)
      val a = dynalens[Shipment]

      val scriptFwd =
        """
          |  items[].number = mapFwd("testmap")
          |""".stripMargin
      val compiledScriptFwd = Parser.parseScript(scriptFwd)
      val expectedCompiledFwd = """BlockStmt(List(MapStmt(items[].number,MapFwdFn(testmap))))"""
      val expectedResultFwd = """top -> Shipment(aaa,List(Item(p123,9,5), Item(p456,1,7)),1)""".stripMargin + "\n"

      val scriptRev =
        """
          |  items[].number = mapRev("testmap")
          |""".stripMargin
      val compiledScriptRev= Parser.parseScript(scriptRev)
      val expectedCompiledRev = """BlockStmt(List(MapStmt(items[].number,MapRevFn(testmap))))"""
      val expectedResultRev = """top -> Shipment(aaa,List(Item(abc,9,5), Item(xyz,1,7)),1)""".stripMargin + "\n"

      for{
        (f,newCtx) <- a.run(compiledScriptFwd, inst, withRegistry)
        resultStrFwd = toStringCtx(newCtx)
        (r,newnewCtx) <- a.run(compiledScriptRev, f, withRegistry)
        resultStrRev = toStringCtx(newnewCtx)
      } yield assertTrue(
        f == Shipment("aaa",List(Item("p123",9,5),Item("p456",1,7)),1) && resultStrFwd == expectedResultFwd && compiledScriptFwd.toString == expectedCompiledFwd &&
        r == inst && resultStrRev == expectedResultRev && compiledScriptRev.toString == expectedCompiledRev
      )
    },
  )
