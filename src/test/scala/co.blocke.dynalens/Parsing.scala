package co.blocke.dynalens

import zio._
import zio.test._
import zio.test.Assertion._

import DynaLens.*
import parser.Parser

/*
case class Item(number: String, qty: Int, num: Int = 7)
case class Shipment(id: String, items:List[Item], num: Int = 2)
case class Pack(label: String, caseSize: Int, shipments: List[Shipment])
case class Order(id: String, pack: Pack)
*/

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
      val a = dynalens[Item]
      val expectedResult = """this -> Item(abc,2,5)
                             |x -> 42
                             |y -> 50""".stripMargin + "\n"
      val inst = Item("abc",2,5)
      for{
        (x,newCtx) <- Parser.run(compiledScript, inst)
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
      val a = dynalens[Shipment]
      val expectedResult = """this -> Shipment(aaa,List(Item(abc,2,5), Item(xyz,1,7)),1)
                             |x -> 17
                             |y -> 20""".stripMargin + "\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      for{
        (x,newCtx) <- Parser.run(compiledScript, inst)
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
      val a = dynalens[Shipment]
      val expectedResult = "this -> Shipment(aaa,List(Item(abc,2,33), Item(xyz,1,33)),1)\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      for{
        (x,newCtx) <- Parser.run(compiledScript, inst)
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
      val a = dynalens[Shipment]
      val expectedResult = "this -> Shipment(aaa,List(Item(abc,2,1), Item(xyz,1,7)),1)\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      for{
        (x,newCtx) <- Parser.run(compiledScript, inst)
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
      val a = dynalens[Shipment]
      val expectedResult = "this -> Shipment(aaa,List(Item(abc,2,1), Item(xyz,1,7)),1)\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      for{
        (x,newCtx) <- Parser.run(compiledScript, inst)
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
      val a = dynalens[Shipment]
      val expectedResult = "this -> Shipment(aaa,List(Item(abc,2,1), Item(xyz,1,7)),1)\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      for{
        (x,newCtx) <- Parser.run(compiledScript, inst)
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
      val a = dynalens[Shipment]
      val expectedResult = "this -> Shipment(aaa,List(Item(abc,2,12), Item(xyz,1,7)),1)\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      for{
        (x,newCtx) <- Parser.run(compiledScript, inst)
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
      val a = dynalens[Shipment]
      val expectedResult = """this -> Shipment(aaa,List(Item(abc,2,12), Item(xyz,1,-1)),1)
                             |z -> -1""".stripMargin + "\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      for{
        (x,newCtx) <- Parser.run(compiledScript, inst)
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
      val a = dynalens[Shipment]
      val expectedResult = """this -> Shipment(aaa,List(Item(abc,4,5), Item(xyz,2,7)),1)
                             |z -> 2""".stripMargin + "\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      for{
        (x,newCtx) <- Parser.run(compiledScript, inst)
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
      val a = dynalens[Shipment]
      val expectedResult = """this -> Shipment(aaa,List(Item(abc,10,5), Item(xyz,0,7)),1)""" + "\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      for{
        (x,newCtx) <- Parser.run(compiledScript, inst)
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
      val a = dynalens[Shipment]
      val expectedResult = """this -> Shipment(aaa,List(Item(abc,2,5), Item(xyz,1,7)),7)
                             |x -> 7
                             |y -> true
                             |z -> false""".stripMargin + "\n"
      val inst = Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),1)
      for{
        (x,newCtx) <- Parser.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa",List(Item("abc",2,5), Item("xyz",1,7)),7) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
  )
