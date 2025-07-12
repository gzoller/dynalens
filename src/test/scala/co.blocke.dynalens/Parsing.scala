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
  )