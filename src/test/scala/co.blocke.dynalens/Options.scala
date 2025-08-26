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

import zio.*
import zio.test.*

import DynaLens.*
import parser.Script

import CtxStrings.*

object Options extends ZIOSpecDefault:

  def spec = suite("Options Tests")(
    test("Simple option assignment") {
      val script =
        """
          |  dunno? = "foom"
          |  interest? = None
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(UpdateStmt(dunno?,ConstantFn(foom)), UpdateStmt(interest[]?,NoneFn())))"""
      val expectedResult =
        """top -> Maybe(abc,Some(foom),None)
          |""".stripMargin
      val inst = Maybe("abc", None, Some(List(Item("abc", 2, 5))))
      val a = dynalens[Maybe]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Maybe("abc", Some("foom")) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Expression-based option assignment") {
      val script =
        """
          |  val x = "yay"
          |  val y = None
          |  dunno = x
          |  interest = y
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,ConstantFn(yay)), ValStmt(y,NoneFn()), UpdateStmt(dunno?,GetFn(x)), UpdateStmt(interest[]?,GetFn(y))))"""
      val expectedResult =
        """top -> Maybe(abc,Some(yay),None)
          |x -> yay
          |y -> None
          |""".stripMargin
      val inst = Maybe("abc", None, Some(List(Item("abc", 2, 5))))
      val a = dynalens[Maybe]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Maybe("abc", Some("yay")) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Get option value (with isDefined)") {
      val script =
        """
          |  val x = dunno?.else("unknown")
          |  val y = x.toUpperCase() :: " ok"
          |  val q = dunno?.isDefined()
          |  val r = interest[]?.isDefined()
          |  val s = None.isDefined()
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,ElseFn(GetFn(dunno?),ConstantFn(unknown))), ValStmt(y,ConcatFn(List(ToUpperFn(GetFn(x)), ConstantFn( ok)))), ValStmt(q,IsDefinedFn(GetFn(dunno?))), ValStmt(r,IsDefinedFn(GetFn(interest[]?))), ValStmt(s,IsDefinedFn(NoneFn()))))"""
      val expectedResult =
        """top -> Maybe(abc,Some(wow),None)
          |q -> true
          |r -> false
          |s -> false
          |x -> wow
          |y -> WOW ok
          |""".stripMargin
      val inst = Maybe("abc", Some("wow"))
      val a = dynalens[Maybe]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Maybe("abc", Some("wow")) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Get option value--List (with isDefined)") {
      val script =
        """
          |  val x = interest[]?.isDefined()
          |  val y = interest[]?.len()
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,IsDefinedFn(GetFn(interest[]?))), ValStmt(y,LengthFn(GetFn(interest[]?)))))"""
      val expectedResult =
        """top -> Maybe(abc,Some(wow),Some(List(Item(abc,2,5))))
          |x -> true
          |y -> 1
          |""".stripMargin
      val inst = Maybe("abc", Some("wow"), Some(List(Item("abc", 2, 5))))
      val a = dynalens[Maybe]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Maybe("abc", Some("wow"), Some(List(Item("abc", 2, 5)))) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Update and Map with optional list") {
      val script =
        """
          |  val x = interest.len()
          |  interest.qty => x * 5
          |  interest.sortDesc(number)
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,LengthFn(GetFn(interest[]?))), MapStmt(interest[]?.qty,MultiplyFn(GetFn(x),ConstantFn(5))), MapStmt(interest[]?,SortFn(IdentityFn,Some(this.number),false))))"""
      val expectedResult =
        """top -> Maybe(abc,Some(wow),Some(List(Item(xyz,10,7), Item(abc,10,5))))
          |x -> 2
          |""".stripMargin
      val inst = Maybe("abc", Some("wow"), Some(List(Item("abc", 2, 5), Item("xyz", 1, 7))))
      val a = dynalens[Maybe]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Maybe("abc", Some("wow"), Some(List(Item("xyz", 10, 7), Item("abc", 10, 5)))) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Update optional list 1") {
      val script =
        """
          |  l2 = l1
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(UpdateStmt(l2[]?,GetFn(l1[]))))"""
      val expectedResult =
        """top -> MyLists(1,List(1, 2, 3),Some(List(1, 2, 3)))
          |""".stripMargin
      val inst = MyLists(1, List(1,2,3), None)
      val a = dynalens[MyLists]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == MyLists(1,List(1, 2, 3),Some(List(1, 2, 3))) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Update optional list 2") {
      val script =
        """
          |  val x = l2
          |  l2 = None
          |  val y = l2
          |  val z = y.isDefined()
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,GetFn(l2[]?)), UpdateStmt(l2[]?,NoneFn()), ValStmt(y,GetFn(l2[]?)), ValStmt(z,IsDefinedFn(GetFn(y)))))"""
      val expectedResult =
        """top -> MyLists(1,List(1, 2, 3),None)
          |x -> List(1, 2, 3)
          |y -> List()
          |z -> false
          |""".stripMargin
      val inst = MyLists(1, List(1,2,3), Some(List(1,2,3)))
      val a = dynalens[MyLists]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == MyLists(1,List(1, 2, 3),None) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Update optional list 3") {
      val script =
        """
          |  val x = l2
          |  l2 = x
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,GetFn(l2[]?)), UpdateStmt(l2[]?,GetFn(x))))"""
      val expectedResult =
        """top -> MyLists(1,List(1, 2, 3),None)
          |x -> List()
          |""".stripMargin
      val inst = MyLists(1, List(1,2,3), None)
      val a = dynalens[MyLists]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == MyLists(1,List(1, 2, 3),None) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Indexed assignment with Some and None") {
      val script =
        """
          |  l2[1] = 15
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(UpdateStmt(l2[1]?,ConstantFn(15))))"""
      val expectedResult =
        """top -> MyLists(1,List(1, 2, 3),Some(List(5, 15, 7)))
          |""".stripMargin
      val inst = MyLists(1, List(1,2,3), Some(List(5,6,7)))
      val a = dynalens[MyLists]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == MyLists(1,List(1, 2, 3),Some(List(5,15,7))) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Indexed assignment with List of Option") {
      val script =
        """
          |  l1.clean()
          |  l1[1] = 15
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(MapStmt(l1[],CleanFn(IdentityFn)), UpdateStmt(l1[1],ConstantFn(15))))"""
      val expectedResult =
        """top -> ListOfOpt(1,List(Some(1), Some(15)))
          |""".stripMargin
      val inst = ListOfOpt(1, List(Some(1),None,Some(3)))
      val a = dynalens[ListOfOpt]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == ListOfOpt(1, List(Some(1),Some(15))) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Map vs Update (update)") {
      val script =
        """
          |  l2 = None
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(UpdateStmt(l2[]?,NoneFn())))"""
      val expectedResult =
        """top -> MyLists(1,List(1, 2, 3),None)
          |""".stripMargin
      val inst = MyLists(1, List(1,2,3), Some(List(4,5,6)))
      val a = dynalens[MyLists]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == MyLists(1, List(1,2,3), None) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Map vs Update (map) - 1") {
      val script =
        """
          |  dunno => "blah" :: this
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(MapStmt(dunno?,ConcatFn(List(ConstantFn(blah), GetFn(this))))))"""
      val expectedResult =
        """top -> Maybe(abc,Some(blahfoo),None)
          |""".stripMargin
      val inst = Maybe("abc", Some("foo"), None)
      val a = dynalens[Maybe]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Maybe("abc",Some("blahfoo"),None) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Map vs Update (map) - 2") {
      val script =
        """
          |  l2 => this + 9
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(MapStmt(l2[]?,LoopFn(AddFn(GetFn(this),ConstantFn(9))))))"""
      val expectedResult =
        """top -> MyLists(1,List(1, 2, 3),Some(List(13, 14, 15)))
          |""".stripMargin
      val inst = MyLists(1, List(1,2,3), Some(List(4,5,6)))
      val a = dynalens[MyLists]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == MyLists(1, List(1,2,3), Some(List(13,14,15))) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Map against None") {
      val script =
        """
          |  interest.number => "blah"
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(MapStmt(interest[]?.number,ConstantFn(blah))))"""
      val expectedResult =
        """top -> Maybe(abc,None,None)
          |""".stripMargin
      val inst = Maybe("abc", None, None)
      val a = dynalens[Maybe]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == inst && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Assignment top optional list (non-empty)") {
      val script =
        """
          |  l2[2] = 99
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(UpdateStmt(l2[2]?,ConstantFn(99))))"""
      val expectedResult =
        """top -> MyLists(1,List(1, 2, 3),Some(List(4, 5, 99)))
          |""".stripMargin
      val inst = MyLists(1, List(1,2,3), Some(List(4,5,6)))
      val a = dynalens[MyLists]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x ==  MyLists(1,List(1, 2, 3),Some(List(4, 5, 99))) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
  )

//  _ <- ZIO.succeed(println("&&& " + compiledScript))
//  _ <- ZIO.succeed(println("!!! " + x))
//  _ <- ZIO.succeed(println("??? " + resultStr))
