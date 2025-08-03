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
import parser.Parser

object Options extends ZIOSpecDefault:

  def spec = suite("Options Tests")(
    test("Simple option assignment") {
      val script =
        """
          |  dunno = "foom"
          |  interest = None
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(UpdateStmt(dunno,ConstantFn(foom)), UpdateStmt(interest,ConstantFn(None))))"""
      val expectedResult =
        """top -> Maybe(abc,Some(foom),None)
          |""".stripMargin
      val inst = Maybe("abc", None, Some(List(Item("abc", 2, 5))))
      val a = dynalens[Maybe]
      for {
        compiledScript <- Parser.parseScript(script)
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
          |  interest[] = y.else(None)
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,ConstantFn(yay)), ValStmt(y,ConstantFn(None)), UpdateStmt(dunno,GetFn(x,false,None,false,true)), MapStmt(interest[],GetFn(y,false,Some(ConstantFn(None)),false,false))))"""
      val expectedResult =
        """top -> Maybe(abc,Some(yay),None)
          |x -> yay
          |y -> None
          |""".stripMargin
      val inst = Maybe("abc", None, Some(List(Item("abc", 2, 5))))
      val a = dynalens[Maybe]
      for {
        compiledScript <- Parser.parseScript(script)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Maybe("abc", Some("yay")) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Get option value (with isDefined)") {
      val script =
        """
          |  val x = dunno.else("unknown")
          |  val y = x.toUpperCase() :: " ok"
          |  val q = dunno.isDefined()
          |  val r = interest.isDefined()
          |  val s = None.isDefined()
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,GetFn(dunno,false,Some(ConstantFn(unknown)),false,false)), ValStmt(y,ConcatFn(List(ToUpperFn(GetFn(x,false,None,false,false)), ConstantFn( ok)))), ValStmt(q,GetFn(dunno,false,None,true,false)), ValStmt(r,GetFn(interest,false,None,true,false)), ValStmt(s,IsDefinedFn(ConstantFn(None)))))"""
      val expectedResult =
        """q -> true
          |r -> false
          |s -> false
          |top -> Maybe(abc,Some(wow),None)
          |x -> wow
          |y -> WOW ok
          |""".stripMargin
      val inst = Maybe("abc", Some("wow"))
      val a = dynalens[Maybe]
      for {
        compiledScript <- Parser.parseScript(script)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Maybe("abc", Some("wow")) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Get option value--List (with isDefined)") {
      val script =
        """
          |  val x = interest[].isDefined()
          |  val y = interest[].len()
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,GetFn(interest[],false,None,true,false)), ValStmt(y,LengthFn(GetFn(interest[],false,None,false,false)))))"""
      val expectedResult =
        """top -> Maybe(abc,Some(wow),Some(List(Item(abc,2,5))))
          |x -> List(Item(abc,2,5))
          |y -> 1
          |""".stripMargin
      val inst = Maybe("abc", Some("wow"), Some(List(Item("abc", 2, 5))))
      val a = dynalens[Maybe]
      for {
        compiledScript <- Parser.parseScript(script)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Maybe("abc", Some("wow"), Some(List(Item("abc", 2, 5)))) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Update and Map with optional list") {
      val script =
        """
          |  val x = interest[].len()
          |  interest[].qty = x * 5
          |  interest[].sortDesc(number)
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,LengthFn(GetFn(interest[],false,None,false,false))), MapStmt(interest[].qty,MultiplyFn(GetFn(x,false,None,false,false),ConstantFn(5))), MapStmt(interest[],SortFn(Some(number),false))))"""
      val expectedResult =
        """top -> Maybe(abc,Some(wow),Some(List(Item(xyz,10,7), Item(abc,10,5))))
          |x -> 2
          |""".stripMargin
      val inst = Maybe("abc", Some("wow"), Some(List(Item("abc", 2, 5), Item("xyz", 1, 7))))
      val a = dynalens[Maybe]
      for {
        compiledScript <- Parser.parseScript(script)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Maybe("abc", Some("wow"), Some(List(Item("xyz", 10, 7), Item("abc", 10, 5)))) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    }
  )

//  _ <- ZIO.succeed(println("&&& " + compiledScript))
//  _ <- ZIO.succeed(println("!!! " + x))
//  _ <- ZIO.succeed(println("??? " + resultStr))
