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

import co.blocke.dynalens.CtxStrings.toStringCtx
import co.blocke.dynalens.DynaLens.dynalens
import co.blocke.dynalens.parser.Script
import co.blocke.dynalens.*
import zio.*
import zio.test.*

object ConcatAndCons extends ZIOSpecDefault:

  def spec = suite("Concat and Cons Tests")(
    test("string + string concatenation") {
      val script =
        """
          |  val x = s1 + s2
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,ConcatFn(List(GetFn(s1,false,None), GetFn(s2,false,None))))))"""
      val expectedResult =
        """top -> SampleStrings(foo,bar,List(a, b),Some(List(c, d)),3,5)
          |x -> foobar""".stripMargin + "\n"

      val inst = SampleStrings("foo","bar",List("a","b"),Some(List("c","d")),3,5)
      val a = dynalens[SampleStrings]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },

    test("string + list[string] concatenation") {
      val script =
        """
          |  val x = s1 + ls
          |  val y = ls + s1
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,ConcatFn(List(GetFn(s1,false,None), GetFn(ls,false,None)))), ValStmt(y,ConcatFn(List(GetFn(ls,false,None), GetFn(s1,false,None))))))"""
      val expectedResult =
        """top -> SampleStrings(foo,bar,List(a, b),Some(List(c, d)),3,5)
          |x -> fooab
          |y -> abfoo""".stripMargin + "\n"

      val inst = SampleStrings("foo","bar",List("a","b"),Some(List("c","d")),3,5)
      val a = dynalens[SampleStrings]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },

    test("string + option[list[string]] concatenation") {
      val script =
        """
          |  val x = s1 + optLs
          |  val y = optLs + s1
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,ConcatFn(List(GetFn(s1,false,None), GetFn(optLs,true,None)))), ValStmt(y,ConcatFn(List(GetFn(optLs,true,None), GetFn(s1,false,None))))))"""
      val expectedResult =
        """top -> SampleStrings(foo,bar,List(a, b),Some(List(c, d)),3,5)
          |x -> foocd
          |y -> cdfoo""".stripMargin + "\n"

      val inst = SampleStrings("foo","bar",List("a","b"),Some(List("c","d")),3,5)
      val a = dynalens[SampleStrings]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },

    test("string + option[list[string]] concatenation (with None)") {
      val script =
        """
          |  val x = s1 + optLs
          |  val y = optLs + s1
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,ConcatFn(List(GetFn(s1,false,None), GetFn(optLs,true,None)))), ValStmt(y,ConcatFn(List(GetFn(optLs,true,None), GetFn(s1,false,None))))))"""
      val expectedResult =
        """top -> SampleStrings(foo,bar,List(a, b),None,3,5)
          |x -> foo
          |y -> foo""".stripMargin + "\n"

      val inst = SampleStrings("foo","bar",List("a","b"),None,3,5)
      val a = dynalens[SampleStrings]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },

    test("numeric + still works") {
      val script =
        """
          |  val x = n1 + n2
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,AddFn(GetFn(n1,false,None),GetFn(n2,false,None)))))"""
      val expectedResult =
        """top -> SampleStrings(foo,bar,List(a, b),Some(List(c, d)),3,5)
          |x -> 8""".stripMargin + "\n"

      val inst = SampleStrings("foo","bar",List("a","b"),Some(List("c","d")),3,5)
      val a = dynalens[SampleStrings]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },

    test("simple cons using ::") {
      val script =
        """
          |  val x = "foo" :: "bar" :: Nil
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,ConsFn(ConstantFn(foo),ConsFn(ConstantFn(bar),ConstantFn(List()))))))"""
      val expectedResult =
        """top -> SampleStrings(foo,bar,List(a, b),Some(List(c, d)),3,5)
          |x -> List(foo, bar)""".stripMargin + "\n"

      val inst = SampleStrings("foo","bar",List("a","b"),Some(List("c","d")),3,5)
      val a = dynalens[SampleStrings]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },

    test("cons onto existing list") {
      val script =
        """
          |  val x = s1 :: ls
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,ConsFn(GetFn(s1,false,None),GetFn(ls,false,None)))))"""
      val expectedResult =
        """top -> SampleStrings(foo,bar,List(a, b),Some(List(c, d)),3,5)
          |x -> List(foo, a, b)""".stripMargin + "\n"

      val inst = SampleStrings("foo","bar",List("a","b"),Some(List("c","d")),3,5)
      val a = dynalens[SampleStrings]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    }
  )