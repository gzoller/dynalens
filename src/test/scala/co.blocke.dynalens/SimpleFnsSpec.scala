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

object SimpleFnsSpec extends ZIOSpecDefault:

  val testCtx: Map[String, (Any, DynaLens[?])] = Map(
    "top" -> ("hello", null),
    "s1" -> ("world", null),
    "num" -> (5, null),
    "x" -> (10, null),
    "y" -> (3, null)
  )

  def spec = suite("SimpleFnsSpec")(
    test("ConstantFn should return constant value") {
      for {
        result <- ConstantFn("foo").resolve(testCtx)
      } yield assertTrue(result == "foo")
    },
    test("GetFn should return context value") {
      for {
        result <- GetFn("s1").resolve(testCtx)
      } yield assertTrue(result == "world")
    },
    test("AddFn should sum two numbers") {
      val fn = AddFn(GetFn("x"), GetFn("y"))
      for {
        result <- fn.resolve(testCtx)
      } yield assertTrue(result == 13)
    },
    test("SubFn should subtract two numbers") {
      val fn = SubtractFn(GetFn("x"), GetFn("y"))
      for {
        result <- fn.resolve(testCtx)
      } yield assertTrue(result == 7)
    },
    test("ConcatFn should concatenate strings") {
      val fn = ConcatFn(List(GetFn("top"), GetFn("s1")))
      for {
        result <- fn.resolve(testCtx)
      } yield assertTrue(result == "helloworld")
    },
    test("ToUpperFn should convert string to upper case") {
      val fn = ToUpperFn(GetFn("s1"))
      for {
        result <- fn.resolve(testCtx)
      } yield assertTrue(result == "WORLD")
    },
    test("SubstringFn should extract substring") {
      val fn = SubstringFn(GetFn("top"), ConstantFn(1), Some(ConstantFn(4)))
      for {
        result <- fn.resolve(testCtx)
      } yield assertTrue(result == "ell")
    },
    test("Bad cast in Fn should fail") {
      val badCtx = testCtx.updated("x", ("oops", null))
      val fn = AddFn(GetFn("x"), ConstantFn(5))
      for {
        result <- fn.resolve(badCtx).either
      } yield assertTrue(result.isLeft)
    }
  ).provide(BiMapRegistry.layer(EmptyBiMapRegistry))
