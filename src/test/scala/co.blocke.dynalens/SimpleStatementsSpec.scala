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

object SimpleStatementsSpec extends ZIOSpecDefault:

  val rootAssignr = dynalens[Item]

  val sampleItem = Item("ABC", 10)

  def spec = suite("SimpleStatementsSpec")(
    test("ValStmt should add a symbol to context") {
      val stmt = ValStmt("v1", ConstantFn(123))
      for {
        newCtx <- stmt.resolve(Map.empty)
      } yield assertTrue(newCtx("v1")._1 == 123)
    },
    test("MapStmt should update object field") {
      val stmt = MapStmt("qty", ConstantFn(5))
      for {
        ctx <- ZIO.succeed(Map("top" -> (sampleItem, rootAssignr)))
        newCtx <- stmt.resolve(ctx)
      } yield assertTrue(newCtx("top")._1.asInstanceOf[Item].qty == 5)
    },
    test("IfStmt true branch executes") {
      val stmt = IfStmt(
        GreaterThanFn(ConstantFn(10), ConstantFn(5)),
        BlockStmt(List(UpdateStmt("qty", ConstantFn(7)))),
        Some(BlockStmt(List(UpdateStmt("qty", ConstantFn(3)))))
      )
      for {
        ctx <- ZIO.succeed(Map("top" -> (sampleItem, rootAssignr)))
        newCtx <- stmt.resolve(ctx)
      } yield assertTrue(newCtx("top")._1.asInstanceOf[Item].qty == 7)
    },
    test("IfStmt false branch executes") {
      val stmt = IfStmt(
        GreaterThanFn(ConstantFn(1), ConstantFn(5)),
        BlockStmt(List(UpdateStmt("qty", ConstantFn(7)))),
        Some(BlockStmt(List(UpdateStmt("qty", ConstantFn(3)))))
      )
      for {
        ctx <- ZIO.succeed(Map("top" -> (sampleItem, rootAssignr)))
        newCtx <- stmt.resolve(ctx)
      } yield assertTrue(newCtx("top")._1.asInstanceOf[Item].qty == 3)
    }
  ).provide(BiMapRegistry.layer(EmptyBiMapRegistry))
