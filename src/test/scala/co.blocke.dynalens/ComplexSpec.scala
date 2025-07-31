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

object ComplexSpec extends ZIOSpecDefault:

  val order = Order(
    "ORD-1",
    Pack(
      "P1",
      2,
      List(
        Shipment("S1", List(Item("A", 0), Item("B", 0))),
        Shipment("S2", List(Item("C", 0)))
      )
    )
  )

  val orderAssignr = dynalens[Order]

  def spec = suite("ComplexSpec")(
    test("BlockFn with val and map should propagate values") {
      val script = BlockStmt(
        List(
          ValStmt("v1", ConstantFn(42)),
          MapStmt("pack.shipments[].items[].qty", GetFn("v1"))
        )
      )
      for {
        ctx <- ZIO.succeed(DynaContext(order, Some(orderAssignr)))
        result <- script.resolve(ctx)
      } yield assertTrue(result.get("top").map(_._1) == Some(Order("ORD-1", Pack("P1", 2, List(Shipment("S1", List(Item("A", 42, 7), Item("B", 42, 7)), 2), Shipment("S2", List(Item("C", 42, 7)), 2))))))
    },
    test("Nested BlockFn with conditional logic") {
      val script = BlockStmt(
        List(
          ValStmt("v1", ConstantFn(15)),
          IfStmt(
            GreaterThanFn(GetFn("v1"), ConstantFn(5)),
            MapStmt("pack.shipments[].items[].qty", ConstantFn(99)),
            Some(MapStmt("pack.shipments[].items[].qty", ConstantFn(1)))
          )
        )
      )
      for {
        ctx <- ZIO.succeed(DynaContext(order, Some(orderAssignr)))
        result <- script.resolve(ctx)
      } yield assertTrue(result.get("top").map(_._1) == Some(Order("ORD-1", Pack("P1", 2, List(Shipment("S1", List(Item("A", 99, 7), Item("B", 99, 7)), 2), Shipment("S2", List(Item("C", 99, 7)), 2))))))
    },
    test("Failing GetFn should error") {
      for {
        result <- GetFn("notThere").resolve(DynaContext.empty).either
      } yield assertTrue(result.isLeft)
    }
  ).provide(BiMapRegistry.layer(EmptyBiMapRegistry))
