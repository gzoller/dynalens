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

object Basic extends ZIOSpecDefault:

  def spec = suite("Basic Functional Tests")(
    test("Simple assignment and retrieval must work") {
      val inst = Person("Mike", 34)
      val ar = dynalens[Person]

      for {
        i2 <- ar.update("name", "Bob", inst)
        i3 <- ar.update("age", 52, i2)
        z <- ar.get("age", i3)
      } yield assertTrue(
        i2 == Person("Bob", 34),
        i3 == Person("Bob", 52),
        z == 52
      )
    },
    test("Nested get access should work") {
      val inst = Company("IBM", Department(123, Person("Mike", 62)))
      val ar = dynalens[Company]
      for {
        v <- ar.get("dept.director.name", inst)
      } yield assertTrue(v == "Mike")
    },
    test("Nested get access to individual indexed field works") {
      val inst = Shipment("abc", List(Item("a", 5), Item("b", 2), Item("c", 100)))
      val ar = dynalens[Shipment]
      for {
        v <- ar.get("items[1].qty", inst)
      } yield assertTrue(v == 2)
    },
    test("Nested update should work") {
      val inst = Company("IBM", Department(123, Person("Mike", 62)))
      val ar = dynalens[Company]
      for {
        v <- ar.update("dept.director.name", "Greg", inst)
      } yield assertTrue(v == Company("IBM", Department(123, Person("Greg", 62))))
    },
    test("Nested update to individual indexed field works") {
      val inst = Shipment("abc", List(Item("a", 5), Item("b", 2), Item("c", 100)))
      val ar = dynalens[Shipment]
      for {
        v <- ar.update("items[1].qty", 1, inst)
      } yield assertTrue(v == Shipment("abc", List(Item("a", 5), Item("b", 1), Item("c", 100))))
    },
    test("Map must work") {
      val inst = Shipment("abc", List(Item("a", 5), Item("b", 2), Item("c", 100)))
      val ar = dynalens[Shipment]
      for {
        v <- ar.map("items[].qty", ConstantFn(3), inst)
      } yield assertTrue(v == Shipment("abc", List(Item("a", 3), Item("b", 3), Item("c", 3))))
    },
    test("Map of nested must work") {
      val inst =
        Order(
          "ord1",
          Pack(
            "palletA",
            4,
            List(
              Shipment("ship1", List(Item("a", 5), Item("b", 2), Item("c", 100))),
              Shipment("ship2", List(Item("x", 12), Item("y", 99), Item("z", 54)))
            )
          )
        )
      val ar = dynalens[Order]
      for {
        v <- ar.map("pack.shipments[].items[].qty", ConstantFn(1), inst)
      } yield assertTrue(
        v ==
          Order(
            "ord1",
            Pack("palletA", 4, List(Shipment("ship1", List(Item("a", 1), Item("b", 1), Item("c", 1))), Shipment("ship2", List(Item("x", 1), Item("y", 1), Item("z", 1)))))
          )
      )
    },
    test("Map of nested with specific index must work") {
      val inst =
        Order(
          "ord1",
          Pack(
            "palletA",
            4,
            List(
              Shipment("ship1", List(Item("a", 5), Item("b", 2), Item("c", 100))),
              Shipment("ship2", List(Item("x", 12), Item("y", 99), Item("z", 54)))
            )
          )
        )
      val ar = dynalens[Order]
      for {
        v <- ar.map("pack.shipments[1].items[].qty", ConstantFn(1), inst)
      } yield assertTrue(
        v ==
          Order(
            "ord1",
            Pack("palletA", 4, List(Shipment("ship1", List(Item("a", 5), Item("b", 2), Item("c", 100))), Shipment("ship2", List(Item("x", 1), Item("y", 1), Item("z", 1)))))
          )
      )
    },
    test("Map with arithmetic function must work") {
      val inst = Shipment("abc", List(Item("a", 5), Item("b", 2), Item("c", 100)))
      val ar = dynalens[Shipment]
      for {
        v <- ar.map("items[].qty", MultiplyFn(ConstantFn(3), GetFn("this")), inst)
      } yield assertTrue(v == Shipment("abc", List(Item("a", 15), Item("b", 6), Item("c", 300))))
    },
    test("Map with Get function must work") {
      val inst = Shipment("abc", List(Item("a", 5), Item("b", 2), Item("c", 100)))
      val ar = dynalens[Shipment]
      for {
        v <- ar.map("items[].qty", GetFn("num"), inst)
      } yield assertTrue(v == Shipment("abc", List(Item("a", 2), Item("b", 2), Item("c", 2))))
    },
    test("Map with Get function (2) must work") {
      val inst = Shipment("abc", List(Item("a", 5), Item("b", 2), Item("c", 100)))
      val ar = dynalens[Shipment]
      for {
        v <- ar.map("items[].qty", GetFn("items[].num"), inst)
      } yield assertTrue(v == Shipment("abc", List(Item("a", 7), Item("b", 7), Item("c", 7))))
    }
  ).provide(BiMapRegistry.layer(EmptyBiMapRegistry))
