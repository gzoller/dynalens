package co.blocke.dynalens

import zio._
import zio.test._
import zio.test.Assertion._

import DynaLens.*

object ComplexSpec extends ZIOSpecDefault:

  val order = Order("ORD-1",
    Pack("P1", 2, List(
      Shipment("S1", List(Item("A", 0), Item("B", 0))),
      Shipment("S2", List(Item("C", 0)))
    )
  ))

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
        ctx <- ZIO.succeed(Map("top" -> (order, orderAssignr)))
        result <- script.resolve(ctx)
      } yield assertTrue(result.get("top").map(_._1) == Some(Order("ORD-1",Pack("P1",2,List(Shipment("S1",List(Item("A",42,7), Item("B",42,7)),2), Shipment("S2",List(Item("C",42,7)),2))))))
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
        ctx <- ZIO.succeed(Map("top" -> (order, orderAssignr)))
        result <- script.resolve(ctx)
      } yield assertTrue(result.get("top").map(_._1) == Some(Order("ORD-1",Pack("P1",2,List(Shipment("S1",List(Item("A",99,7), Item("B",99,7)),2), Shipment("S2",List(Item("C",99,7)),2))))))
    },
    test("Failing GetFn should error") {
      for {
        result <- GetFn("notThere").resolve(Map.empty).either
      } yield assertTrue(result.isLeft)
    }
  ).provide(BiMapRegistry.layer(EmptyBiMapRegistry))