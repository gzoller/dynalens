package co.blocke.dynalens

import zio.*
import zio.test.*
import co.blocke.testkit.ZioTestKit._

object LensUpdateSpec extends ZIOSpecDefault:
  def spec = suite("DynaLens Update Tests")(
    test("scalar update") {
      val item = Item("A", 10)
      val lens = DynaLens.into[Item].topLens // assume root/lens already generated
      val path = Path.parsePath("qty")
      for {
        updated <- lens.update(path, 99, item)
      } yield assertTrue(updated == Item("A", 99))
    },
    test("list element update") {
      val order = Order("O1", List(Item("A",1), Item("B",2)))
      val lens = DynaLens.into[Order].topLens
      val path = Path.parsePath("items[1].qty")
      for {
        updated <- lens.update(path, 100, order)
      } yield assertTrue(updated ==
        Order("O1", List(Item("A",1), Item("B",100))))
    },
    test("map value update") {
      val mh = MapHolder(Map("x" -> 1, "y" -> 2))
      val lens = DynaLens.into[MapHolder].topLens
      val path = Path.parsePath("things[x]")
      for {
        updated <- lens.update(path, 9, mh)
      } yield assertTrue(updated ==
        MapHolder(Map("x" -> 9, "y" -> 2)))
    },
    test("optional list element update") {
      val holder = OptionListHolder(Some(List("A", "B", "C")))
      val lens = DynaLens.into[OptionListHolder].topLens
      val path = Path.parsePath("itemsOpt[1]")
      for {
        updated <- lens.update(path, "Z", holder)
      } yield assertTrue(updated == OptionListHolder(Some(List("A", "Z", "C"))))
    },
    test("optional scalar update to Some") {
      val holder = OptScalarHolder(Some(10))
      val lens = DynaLens.into[OptScalarHolder].topLens
      val path = Path.parsePath("opt")
      for {
        updated <- lens.update(path, 42, holder)
        path2 = Path.parsePath("opt")
        updated2 <- lens.update(path2, Some(5), holder)
      } yield assertTrue(updated == OptScalarHolder(Some(42))) &&
        assertTrue(updated2 == OptScalarHolder(Some(5)))
    },
    test("optional scalar update from None to Some") {
      val holder = OptScalarHolder(None)
      val lens = DynaLens.into[OptScalarHolder].topLens
      val path = Path.parsePath("opt")
      for {
        updated <- lens.update(path, 7, holder)
      } yield assertTrue(updated == OptScalarHolder(Some(7)))
    },
    test("optional map entry update") {
      val holder = OptionMapHolder(Some(Map("x" -> 1, "y" -> 2)))
      val lens = DynaLens.into[OptionMapHolder].topLens
      val path = Path.parsePath("mapOpt[x]")
      for {
        updated <- lens.update(path, 99, holder)
      } yield assertTrue(updated == OptionMapHolder(Some(Map("x" -> 99, "y" -> 2))))
    },
    test("deep nested update") {
      val deep = DeepRoot(DeepMapHolder(
        List(Map("d1" -> DeepItem("desc1", 1.0)))
      ))
      val lens = DynaLens.into[DeepRoot].topLens
      val path = Path.parsePath("holder.items[0][d1].price")
      for {
        updated <- lens.update(path, 9.99, deep)
      } yield assertTrue(
        updated ==
          DeepRoot(
            DeepMapHolder(
              List(Map("d1" -> DeepItem("desc1", 9.99)))
            )
          )
      )
    },
    test("non-optional scalar update with Option should fail") {
      val item = Item("A", 10)
      val lens = DynaLens.into[Item].topLens
      val path = Path.parsePath("qty")
      for {
        result <- lens.update(path, Some(5), item).either
      } yield assertTrue(result.isLeft)
    },
    test("map update with missing key inserts new entry") {
      val lens = DynaLens.into[MapHolder].topLens
      for {
        result <- lens.update(Path.parsePath("things[z]"), 2, MapHolder(Map("x" -> 1))).either
      } yield assertTrue(result == Right(MapHolder(Map("x" -> 1, "z" -> 2))))
    },
    test("list update with out of bounds index should fail") {
      val order = Order("O1", List(Item("A",1)))
      val lens = DynaLens.into[Order].topLens
      val path = Path.parsePath("items[5].qty")
      for {
        result <- lens.update(path, 10, order).either
      } yield assertTrue(result.isLeft)
    },
    test("list update with non-numeric index should fail") {
      val order = Order("O1", List(Item("A",1)))
      val lens = DynaLens.into[Order].topLens
      val path = Path.parsePath("items[abc].qty")
      for {
        result <- lens.update(path, 10, order).either
      } yield assertTrue(result.isLeft)
    },
    test("update scalar deeper path should fail") {
      val item = Item("A",10)
      val lens = DynaLens.into[Item].topLens
      val path = Path.parsePath("qty.foo")
      for {
        result <- lens.update(path, 100, item).either
      } yield assertTrue(result.isLeft)
    },
    test("map update with wrong key type should fail") {
      val mh2 = MapHolder2(Map(1 -> "one", 2 -> "two"))
      val lens = DynaLens.into[MapHolder2].topLens
      val path = Path.parsePath("things[x]")
      for {
        result <- lens.update(path, "foo", mh2).either
      } yield assertTrue(result.isLeft)
    },
    test("deep list nested update") {
      val order = Order("O1", List(
        Item("soap", 3),
        Item("pen", 2),
        Item("book", 1)
      ))
      val lens = DynaLens.into[Order].topLens
      val path = Path.parsePath("items[2].name")
      for {
        updated <- lens.update(path, "magazine", order)
      } yield assertTrue(
        updated == Order("O1",
          List(
            Item("soap", 3),
            Item("pen", 2),
            Item("magazine", 1)
          )
        )
      )
    },
    test("deep map element update") {
      val deep = DeepRoot(
        DeepMapHolder(List(
          Map(
            "d1" -> DeepItem("old", 1.0),
            "d2" -> DeepItem("other", 2.0)
          )
        ))
      )
      val lens = DynaLens.into[DeepRoot].topLens
      val path = Path.parsePath("holder.items[0][d1].price")
      for {
        updated <- lens.update(path, 4.44, deep)
      } yield assertTrue(
        updated == DeepRoot(
          DeepMapHolder(List(
            Map(
              "d1" -> DeepItem("old", 4.44),
              "d2" -> DeepItem("other", 2.0)
            )
          ))
        )
      )
    },
    test("enum scalar update") {
      val item = ColorItem(ColorEnum.Red)
      val lens = DynaLens.into[ColorItem].topLens
      val path = Path.parsePath("color")
      for {
        updated <- lens.update(path, "Blue", item)
      } yield assertTrue(updated == ColorItem(ColorEnum.Blue))
    },
    test("enum map value update") {
      val mh = ColorMapHolder(Map("Red" -> ColorEnum.Green))
      val lens = DynaLens.into[ColorMapHolder].topLens
      val path = Path.parsePath("things[Red]")
      for {
        updated <- lens.update(path, "Blue", mh)
      } yield assertTrue(
        updated == ColorMapHolder(
          Map("Red" -> ColorEnum.Blue)
        )
      )
    },
    test("enum map key update") {
      val mh = EnumMapHolder(Map(
        ColorEnum.Red -> "fire",
        ColorEnum.Blue -> "water"
      ))
      val lens = DynaLens.into[EnumMapHolder].topLens
      val path = Path.parsePath("things[Red]")
      for {
        updated <- lens.update(path, "heat", mh)
      } yield assertTrue(
        updated == EnumMapHolder(
          Map(
            ColorEnum.Red -> "heat",
            ColorEnum.Blue -> "water"
          )
        )
      )
    },
    test("double-indexed chain update") {
      val deep = DeepRoot(DeepMapHolder(List(
        Map("d1" -> DeepItem("old", 1.0))
      )))
      val lens = DynaLens.into[DeepRoot].topLens
      val path = Path.parsePath("holder.items[0][d1]")
      for {
        updated <- lens.update(path, DeepItem("wow", 99.0), deep)
      } yield assertTrue(
        updated == DeepRoot(
          DeepMapHolder(List(
            Map("d1" -> DeepItem("wow", 99.0))
          ))
        )
      )
    },
    test("fail deep scalar child navigation") {
      val deep = DeepRoot(DeepMapHolder(List(
        Map("d1" -> DeepItem("old", 1.0))
      )))
      val lens = DynaLens.into[DeepRoot].topLens
      val path = Path.parsePath("holder.items[0][d1].price.child")
      for {
        result <- lens.update(path, 10.0, deep).either
      } yield assertTrue(result.isLeft)
    },
    test("deep nested update on optional-holder root") {
      val holder = Holder(Some(DeepNested(List(Map("k" -> 5)))))

      val lens = DynaLens.into[Holder].topLens
      val path = Path.parsePath("deep.items[0][k]")
      for {
        updated <- lens.update(path, 9, holder)
      } yield assertTrue(
        updated == Holder(Some(DeepNested(List(Map("k" -> 9)))))
      )
    },
    test("list update at boundary index should fail") {
      val order = Order("O1", List(Item("A",1), Item("B",2)))
      val lens = DynaLens.into[Order].topLens
      val path = Path.parsePath("items[2].qty") // size == 2 => invalid
      for {
        result <- lens.update(path, 10, order).either
      } yield assertTrue(result.isLeft)
    }
  ) @@ ziotestkit

