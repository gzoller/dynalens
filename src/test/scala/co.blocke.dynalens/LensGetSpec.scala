package co.blocke.dynalens


import zio.*
import zio.test.*
import co.blocke.testkit.ZioTestKit._


object LensGetSpec extends ZIOSpecDefault:
  def spec = suite("DynaLens Get Tests")(
    test("macro builds DynaLens tree for Order") {
      val root = DynaLens.into[Order]
      assertTrue(root.topLens.name == "Order")
      assertTrue(root.schema.name == "Order")
      assertTrue(root.topLens.fields.contains("items"))
      assertTrue(root.topLens.fields("items").isInstanceOf[ListLens])
    },
    test("Get a path") {
      val order = Order("O1", List(Item("soap",3), Item("pen", 2)))
      val lens = DynaLens.into[Order].topLens
      for {
        itemLens <- lens.get(Path.parsePath("items[1].name"), order)
      } yield assertTrue(itemLens == "pen")
    },
    test("Get items list") {
      val order = Order("O1", List(Item("soap",3), Item("pen", 2)))
      val lens = DynaLens.into[Order].topLens
      for {
        items <- lens.get(Path.parsePath("items"), order)
      } yield assertTrue(items.asInstanceOf[List[Any]].size == 2)
    },
    test("Get Option[List]") {
      val someHolder = OptionListHolder(Some(List("a", "b")))
      val lens = DynaLens.into[OptionListHolder].topLens
      for {
        items <- lens.get(Path.parsePath("itemsOpt"), someHolder)
        itemsNone <- lens.get(Path.parsePath("itemsOpt"), OptionListHolder(None))
      } yield assertTrue(
        items.asInstanceOf[List[Any]].size == 2,
        itemsNone.asInstanceOf[List[Any]].isEmpty
      )
    },
    test("Get Map") {
      val holder = MapHolder(Map("x" -> 1, "y" -> 2))
      val lens = DynaLens.into[MapHolder].topLens
      for {
        m <- lens.get(Path.parsePath("things"), holder)
        xVal <- lens.get(Path.parsePath("things[x]"), holder)
      } yield assertTrue(
        m.asInstanceOf[Map[String, Int]].size == 2,
        xVal == 1
      )
      val holder2 = MapHolder2(Map(1 -> "x", 2 -> "y"))
      val lens2 = DynaLens.into[MapHolder2].topLens
      for {
        m <- lens2.get(Path.parsePath("things"), holder2)
        xVal <- lens2.get(Path.parsePath("things[2]"), holder2)
      } yield assertTrue(
        m.asInstanceOf[Map[String, Int]].size == 2,
        xVal == "y"
      )
    },
    test("Get Enum Map") {
      val holder = EnumMapHolder(Map(ColorEnum.Red -> "apple", ColorEnum.Blue -> "sky"))
      val lens = DynaLens.into[EnumMapHolder].topLens
      for {
        m <- lens.get(Path.parsePath("things"), holder)
        valBlue <- lens.get(Path.parsePath("things[Blue]"), holder)
      } yield assertTrue(
        m.asInstanceOf[Map[String, String]].size == 2,
        valBlue == "sky"
      )
    },
    test("Get Long Map") {
      val holder = LongMapHolder(Map(100L -> "century", 200L -> "double"))
      val lens = DynaLens.into[LongMapHolder].topLens
      for {
        m <- lens.get(Path.parsePath("things"), holder)
        val200 <- lens.get(Path.parsePath("things[200]"), holder)
      } yield assertTrue(
        m.asInstanceOf[Map[String, String]].size == 2,
        val200 == "double"
      )
    },
    test("Get Option[Map]") {
      val holderSome = OptionMapHolder(Some(Map("x" -> 1, "y" -> 2)))
      val holderNone = OptionMapHolder(None)
      val lens = DynaLens.into[OptionMapHolder].topLens
      for {
        mSome <- lens.get(Path.parsePath("mapOpt"), holderSome)
        mNone <- lens.get(Path.parsePath("mapOpt"), holderNone)
        xVal <- lens.get(Path.parsePath("mapOpt[x]"), holderSome)
      } yield assertTrue(
        mSome.asInstanceOf[Map[String, Int]].size == 2,
        mNone.asInstanceOf[Map[String, Int]].isEmpty,
        xVal == 1
      )
    },
    test("Get deeply nested path (field -> list index -> map key -> field)") {
      val deepItem1 = DeepItem("soap", 2.99)
      val deepItem2 = DeepItem("pen", 1.25)
      val holder = DeepMapHolder(List(Map("a" -> deepItem1, "b" -> deepItem2)))
      val root = DeepRoot(holder)
      val lens = DynaLens.into[DeepRoot].topLens
      for {
        desc <- lens.get(Path.parsePath("holder.items[0][b].desc"), root)
        price <- lens.get(Path.parsePath("holder.items[0][a].price"), root)
      } yield assertTrue(
        desc == "pen",
        price == 2.99
      )
    },
    test("Get Enum field as string") {
      val holder = EnumHolder(ColorEnum.Blue)
      val lens = DynaLens.into[EnumHolder].topLens
      for {
        c <- lens.get(Path.parsePath("color"), holder)
      } yield assertTrue(c == "Blue")
    },
    test("Get Optional Scalar in class returns Some(value) or None") {
      val holderSome = OptScalarHolder(Some(42))
      val holderNone = OptScalarHolder(None)
      val lens = DynaLens.into[OptScalarHolder].topLens
      for {
        vSome <- lens.get(Path.parsePath("opt"), holderSome)
        vNone <- lens.get(Path.parsePath("opt"), holderNone)
      } yield assertTrue(
        vSome == Some(42),
        vNone == None
      )
    },
    test("Get with list index out of bounds returns failure") {
      val order = Order("O1", List(Item("soap",3), Item("pen", 2)))
      val lens = DynaLens.into[Order].topLens
      for {
        result <- lens.get(Path.parsePath("items[10].name"), order).either
      } yield assertTrue(result.isLeft)
    },
    test("Get missing map key returns None (not failure)") {
      val holder = MapHolder(Map("x" -> 1, "y" -> 2))
      val lens = DynaLens.into[MapHolder].topLens
      for {
        result <- lens.get(Path.parsePath("things[z]"), holder).either
      } yield assertTrue(result == Right(None))
    },
    test("Get Enum inside list") {
      val order = ColorOrder(List(ColorItem(ColorEnum.Green)))
      val lens = DynaLens.into[ColorOrder].topLens
      for {
        c <- lens.get(Path.parsePath("items[0].color"), order)
      } yield assertTrue(c == "Green")
    },
    test("Get Enum inside map") {
      val holder = ColorMapHolder(Map("one" -> ColorEnum.Green))
      val lens = DynaLens.into[ColorMapHolder].topLens
      for {
        c <- lens.get(Path.parsePath("things[one]"), holder)
      } yield assertTrue(c == "Green")
    },
    test("Get root object with empty path") {
      val order = Order("O1", Nil)
      val lens = DynaLens.into[Order].topLens
      for {
        rootVal <- lens.get(Nil, order)
      } yield assertTrue(rootVal == order)
    },
    test("Get fails when path starts with index") {
      val order = Order("O1", Nil)
      val lens = DynaLens.into[Order].topLens
      for {
        result <- lens.get(Path.parsePath("[0]"), order).either
      } yield assertTrue(result.isLeft)
    },
    test("Get fails when list index is non-numeric") {
      val order = Order("O1", Nil)
      val lens = DynaLens.into[Order].topLens
      for {
        result <- lens.get(Path.parsePath("items[abc]"), order).either
      } yield assertTrue(result.isLeft)
    },
    test("Get Map inside Option[List]") {
      val holder = Holder(Some(DeepNested(List(Map("a" -> 10, "b" -> 20)))))
      val lens = DynaLens.into[Holder].topLens
      for {
        v <- lens.get(Path.parsePath("deep.items[0][b]"), holder)
      } yield assertTrue(v == 20)
    },
    test("Validate DynaContext behavior") {
      val lens = DynaLens.into[Order].topLens
      val ctx = DynaContext(Map.empty, TestHelpers.emptyDL)
        .bind("x", "wow", ScalarLens("x", false, None))
        .setThis(Order("O1", Nil), lens)
      assertTrue(ctx.getValue("x").contains("wow"))
      assertTrue(ctx.get("this").isDefined)
    },
    // === Anchored traversal tests ===

    test("this anchor should allow field traversal inside list elements") {
      case class Wrap(items: List[Item])
      val data = Wrap(List(Item("soap", 3), Item("pen", 2)))
      val lens = DynaLens.into[Wrap].topLens
      for {
        v <- lens.get(Path.parsePath("items[1].this.name"), data)
      } yield assertTrue(v == "pen")
    },
    test("this_value supports field traversal inside map values") {
      case class Wrap(things: Map[String, Item])
      val data = Wrap(Map("a" -> Item("soap", 3), "b" -> Item("pen", 2)))
      val lens = DynaLens.into[Wrap].topLens
      for {
        v <- lens.get(Path.parsePath("things[a].this_value.name"), data)
      } yield assertTrue(v == "soap")
    },
    test("this_key rejects field navigation") {
      case class Wrap(things: Map[String, Int])
      val data = Wrap(Map("a" -> 1))
      val lens = DynaLens.into[Wrap].topLens
      for {
        result <- lens.get(Path.parsePath("things[a].this_key.foo"), data).either
      } yield assertTrue(result.isLeft)
    },
    test("nested: list -> map -> this_value") {
      case class Wrap(items: List[Map[String, Item]])
      val data = Wrap(List(Map("x" -> Item("soap", 3))))
      val lens = DynaLens.into[Wrap].topLens
      for {
        v <- lens.get(Path.parsePath("items[0][x].this_value.qty"), data)
      } yield assertTrue(v == 3)
    }
  ) @@ ziotestkit