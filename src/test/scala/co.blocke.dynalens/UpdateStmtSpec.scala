package co.blocke.dynalens

import zio.*
import zio.test.*
import fn.*
import co.blocke.testkit.ZioTestKit._


object UpdateStmtSpec extends ZIOSpecDefault {
  case class Person(name: String, age: Int)
  case class UpdateAddress1(street: String, city: String)
  case class UpdatePerson1(name: String, age: Int, address: UpdateAddress1)
  case class UpdateAddress2(street: String, city: String)
  case class UpdatePersonOpt(name: String, address: Option[UpdateAddress2])
  case class UpdateItem(name: String, qty: Int)
  case class WithList(items: List[UpdateItem])
  case class WithListOpt(items: Option[List[UpdateItem]])
  case class WithMap(things: Map[String, UpdateItem])
  case class WithMapOpt(things: Option[Map[String, UpdateItem]])

  val lens = DynaLens.into[Person].topLens
  val lens2 = DynaLens.into[UpdatePerson1].topLens
  val lensOpt = DynaLens.into[UpdatePersonOpt].topLens

  val listLens = DynaLens.into[WithList].topLens
  val listOptLens = DynaLens.into[WithListOpt].topLens
  val mapLens = DynaLens.into[WithMap].topLens
  val mapOptLens = DynaLens.into[WithMapOpt].topLens

  def spec = suite("UpdateStmt")(

    test("Simple scalar update at top — modify Int field") {
      val p = Person("Greg", 51)
      val stmt = UpdateStmt("age", ConstantFn(42), "<pos>")
      for
        updated <- stmt.resolve(DynaContext(Map("top" -> (p, lens))))
        updatedPerson = updated.getValue("top").get.asInstanceOf[Person]
      yield assertTrue(updatedPerson.age == 42)
    },

    test("Simple scalar update at top — modify String field") {
      val p = Person("Greg", 51)
      val stmt = UpdateStmt("name", ConstantFn("Bob"), "<pos>")
      for
        updated <- stmt.resolve(DynaContext(Map("top" -> (p, lens))))
        updatedPerson = updated.getValue("top").get.asInstanceOf[Person]
      yield assertTrue(updatedPerson.name == "Bob")
    },

    test("Update missing field fails for required update") {
      val p = Person("Greg", 51)
      val stmt = UpdateStmt("bogus", ConstantFn("X"), "<pos>")
      for
        result <- stmt.resolve(DynaContext(Map("top" -> (p, lens)))).either
      yield assertTrue(result.isLeft)
    },

    test("Receiver null at top fails") {
      val p: Person = null.asInstanceOf[Person]
      val stmt = UpdateStmt("age", ConstantFn(99), "<pos>")
      for
        result <- stmt.resolve(DynaContext(Map("top" -> (p, lens)))).either
      yield assertTrue(result.isLeft)
    },

    test("Constant value type mismatch fails") {
      val p = Person("Greg", 51)
      val stmt = UpdateStmt("age", ConstantFn("nope"), "<pos>")
      for
        result <- stmt.resolve(DynaContext(Map("top" -> (p, lens)))).either
      yield assertTrue(result.isLeft)
    },

    test("Update nested scalar field — required parent") {
      val p = UpdatePerson1("Greg", 51, UpdateAddress1("Main", "Dallas"))
      val stmt = UpdateStmt("address.city", ConstantFn("Austin"), "<pos>")
      for
        updated <- stmt.resolve(DynaContext(Map("top" -> (p, lens2))))
        result = updated.getValue("top").get.asInstanceOf[UpdatePerson1]
      yield assertTrue(result.address.city == "Austin")
    },

    test("Missing nested field fails") {
      val p = UpdatePerson1("Greg", 51, UpdateAddress1("Main", "Dallas"))
      val stmt = UpdateStmt("address.bogus", ConstantFn("X"), "<pos>")
      for
        result <- stmt.resolve(DynaContext(Map("top" -> (p, lens2)))).either
      yield assertTrue(result.isLeft)
    },

    test("Null nested parent → required fails") {
      val p = UpdatePerson1("Greg", 51, null.asInstanceOf[UpdateAddress1])
      val stmt = UpdateStmt("address.city", ConstantFn("Anywhere"), "<pos>")
      for
        result <- stmt.resolve(DynaContext(Map("top" -> (p, lens2)))).either
      yield assertTrue(result.isLeft)
    },

    test("Optional nested present → success") {
      val p = UpdatePersonOpt("Greg", Some(UpdateAddress2("Main", "Dallas")))
      val stmt = UpdateStmt("address.city", ConstantFn("Austin"), "<pos>")
      for
        updated <- stmt.resolve(DynaContext(Map("top" -> (p, lensOpt))))
        result = updated.getValue("top").get.asInstanceOf[UpdatePersonOpt]
      yield assertTrue(result.address.get.city == "Austin")
    },

    test("Optional nested missing → does not crash; remains None") {
      val p = UpdatePersonOpt("Greg", None)
      val stmt = UpdateStmt("address.city", ConstantFn("Austin"), "<pos>")
      for
        updateResult <- stmt.resolve(DynaContext(Map("top" -> (p, lensOpt)))).either
      yield assertTrue(updateResult.isRight) // path not applied, but no crash
    },

    test("Nested type mismatch fails") {
      val p = UpdatePerson1("Greg", 51, UpdateAddress1("Main", "Dallas"))
      val stmt = UpdateStmt("address.city", ConstantFn(123), "<pos>")
      for
        result <- stmt.resolve(DynaContext(Map("top" -> (p, lens2)))).either
      yield assertTrue(result.isLeft)
    },

    test("Update optional nested field to None") {
      val p = UpdatePersonOpt("Greg", Some(UpdateAddress2("Main", "Dallas")))
      val stmt = UpdateStmt("address", ConstantFn(None), "<pos>")
      for
        updated <- stmt.resolve(DynaContext(Map("top" -> (p, lensOpt))))
        result = updated.getValue("top").get.asInstanceOf[UpdatePersonOpt]
      yield assertTrue(result.address.isEmpty)
    },

    test("List element update — success") {
      val p = WithList(List(UpdateItem("aaa", 1)))
      val stmt = UpdateStmt("items[0].qty", ConstantFn(5), "<pos>")
      for
        updated <- stmt.resolve(DynaContext(Map("top" -> (p, listLens))))
        result = updated.getValue("top").get.asInstanceOf[WithList]
      yield assertTrue(result.items.head.qty == 5)
    },

    test("List index out of bounds fails") {
      val p = WithList(List(UpdateItem("aaa", 1)))
      val stmt = UpdateStmt("items[2].qty", ConstantFn(99), "<pos>")
      for
        result <- stmt.resolve(DynaContext(Map("top" -> (p, listLens)))).either
      yield assertTrue(result.isLeft)
    },

    test("Optional list missing → remains None, no crash") {
      val p = WithListOpt(None)
      val stmt = UpdateStmt("items[0].qty", ConstantFn(7), "<pos>")
      for
        result <- stmt.resolve(DynaContext(Map("top" -> (p, listOptLens)))).either
      yield assertTrue(result.isRight) // ignored path, remains None
    },

    test("List inner type mismatch fails") {
      val p = WithList(List(UpdateItem("aaa", 1)))
      val stmt = UpdateStmt("items[0].qty", ConstantFn("nope"), "<pos>")
      for
        result <- stmt.resolve(DynaContext(Map("top" -> (p, listLens)))).either
      yield assertTrue(result.isLeft)
    },

    // ---- Maps ----

    test("Map value update — success") {
      val p = WithMap(Map("one" -> UpdateItem("aaa", 1)))
      val stmt = UpdateStmt("things[one].qty", ConstantFn(10), "<pos>")
      for
        updated <- stmt.resolve(DynaContext(Map("top" -> (p, mapLens))))
        result = updated.getValue("top").get.asInstanceOf[WithMap]
      yield assertTrue(result.things("one").qty == 10)
    },

    test("Missing map key fails") {
      val p = WithMap(Map("one" -> UpdateItem("aaa", 1)))
      val stmt = UpdateStmt("things[two].qty", ConstantFn(99), "<pos>")
      for
        result <- stmt.resolve(DynaContext(Map("top" -> (p, mapLens)))).either
      yield assertTrue(result.isLeft)
    },

    test("Optional map missing → remains None, no crash") {
      val p = WithMapOpt(None)
      val stmt = UpdateStmt("things[one].qty", ConstantFn(10), "<pos>")
      for
        result <- stmt.resolve(DynaContext(Map("top" -> (p, mapOptLens)))).either
      yield assertTrue(result.isRight)
    },

    test("Map inner type mismatch fails") {
      val p = WithMap(Map("one" -> UpdateItem("aaa", 1)))
      val stmt = UpdateStmt("things[one].qty", ConstantFn("bad"), "<pos>")
      for
        result <- stmt.resolve(DynaContext(Map("top" -> (p, mapLens)))).either
      yield assertTrue(result.isLeft)
    },


    test("Direct map assignment with missing key inserts new entry") {
      val p = WithMap(Map("one" -> UpdateItem("aaa", 1)))
      val stmt = UpdateStmt("things[two]", ConstantFn(UpdateItem("bbb", 2)), "<pos>")
      for
        updated <- stmt.resolve(DynaContext(Map("top" -> (p, mapLens))))
        result = updated.getValue("top").get.asInstanceOf[WithMap]
      yield assertTrue(result.things("two") == UpdateItem("bbb", 2))
    },

    test("Optional map missing + direct assignment → ignored (remains None)") {
      val p = WithMapOpt(None)
      val stmt = UpdateStmt("things[two]", ConstantFn(UpdateItem("bbb", 2)), "<pos>")
      for
        result <- stmt.resolve(DynaContext(Map("top" -> (p, mapOptLens)))).either
      yield assertTrue(result.isRight) // ignored, stays None
    },

    test("Nested update on missing map key fails") {
      val p = WithMap(Map("one" -> UpdateItem("aaa", 1)))
      val stmt = UpdateStmt("things[two].qty", ConstantFn(7), "<pos>")
      for
        result <- stmt.resolve(DynaContext(Map("top" -> (p, mapLens)))).either
      yield assertTrue(result.isLeft)
    },

    test("Direct list assignment OOB fails (lists do not auto-expand)") {
      val p = WithList(List(UpdateItem("aaa", 1)))
      val stmt = UpdateStmt("items[2]", ConstantFn(UpdateItem("bbb", 2)), "<pos>")
      for
        result <- stmt.resolve(DynaContext(Map("top" -> (p, listLens)))).either
      yield assertTrue(result.isLeft)
    },

    test("Nested list update OOB still fails") {
      val p = WithList(List(UpdateItem("aaa", 1)))
      val stmt = UpdateStmt("items[2].qty", ConstantFn(5), "<pos>")
      for
        result <- stmt.resolve(DynaContext(Map("top" -> (p, listLens)))).either
      yield assertTrue(result.isLeft)
    },

    test("Update using this: modify top-level field via this") {
      val p = Person("Greg", 51)
      val stmt = UpdateStmt("this.age", ConstantFn(33), "<pos>")
      for
        updated <- stmt.resolve(DynaContext(Map("top" -> (p, lens))))
        result = updated.getValue("top").get.asInstanceOf[Person]
      yield assertTrue(result.age == 33)
    },

    test("Inner reference: use this inside nested update") {
      val p = WithList(List(UpdateItem("aaa", 1), UpdateItem("bbb", 2)))
      val stmt = UpdateStmt(
        "items[1].qty",
        AddFn(
          GetFn("this.items[1].qty", false, RootFn, "<pos>"),
          List(ConstantFn(10)),
          "<pos>"
        ),
        "<pos>"
      )
      for
        updated <- stmt.resolve(DynaContext(Map("top" -> (p, listLens))))
        result = updated.getValue("top").get.asInstanceOf[WithList]
      yield assertTrue(result.items(1).qty == 12)
    },

    test("this resolves relative to current object, not top") {
      val p = UpdatePerson1("Greg", 51, UpdateAddress1("Main", "Dallas"))
      val stmt = UpdateStmt(
        "address.city",
        ConcatFn(
          GetFn("this", false, RootFn, "<pos>"),
          List(ConstantFn(" - TX")),
          "<pos>"
        ),
        "<pos>"
      )
      for
        updated <- stmt.resolve(DynaContext(Map("top" -> (p, lens2))))
        result = updated.getValue("top").get.asInstanceOf[UpdatePerson1]
      yield assertTrue(result.address.city == "Dallas - TX")
    },

    test("this inside optional nested parent missing → ignored") {
      val p = UpdatePersonOpt("Greg", None)
      val stmt = UpdateStmt(
        "address.city",
        ConcatFn(
          GetFn("this.city", false, RootFn, "<pos>"),
          List(ConstantFn("!")),
          "<pos>"
        ),
        "<pos>"
      )
      for
        updateResult <- stmt.resolve(DynaContext(Map("top" -> (p, lensOpt)))).either
      yield assertTrue(updateResult.isRight)
    },
  ).provide(
    ZLayer.succeed(
      RuntimeEnv(
        biMapRegistry = new BiMapRegistry()
      )
    )
  ) @@ ziotestkit
}
