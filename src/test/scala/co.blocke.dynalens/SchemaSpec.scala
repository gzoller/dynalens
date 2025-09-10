package co.blocke.dynalens


import zio.test.*
import DynaLens.*

object SchemaSpec extends ZIOSpecDefault {

  val testSchema = Schema(
    className = "Root",
    fields = List(
      OptionType("optUser", ClassType("optUser", "User"), "scala.Option"),
      ListType("orders", ClassType("orders", "Order"), "scala.List"),
      ScalarType("id", "java.lang.String")
    ),
    catalog = Map(
      "User" -> Schema(
        className = "User",
        fields = List(
          ScalarType("name", "java.lang.String"),
          ClassType("address", "Address")
        ),
        catalog = Map.empty
      ),
      "Address" -> Schema(
        className = "Address",
        fields = List(
          ScalarType("city", "java.lang.String"),
          OptionType("zipcode", ScalarType("zipcode", "java.lang.String"), "scala.Option")
        ),
        catalog = Map.empty
      ),
      "Order" -> Schema(
        className = "Order",
        fields = List(
          ParamClassType(
            name = "item",
            typeName = "Wrapper[String]",
            schema = Schema(
              className = "Wrapper",
              fields = List(
                ScalarType("value", "java.lang.String")
              ),
              catalog = Map.empty
            )
          )
        ),
        catalog = Map.empty
      )
    )
  )

  val mapSchema = Schema(
    className = "Root",
    fields = List(
      MapType(
        name = "contacts",
        keyType = ScalarType("key", "java.lang.String"),
        valueType = ClassType("value", "Contact"),
        typeName = "scala.collection.immutable.Map"
      )
    ),
    catalog = Map(
      "Contact" -> Schema(
        className = "Contact",
        fields = List(
          ScalarType("name", "java.lang.String"),
          OptionType("nickname", ScalarType("nickname", "java.lang.String"), "scala.Option")
        ),
        catalog = Map.empty
      )
    )
  )

  def spec = suite("Schema.resolvePath")(
    test("resolve scalar field") {
      assertTrue(
        testSchema.resolvePath("id").map(_.scalaType) == Some("java.lang.String")
      )
    },
    test("resolve nested optional class field") {
      assertTrue(
        testSchema.resolvePath("optUser.name").map(_.scalaType) == Some("Option[java.lang.String]")
      )
    },
    test("resolve doubly nested optional field") {
      assertTrue(
        testSchema.resolvePath("optUser.address.zipcode").map(_.scalaType) == Some("Option[java.lang.String]")
      )
    },
    test("resolve list of class field (no option accumulation)") {
      assertTrue(
        testSchema.resolvePath("orders.item.value").map(_.scalaType) == Some("java.lang.String")
      )
    },
    test("resolve bad path") {
      assertTrue(
        testSchema.resolvePath("optUser.address.zipcode.foo").isEmpty
      )
    },
    test("resolve bad root field") {
      assertTrue(
        testSchema.resolvePath("doesNotExist").isEmpty
      )
    },
    test("resolve map key type") {
      assertTrue(
        mapSchema.resolvePath("contacts.key").map(_.scalaType) == Some("java.lang.String")
      )
    },
    test("resolve map value class field") {
      assertTrue(
        mapSchema.resolvePath("contacts.value.name").map(_.scalaType) == Some("java.lang.String")
      )
    },
    test("resolve map value optional field") {
      assertTrue(
        mapSchema.resolvePath("contacts.value.nickname").map(_.scalaType) == Some("Option[java.lang.String]")
      )
    },
    test("resolve bad map value path") {
      assertTrue(
        mapSchema.resolvePath("contacts.value.wrongField").isEmpty
      )
    },
    test("resolve bad map root") {
      assertTrue(
        mapSchema.resolvePath("notamap.key").isEmpty
      )
    },
    test("DynaLens embeds simple Schema") {
      val lens = dynalens[Foo]
      val s = lens._schema
      assertTrue(
        s.className == "co.blocke.dynalens.Foo",
        s.fields.map(_.name) == List("a", "b"),
        s.fields.map(_.typeName) == List("java.lang.String", "scala.Int"),
        s.catalog.isEmpty
      )
    }
  )
}