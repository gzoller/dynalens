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
    },
    test("list of primitives") {
      val lens = dynalens[Listy]
      val schema = lens._schema

      assertTrue(
        schema.className == "co.blocke.dynalens.Listy",
        schema.fields.collect { case l: ListType => l.typeName } == List("scala.collection.immutable.List[java.lang.String]"),
        schema.fields.collect { case l: ListType => l.elementType.typeName } == List("java.lang.String")
      )
    },
    test("option of primitive") {
      val lens = dynalens[Opty]
      val schema = lens._schema

      assertTrue(
        schema.className == "co.blocke.dynalens.Opty",
        schema.fields.collect { case o: OptionType => o.typeName } == List("scala.Option"),
        schema.fields.collect { case o: OptionType => o.valueType.typeName } == List("scala.Int")
      )
    },
    test("map of string->int") {
      val lens = dynalens[Mappy]
      val schema = lens._schema

      assertTrue(
        schema.className == "co.blocke.dynalens.Mappy",
        schema.fields.collect { case m: MapType => m.typeName }.head.startsWith("scala.collection"),
        schema.fields.collect { case m: MapType => m.keyType.typeName } == List("java.lang.String"),
        schema.fields.collect { case m: MapType => m.valueType.typeName } == List("scala.Int")
      )
    },
    test("option of list, and map with option values") {
      val lens = dynalens[Combo]
      val schema = lens._schema

      val notes = schema.fields.collectFirst { case o: OptionType if o.name == "notes" => o }
      val props = schema.fields.collectFirst { case m: MapType if m.name == "props"   => m }

      assertTrue(
        notes.isDefined,
        notes.get.valueType.isInstanceOf[ListType],
        props.isDefined,
        props.get.valueType.isInstanceOf[OptionType]
      )
    },
    test("nested plain class is added to master catalog") {
      val lens   = dynalens[Person2]
      val schema = lens._schema

      // the Person schema should have a ClassType field for 'address'
      val addressField = schema.fields.collectFirst { case c: ClassType if c.name == "address" => c }
      assertTrue(addressField.isDefined)

      // the Address schema must be present in the master catalog
      val inCatalog = schema.catalog.get(addressField.get.typeName)
      assertTrue(inCatalog.isDefined)

      // and the Address schema itself should contain its fields
      assertTrue(
        inCatalog.get.fields.exists {
          case ScalarType(n, t) if n == "city" && t == "java.lang.String" => true
          case _ => false
        }
      )
    },
    test("parameterized class field is inlined, not in master catalog") {
      val lens   = dynalens[Order2]
      val schema = lens._schema

      // the Order schema should have a ParamClassType field for 'item'
      val itemField = schema.fields.collectFirst { case p: ParamClassType if p.name == "item" => p }
      assertTrue(itemField.isDefined)

      val paramClass = itemField.get

      // the master catalog should NOT contain a schema for Wrapper[String]
      assertTrue(schema.catalog.get(paramClass.typeName).isEmpty)

      // but the ParamClassType must contain its own inline schema with a value field
      assertTrue(
        paramClass.schema.fields.exists {
          case ClassType(n, t) if n == "value" && t.endsWith("Address") => true
          case _ => false
        }
      )
    }
  )
}