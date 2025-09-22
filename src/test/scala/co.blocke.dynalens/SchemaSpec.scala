import co.blocke.dynalens.DynaLens.dynalens
import co.blocke.dynalens._
import zio.test._

object SchemaSpec extends ZIOSpecDefault {

  // --- Helper schemas for direct testing -------------------------------------

  val testSchema = ClassType(
    name     = "Root",
    typeName = "Root",
    fields   = List(
      OptionType("optUser",
        ClassType("optUser", "User", List(
          ScalarType("name", "java.lang.String"),
          ClassType("address", "Address", List(
            ScalarType("city", "java.lang.String"),
            OptionType("zipcode", ScalarType("zipcode", "java.lang.String"), "scala.Option")
          ))
        )),
        "scala.Option"
      ),
      ListType("orders",
        ClassType("orders", "Order", List(
          ClassType("item", "Wrapper[String]", List(
            ScalarType("value", "java.lang.String")
          ))
        )),
        "scala.List"
      ),
      ScalarType("id", "java.lang.String")
    )
  )

  val mapSchema = ClassType(
    name     = "Root",
    typeName = "Root",
    fields   = List(
      MapType(
        name      = "contacts",
        keyType   = ScalarType("key", "java.lang.String"),
        valueType = ClassType("value", "Contact", List(
          ScalarType("name", "java.lang.String"),
          OptionType("nickname", ScalarType("nickname", "java.lang.String"), "scala.Option")
        )),
        typeName  = "scala.collection.immutable.Map"
      )
    )
  )

  // --- Tests -----------------------------------------------------------------

  def spec = suite("Schema.resolvePath")(
    test("resolve scalar field") {
      assertTrue(Schema.resolvePath(testSchema, "id").map(_.scalaType) == Some("java.lang.String"))
    },
    test("resolve nested optional class field") {
      assertTrue(Schema.resolvePath(testSchema, "optUser.name").map(_.scalaType) == Some("Option[java.lang.String]"))
    },
    test("resolve doubly nested optional field") {
      assertTrue(Schema.resolvePath(testSchema, "optUser.address.zipcode").map(_.scalaType) == Some("Option[java.lang.String]"))
    },
    test("resolve list of class field (no option accumulation)") {
      assertTrue(Schema.resolvePath(testSchema, "orders.item.value").map(_.scalaType) == Some("java.lang.String"))
    },
    test("resolve bad path") {
      assertTrue(Schema.resolvePath(testSchema, "optUser.address.zipcode.foo").isEmpty)
    },
    test("resolve bad root field") {
      assertTrue(Schema.resolvePath(testSchema, "doesNotExist").isEmpty)
    },
    test("resolve map key type") {
      assertTrue(Schema.resolvePath(mapSchema, "contacts.key").map(_.scalaType) == Some("java.lang.String"))
    },
    test("resolve map value class field") {
      assertTrue(Schema.resolvePath(mapSchema, "contacts.value.name").map(_.scalaType) == Some("java.lang.String"))
    },
    test("resolve map value optional field") {
      assertTrue(Schema.resolvePath(mapSchema, "contacts.value.nickname").map(_.scalaType) == Some("Option[java.lang.String]"))
    },
    test("resolve bad map value path") {
      assertTrue(Schema.resolvePath(mapSchema, "contacts.value.wrongField").isEmpty)
    },
    test("resolve bad map root") {
      assertTrue(Schema.resolvePath(mapSchema, "notamap.key").isEmpty)
    },
    test("DynaLens embeds simple Schema") {
      val lens   = dynalens[Foo]
      val schema = lens._schema
      assertTrue(
        schema.typeName == "co.blocke.dynalens.Foo",
        schema.fields.map(_.name) == List("a", "b"),
        schema.fields.map(_.typeName) == List("java.lang.String", "scala.Int")
      )
    },
    test("list of primitives") {
      val lens = dynalens[Listy]
      val schema = lens._schema
      assertTrue(
        schema.typeName == "co.blocke.dynalens.Listy",
        schema.fields.collect { case l: ListType => l.typeName } == List("scala.collection.immutable.List[java.lang.String]"),
        schema.fields.collect { case l: ListType => l.elementType.typeName } == List("java.lang.String")
      )
    },
    test("option of primitive") {
      val lens = dynalens[Opty]
      val schema = lens._schema
      assertTrue(
        schema.typeName == "co.blocke.dynalens.Opty",
        schema.fields.collect { case o: OptionType => o.typeName } == List("scala.Option"),
        schema.fields.collect { case o: OptionType => o.valueType.typeName } == List("scala.Int")
      )
    },
    test("map of string->int") {
      val lens = dynalens[Mappy]
      val schema = lens._schema
      assertTrue(
        schema.typeName == "co.blocke.dynalens.Mappy",
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
    test("nested plain class is fully inlined") {
      val lens   = dynalens[Person2]
      val schema = lens._schema
      val addressField = schema.fields.collectFirst { case c: ClassType if c.name == "address" => c }
      assertTrue(addressField.isDefined)
      assertTrue(
        addressField.get.fields.exists {
          case ScalarType(n, t) if n == "city" && t == "java.lang.String" => true
          case _ => false
        }
      )
    },
    test("parameterized class field is fully inlined") {
      val lens   = dynalens[Order2]
      val schema = lens._schema
      val itemField = schema.fields.collectFirst { case c: ClassType if c.name == "item" => c }
      assertTrue(itemField.isDefined)
      assertTrue(
        itemField.get.fields.exists {
          case ClassType(n, t, _) if n == "value" && t.endsWith("Address") => true
          case _ => false
        }
      )
    },
    test("sealed trait field is represented as a SealedTraitType with all concrete subclasses") {
      val lens   = dynalens[Zoo]
      val schema = lens._schema

      // 1) Find the SealedTraitType for 'animal'
      val animalField = schema.fields.collectFirst { case t: SealedTraitType if t.name == "animal" => t }
      assertTrue(animalField.isDefined)

      val traitType = animalField.get
      assertTrue(traitType.typeName.endsWith("Animal"))

      // 2) Subtypes are just fully-qualified class names (strings)
      assertTrue(
        traitType.subTypes.exists(_.endsWith("Dog")),
        traitType.subTypes.exists(_.endsWith("Cat"))
      )

      // 3) Common fields live on the SealedTraitType itself
      assertTrue(
        traitType.fields.exists { case ScalarType(n, t) => n == "name" && t.endsWith("String"); case _ => false }
      )

      // (Optional) Ensure subtype-specific fields are NOT here (we only model common fields)
      assertTrue(
        !traitType.fields.exists { case ScalarType(n, _) => n == "barkVolume" || n == "lives"; case _ => false }
      )
    }
  )
}