package co.blocke.dynalens
package util

import zio.test.*

object PathUtilSpec extends ZIOSpecDefault:

  // Minimal helper schemas
  private val itemSchema = ClassType(
    "Item",
    "Item",
    List(
      ScalarType("name", "String"),
      ScalarType("qty", "Int")
    )
  )

  private val orderSchema = ClassType(
    "Order",
    "Order",
    List(
      ListType("items", itemSchema, "List[Item]"),
      MapType("meta", ScalarType("k","String"), ScalarType("v","String"), "Map[String,String]"),
      itemSchema.copy(name = "maybeItem", isOptional = true)
    )
  )

  def spec = suite("PathUtilSpec") (

    // --------------------------------------------------------------------------
    // Root traversal
    // --------------------------------------------------------------------------
    test("Root scalar path") {
      val ft = ScalarType("foo", "Int")
      assertTrue(PathUtil.getPathType("foo", ft) == Right(ft))
    },

    test("Root list index simple") {
      val ft = ListType("nums", ScalarType("num", "Int"), "List[Int]")
      val result = PathUtil.getPathType("[2]", ft)
      assertTrue(result.exists(_.typeName == "Int"))
    },

    test("Root map index simple") {
      val ft = MapType("tbl", ScalarType("k","String"), ScalarType("v","Float"), "Map[String,Float]")
      val result = PathUtil.getPathType("[someKey]", ft)
      assertTrue(result.exists(_.typeName == "Float"))
    },

    test("Root option wrapping list") {
      val ft = ListType("nums", ScalarType("num","Int"), "List[Int]")
      val result = PathUtil.getPathType("[0]", ft)
      assertTrue(result.exists(_.typeName == "Int"))
    },

    // --------------------------------------------------------------------------
    // Standard nested traversal
    // --------------------------------------------------------------------------
    test("Simple class field lookup") {
      val result = PathUtil.getPathType("items", orderSchema)
      assertTrue(result.exists(_.typeName.contains("List[Item]")))
    },

    test("Class → List index") {
      val result = PathUtil.getPathType("items[1]", orderSchema)
      assertTrue(result.exists(_.typeName == "Item"))
    },

    test("Class → List index → field") {
      val result = PathUtil.getPathType("items[0].qty", orderSchema)
      assertTrue(result.exists(_.typeName == "Int"))
    },

    test("Class → Map index") {
      val result = PathUtil.getPathType("meta[someKey]", orderSchema)
      assertTrue(result.exists(_.typeName == "String"))
    },

    test("Class → Option unwrap → field") {
      val result = PathUtil.getPathType("maybeItem.name", orderSchema)
      assertTrue(result.exists(_.typeName == "String"))
    },

    // --------------------------------------------------------------------------
    // Curried and mixed indexing
    // --------------------------------------------------------------------------
    test("List[List[Int]] curried indexes") {
      val schema = ListType("root", ListType("inner", ScalarType("i","Int"), "List[Int]"), "List[List[Int]]")
      val result = PathUtil.getPathType("[2][1]", schema)
      assertTrue(result.exists(_.typeName == "Int"))
    },

    test("Map[String, List[Int]] curried indexes") {
      val schema = MapType("root", ScalarType("k","String"), ListType("v", ScalarType("i","Int"), "List[Int]"), "Map[String,List[Int]]")
      val result = PathUtil.getPathType("[key][3]", schema)
      assertTrue(result.exists(_.typeName == "Int"))
    },

    test("Option[List[Map[String, Float]]] curried indexes") {
      val inner = MapType("map", ScalarType("k","String"), ScalarType("v","Float"), "Map[String,Float]")
      val opt = ListType("l", inner, "List[Map[String,Float]]")
      val result = PathUtil.getPathType("[0][foo]", opt)
      assertTrue(result.exists(_.typeName == "Float"))
    },

    // --------------------------------------------------------------------------
    // Edge and error cases
    // --------------------------------------------------------------------------
    test("Invalid path: index on scalar") {
      val ft = ScalarType("n", "Int")
      val result = PathUtil.getPathType("[0]", ft)
      assertTrue(result.isLeft)
    },

    test("Invalid path: descend into scalar") {
      val ft = ScalarType("n", "Int")
      val result = PathUtil.getPathType("n.x", ft)
      assertTrue(result.isLeft)
    },

    test("Invalid path: missing field on class") {
      val result = PathUtil.getPathType("nope", orderSchema)
      assertTrue(result.isLeft)
    },

    test("Invalid path: extra index on non-container") {
      val result = PathUtil.getPathType("items[0].qty[3]", orderSchema)
      assertTrue(result.isLeft)
    },

    test("Invalid path: curried on scalar in chain") {
      val ft = ListType("root", ScalarType("v","Int"), "List[Int]")
      val result = PathUtil.getPathType("[0][1]", ft)
      assertTrue(result.isLeft)
    },

    test("Empty path string") {
      val result = PathUtil.getPathType("", orderSchema)
      assertTrue(result.exists(_.typeName == "scala.Any"))
    }
  )