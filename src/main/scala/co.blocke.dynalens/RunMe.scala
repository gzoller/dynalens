package co.blocke.dynalens


@main def run(): Unit =

  val ti = Map("id" -> "", "items" -> Map("number" -> "", "qty" -> "", "num" -> "", "__type" -> "[]"), "num" -> "" )

  val paths = List(
    "items[].qty",
    "items[]",
    "items[3]"
  )

  paths.foreach( p =>
    println(s"Path: $p --> "+ parser.Utility.getPathType(p))
  )

  // TypeInfo
  // Map(id -> , items -> Map(number -> , qty -> , num -> , __type -> []), num -> )

  // Registry
  // List(items)

//  case class Item(number: String, qty: Int, num: Int = 7)
//  case class Shipment(id: String, items: List[Item], num: Int = 2)
