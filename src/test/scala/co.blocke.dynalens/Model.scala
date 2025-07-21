/*
 * Copyright (c) 2025 Greg Zoller
 * This software is licensed under the MIT License (see LICENSE file).
 */

package co.blocke.dynalens

case class Person(name: String, age: Int)
case class Department(level: Int, director: Person)
case class Company(id: String, dept: Department)


case class Item(number: String, qty: Int, num: Int = 7)
case class Shipment(id: String, items:List[Item], num: Int = 2)
case class Pack(label: String, caseSize: Int, shipments: List[Shipment])
case class Order(id: String, pack: Pack)

case class Ticket(id: java.util.UUID, when: java.util.Date)

// Utility fn for testing
def toStringCtx(ctx: Map[String, (Any,DynaLens[?])]): String = {
  val sb = new StringBuffer()
  ctx.foreach { case (key, (value,_)) =>
    sb.append(s"$key -> $value\n")
  }
  sb.toString
}