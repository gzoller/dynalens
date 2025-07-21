/*
 * Copyright (c) 2025 Greg Zoller
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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