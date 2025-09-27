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
case class Shipment(id: String, items: List[Item], num: Int = 2)
case class Pack(label: String, caseSize: Int, shipments: List[Shipment])
case class Order(id: String, pack: Pack)

case class Ticket(id: java.util.UUID, when: java.util.Date)

case class Registry(id: String, giftNums: List[Int], giftDesc: List[String])

case class Maybe(id: String, dunno: Option[String] = None, interest: Option[List[Item]] = None)
case class MyLists(id: Int, l1: List[Int], l2: Option[List[Int]])
case class ListOfOpt(id: Int, l1: List[Option[Int]])
case class ComplexLists(id: Int, l1: List[Int], l2: List[List[Int]])

case class Mapped(id: Int, m: Map[String, Int], om: Option[Map[String, Int]], cplx: Map[String, List[Person]])

// Schema test classes
case class Foo(a: String, b: Int)
case class Listy(names: List[String])
case class Opty(maybeId: Option[Int])
case class Mappy(props: Map[String, Int])
case class Combo(
                  notes: Option[List[Int]],
                  props: Map[String, Option[Int]]
                )

case class Address(city: String, zip: String)
case class Wrapper[T](value: T)
case class Person2(name: String, address: Address)
case class Order2(id: String, item: Wrapper[Address])

sealed trait Animal:
  val name: String
case class Dog(name: String, barkVolume: Int) extends Animal
case class Cat(name: String, lives: Int)      extends Animal

case class Zoo(animal: Animal)

case class OptTest(
                   l2: Option[List[Int]],
                   maybeInt: Option[Int],
                   nums: List[Int]
                 )