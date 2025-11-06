package co.blocke.dynalens
package parser


final case class Item(number: String, qty: Int, num: Int)
final case class Shipment(id: String, items: List[Item], total: Int)

