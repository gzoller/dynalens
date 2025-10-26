package co.blocke.dynalens

case class Item(name: String, qty: Int)
case class Order(id: String, items: List[Item])

case class MapHolder(things: Map[String, Int])
case class MapHolder2(things: Map[Int, String])
case class OptionListHolder(itemsOpt: Option[List[String]])
case class OptionMapHolder(mapOpt: Option[Map[String, Int]])

// Deeply nested test models
case class DeepItem(desc: String, price: Double)
case class DeepMapHolder(items: List[Map[String, DeepItem]])
case class DeepRoot(holder: DeepMapHolder)

enum ColorEnum:
  case Red, Blue, Green

case class EnumMapHolder(things: Map[ColorEnum, String])
case class LongMapHolder(things: Map[Long, String])
case class EnumHolder(color: ColorEnum)

case class OptScalarHolder(opt: Option[Int])

case class ColorItem(color: ColorEnum)

case class ColorOrder(items: List[ColorItem])

case class ColorMapHolder(things: Map[String, ColorEnum])

case class DeepNested(items: List[Map[String, Int]])

case class Holder(deep: Option[DeepNested])

