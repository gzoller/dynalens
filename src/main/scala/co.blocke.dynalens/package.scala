package co.blocke.dynalens

type DynaContext = scala.collection.mutable.Map[String, (Any, DynaLens[?])]

object DynaContext:
  def apply(target: Any, lens: DynaLens[?]): DynaContext =
    scala.collection.mutable.Map("top" -> (target, lens))

  def empty: DynaContext = scala.collection.mutable.Map.empty