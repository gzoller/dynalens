package co.blocke.dynalens

object Path:

  sealed trait PathElement:
    val name: String
  case class Field(name: String) extends PathElement
  case class IndexedField(name: String, index: Int) extends PathElement

  def parsePath(path: String): List[PathElement] = {
    val regex = """(\w+)(?:\[(\d*)\])?""".r
    path.split("\\.").toList.map {
      case regex(field, null)         => Field(field)
      case regex(field, "")           => IndexedField(field, -1)     // "[]" syntax
      case regex(field, indexStr)     => IndexedField(field, indexStr.toInt)
    }
  }
  
  def partialPath(pathParts: List[PathElement]): String =
    pathParts.map {
      case IndexedField(name, i) if i >= 0 => s"$name[$i]"
      case Field(name) => name
      case IndexedField(name, _) => s"$name"
    }.mkString(".")