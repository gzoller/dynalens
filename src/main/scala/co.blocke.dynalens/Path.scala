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