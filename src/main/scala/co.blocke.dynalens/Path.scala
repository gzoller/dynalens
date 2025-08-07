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

  sealed trait PathElement {
    def name: String
    def isOptional: Boolean
  }
  case class Field(name: String, isOptional: Boolean = false) extends PathElement
  case class IndexedField(name: String, index: Option[Int], isOptional: Boolean = false) extends PathElement

  def parsePath(path: String): List[PathElement] = {
    path.split("\\.").toList.map { segment =>
      val isOptional = segment.endsWith("?")
      val clean = if isOptional then segment.dropRight(1) else segment

      if clean.matches(""".+\[\d+\]""") then
        val name = clean.takeWhile(_ != '[')
        val index = clean.dropWhile(_ != '[').drop(1).dropRight(1).toInt
        IndexedField(name, Some(index), isOptional)
      else if clean.endsWith("[]") then
        val name = clean.dropRight(2)
        IndexedField(name, None, isOptional)
      else
        Field(clean, isOptional)
    }
  }

  // Strip all "noise" out of path--just raw path--for DynaLens low-level operations
  def partialPath(pathParts: List[PathElement]): String =
    pathParts
      .map {
        case IndexedField(name, Some(i), _)   => s"$name[$i]"
        case Field(name, _)                   => name
        case IndexedField(name, _, _)         => name
      }
      .mkString(".")
