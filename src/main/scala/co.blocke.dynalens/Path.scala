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
  }
  case class Field(name: String) extends PathElement
  case class IndexedField(name: String, index: Option[String]) extends PathElement:
    def listIndex(): Option[Int] =
      index.flatMap(i => scala.util.Try(i.toInt).toOption)

  def parsePath(path: String): List[PathElement] =
    path.split("\\.").toList.map { segment =>
      val clean = segment

      val re = """^([^\[]+)(?:\[(\w+|"(?:[^"\\]|\\.)*")\])?$""".r
      clean match {
        case re(name, idx) if idx != null =>
          val unquoted =
            if idx.startsWith("\"") && idx.endsWith("\"") && idx.length >= 2 then
              idx.substring(1, idx.length - 1)
            else idx
          IndexedField(name, Some(unquoted))
        case re(name, _) =>
          Field(name)
      }
    }

  private val re = """^([^\[]+)(?:\[(\w+|"(?:[^"\\]|\\.)*")\])?$""".r
  def segmentAndIndex(seg: String): (String, Option[String]) = seg match
    case re(name, idx) =>
      val unquoted =
        if idx != null && idx.startsWith("\"") && idx.endsWith("\"") && idx.length >= 2 then
          idx.substring(1, idx.length - 1)
        else idx
      (name, Option(unquoted))
    case _ =>
      (seg, None)

  // Strip all "noise" out of path--just raw path--for DynaLens low-level operations
  def partialPath(pathParts: List[PathElement]): String =
    pathParts
      .map {
        case IndexedField(name, Some(i)) => s"$name[$i]"
        case Field(name)                 => name
        case IndexedField(name, None)   => name
      }
      .mkString(".")
