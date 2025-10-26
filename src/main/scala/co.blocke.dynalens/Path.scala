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


case class PathElement(name: Option[String], index: Option[String]) {
  def listIndex(): Option[Int] =
    index.flatMap(i => scala.util.Try(i.toInt).toOption)
}


object Path:

//  private val segmentRe = """^([^.\[]+)(?:\[([^\]]+)\])?$""".r

  def parsePath(path: String): List[PathElement] =
    if path == null || path.trim.isEmpty then Nil
    else
      val buf = collection.mutable.ListBuffer.empty[PathElement]
      val sb = new StringBuilder
      var i = 0
      while i < path.length do
        path.charAt(i) match
          // Dotted boundary: end of field name
          case '.' =>
            if sb.nonEmpty then
              buf += PathElement(Some(sb.toString), None)
              sb.clear()
          // Bracketed index or key
          case '[' =>
            if sb.nonEmpty then
              // Normal case: something like items[0]
              val baseName = sb.toString
              sb.clear()
              i += 1
              val idx = new StringBuilder
              while i < path.length && path.charAt(i) != ']' do
                idx.append(path.charAt(i))
                i += 1
              if idx.nonEmpty then
                buf += PathElement(Some(baseName), Some(idx.toString))
            else
              // Case like [key] after a list or map
              i += 1
              val idx = new StringBuilder
              while i < path.length && path.charAt(i) != ']' do
                idx.append(path.charAt(i))
                i += 1
              if idx.nonEmpty then
                buf += PathElement(None, Some(idx.toString))
          case ']' => // handled by inner loop
          case c => sb.append(c)
        i += 1
      if sb.nonEmpty then buf += PathElement(Some(sb.toString), None)
      buf.toList