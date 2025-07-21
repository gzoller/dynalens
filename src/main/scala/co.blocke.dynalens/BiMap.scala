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

import zio.*

final class BiMap[A, B] private (
    val forward: Map[A, B],
    val reverse: Map[B, A]
) {
  def getForward(a: A): Option[B] = forward.get(a)
  def getReverse(b: B): Option[A] = reverse.get(b)

  def containsForward(a: A): Boolean = forward.contains(a)
  def containsReverse(b: B): Boolean = reverse.contains(b)

  def keys: Iterable[A] = forward.keys
  def values: Iterable[B] = forward.values
}

object BiMap {
  def apply[A, B](entries: Iterable[(A, B)]): BiMap[A, B] = {
    val f = entries.toMap
    val r = entries.map(_.swap).toMap
    new BiMap(f, r)
  }

  def fromMap[A, B](m: Map[A, B]): BiMap[A, B] = {
    val r = m.map(_.swap)
    new BiMap(m, r)
  }
}

trait _BiMapRegistry {
  def get(name: String): Option[BiMap[String, String]]
  def register(name: String, bimap: BiMap[String, String]): _BiMapRegistry
}

// Repo of BiMaps by name
class BiMapRegistry extends _BiMapRegistry {
  private val maps = scala.collection.mutable.Map.empty[String, BiMap[String, String]]

  def register(name: String, bimap: BiMap[String, String]): _BiMapRegistry =
    maps.put(name, bimap)
    this

  def get(name: String): Option[BiMap[String, String]] =
    maps.get(name)
}

object BiMapRegistry:
  def layer(registry: _BiMapRegistry): ULayer[_BiMapRegistry] =
    ZLayer.succeed(registry)

object EmptyBiMapRegistry extends _BiMapRegistry {
  def get(name: String): Option[BiMap[String, String]] = None
  def register(name: String, bimap: BiMap[String, String]): _BiMapRegistry = this
}
