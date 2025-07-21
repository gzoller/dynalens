/*
 * Copyright (c) 2025 Greg Zoller
 * This software is licensed under the MIT License (see LICENSE file).
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
  def get(name: String): Option[BiMap[String,String]]
  def register(name: String, bimap: BiMap[String,String]): _BiMapRegistry
}

// Repo of BiMaps by name
class BiMapRegistry extends _BiMapRegistry {
  private val maps = scala.collection.mutable.Map.empty[String, BiMap[String,String]]

  def register(name: String, bimap: BiMap[String,String]): _BiMapRegistry =
    maps.put(name, bimap)
    this

  def get(name: String): Option[BiMap[String,String]] =
    maps.get(name)
}

object BiMapRegistry:
  def layer(registry: _BiMapRegistry): ULayer[_BiMapRegistry] =
    ZLayer.succeed(registry)

object EmptyBiMapRegistry extends _BiMapRegistry {
  def get(name: String): Option[BiMap[String, String]] = None
  def register(name: String, bimap: BiMap[String, String]): _BiMapRegistry = this
}