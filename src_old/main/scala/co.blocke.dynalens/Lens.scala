package co.blocke.dynalens


import zio.*

sealed trait Lens {
  def name: String
  def isOptional: Boolean
  def parent: Option[Lens]

  /** Get value at the given path (relative to this lens). */
  def get(path: List[String], obj: Any): ZIO[Any, DynaLensError, Any]

  /** Update value at the given path, returning the updated object. */
  def update(path: List[String], value: Any, obj: Any): ZIO[Any, DynaLensError, Any]

  /** Optionally expose children (ClassLens mostly). */
  def children: Map[String, Lens] = Map.empty
}