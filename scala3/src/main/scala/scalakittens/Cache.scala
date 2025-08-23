package scalakittens

import scalakittens.Params.*

import scala.collection.mutable

case class Cache[K,V](build: K => V) extends (K => V):
  private val cache: mutable.Map[K, V] = mutable.Map[K, V]()

    override def equals(obj: Any): Boolean = obj match
      case other: Cache[_, _] => build == other.build
      case anything => false

  inline def apply(k: K): V = cache.getOrElseUpdate(k, build(k))
