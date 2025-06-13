package scalakittens

import scalakittens.Params.*

import scala.collection.mutable

case class Cache[K,V](tag: Any, build: K => V, isFinite: Boolean = true) extends (K => V):
  private val cache: mutable.Map[K, V] = mutable.Map[K, V]()

    override def equals(obj: Any): Boolean = obj match
      case other: Cache[_, _] =>
        build == other.build && isFinite == other.isFinite
      case anything => false
      
  private val app: K => V = 
    if isFinite then (k: K) => cache.getOrElseUpdate(k, build(k)) 
    else (k: K) => build(k)

  inline def apply(k: K): V = app(k)
