package scalakittens

import scala.collection.mutable

case class Cache[K,V](isFinite: Boolean, build: K => V):
  private val cache: mutable.Map[K, V] = mutable.Map[K, V]()

  private val app: K => V = if (isFinite) (k: K) => cache.getOrElseUpdate(k, build(k)) else (k: K) => build(k)

  inline def apply(k: K): V = app(k)
