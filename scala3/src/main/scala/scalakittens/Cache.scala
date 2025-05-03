package scalakittens

import scala.collection.mutable

class Cache[K,V](isFinite: Boolean) {
  private val cache: mutable.Map[K, V] = mutable.Map[K, V]()

  def apply(k: K, build: K => V): V =
    val r = if (isFinite) then cache.getOrElseUpdate(k, build(k))
    else build(k)
    r
}
