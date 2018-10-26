package math.cat

import scala.reflect.ClassTag

/**
  * An implementation of Set that cannot enumerate its elements.
  * AC is also optional here.
  *
  * @tparam T element type
  * @author Vlad Patryshev
  */
abstract class BigSet[T] extends Set[T] { thisOne =>
  override def size: Int = Int.MaxValue

  def whoami: String = "A BIG SET"

  override def toString: String = whoami
// TODO: figure out whether to kick this out.
//  override def containsAll(c: Seq[T]): Boolean = c.forall(contains)
//
//  override def addAll(c: Seq[_ <: T]): Boolean = notMutable
//
//  override def retainAll(c: util.Collection[_]): Boolean = notMutable
//
//  override def removeAll(c: util.Collection[_]): Boolean = notMutable
//
//  override def clear(): Unit = notMutable
  
def map[U] (f: Functions.Bijection[T,U]) : BigSet[U] = BigSet((u: U) => this contains (f unapply u))
  
  override def filter(p: T => Boolean): BigSet[T] = BigSet((t: T) => p(t) && (this contains t))

}

object BigSet {

  def apply[T](p: T => Boolean): BigSet[T] = new BigSet[T] with NotEnumerableSet[T] {
    override def contains(t: T) = p(t)
  }

}
