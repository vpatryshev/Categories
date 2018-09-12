package math.cat

import math.cat.Sets.{BigSet, bigset}

import scala.reflect.ClassTag

/**
  * An implementation of Set that cannot enumerate its elements.
  * AC is also optional here.
  *
  * @tparam T element type
  * @author Vlad Patryshev
  */
abstract class BigSet[T] extends Set[T] { thisOne =>
  override def size: Int = Integer.MAX_VALUE

  def whoami: String = "A BIG SET"

  override def toString: String = whoami

  private def notEnumerable = throw new UnsupportedOperationException("This set is not enumerable")

  private def notMutable = throw new UnsupportedOperationException("This set is immutable")
  
  override def isEmpty: Boolean = notEnumerable

  override def iterator: Iterator[T] = notEnumerable

  override def toArray[S >: T : ClassTag]: Array[S] = notEnumerable
  
  override def +(value: T): BigSet[T] = if (contains(value)) this else new BigSet[T] {
    override def contains(another: T): Boolean = another == value || (thisOne contains another)
  }
  
  override def -(value: T): BigSet[T] = if (!contains(value)) this else new BigSet[T] {
    override def contains(another: T): Boolean = another != value && (thisOne contains another)
  }

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
  /**
    * A big set of all finite sets in Java. This set is infinite, of course.
    */
  val FINITE_SETS: BigSet[Set[_]] = new BigSet[Set[_]] {
    override def contains(s: Set[_]): Boolean = s.size < Integer.MAX_VALUE

    override def whoami: String = "SetF, the class of all finite sets"
  }

  def apply[T](p: T => Boolean): BigSet[T] = new BigSet[T] {
    override def contains(t: T) = p(t)
  }

}
