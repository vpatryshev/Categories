package math.sets

import scalakittens.Containers.*
import math.sets.BigSet.comprehension

import scala.annotation.targetName

/**
  * An implementation of a large Set; can be non-enumerable.
  * AC is also optional here.
  *
  * @param name name of this big set
  * @tparam T element type
  */
abstract class BigSet[T](val name: String = "A BIG SET") extends Set[T]:
  override def size: Int = Sets.InfiniteSize

  override def toString: String = name

  /**
    * Maps a big set via a bijection
    * @param f a bijection
    * @tparam U value type for `f`
    * @return another big set, with values of type `U`, consisting of values of `f` on this big set
    */
  infix def map[U](f: Functions.Bijection[T, U]): BigSet[U] =
    comprehension((u: U) => (f unapply u) ∈ this)

  /**
    * Filters this big set by a given predicate
    * @param p the predicate
    * @return a new big set consisting only of values satisfying `p`
    */
  infix override def filter(p: T => Boolean): BigSet[T] =
    comprehension((t: T) => p(t) && (t ∈ this), s"$name, filtered")

  override def hashCode: Int = System.identityHashCode(this)

  /**
    * This is a pretty bad equality for sets, but what can we do?
    * @param other anything, according to Java rules
    * @return true if it is the same set
    */
  override def equals(other: Any): Boolean = other match
    case ref: AnyRef => this eq ref
    case other => false

object BigSet:
  def apply[T](source: Set[T]): BigSet[T] =
    new BigSet[T]() with EnumerableSet[T]:
      override def contains(t: T): Boolean = source(t)
      override def size: Int = source.size
      override def toString: String = source.toString
      override def iterator: Iterator[T] = source.iterator

  def of[T](name: String): BigSet[T] =
    new BigSet[T](name) with NonEnumerable[T, BigSet[T]]:
      override def contains(t: T) = true

  def comprehension[T](p: T => Boolean, name: String = "Big Set with a predicate"): BigSet[T] =
    new BigSet[T](name) with NonEnumerable[T, BigSet[T]]:
      override def contains(t: T): Boolean =
        try   p(t)
        catch case x: Throwable => false

  extension [T](x: T)
    @targetName("in")
    infix inline def ∈(X: BigSet[T]): Boolean = X contains x
    @targetName("notIn")
    infix inline def ∉(X: BigSet[T]): Boolean = !(X contains x)

