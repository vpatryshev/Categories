package math.sets

import math.sets.BigSet.comprehension

/**
  * An implementation of a large Set, can be non-enumerable.
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
    * @return another big set, with values of type `U`, consisting of values of `f` on this bigset
    */
  infix def map[U](f: Functions.Bijection[T, U]): BigSet[U] =
    comprehension((u: U) => this contains (f unapply u))

  /**
    * Filters this bigset by a given predicate
    * @param p the predicate
    * @return a new bigset consisting only of values satisfying `p`
    */
  override def filter(p: T => Boolean): BigSet[T] =
    comprehension((t: T) => p(t) && (this contains t), s"$name, filtered")

  override def hashCode: Int = System.identityHashCode(this)

  /**
    * This is a pretty bad equality for sets, but what can we do?
    * @param other anything, according to Java rules
    * @return true if it is the same set
    */
  override def equals(other: Any): Boolean = other match
    case ref: AnyRef => this eq ref
    case notaref => false

object BigSet:
  def apply[T](source: Set[T]): BigSet[T] =
    new BigSet[T]() with EnumerableSet[T]:
      override def contains(t: T) = source(t)
      override def size: Int = source.size
      override def toString: String = source.toString
      override def iterator: Iterator[T] = source.iterator

  def of[T](name: String): BigSet[T] =
    new BigSet[T](name) with NonEnumerable[T, BigSet[T]]:
      override def contains(t: T) = true

  def comprehension[T](p: T => Boolean, name: String = "Big Set with a predicate"): BigSet[T] =
    new BigSet[T](name) with NonEnumerable[T, BigSet[T]]:
      override def contains(t: T) = p(t)
