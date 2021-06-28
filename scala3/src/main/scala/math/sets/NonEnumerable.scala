package math.sets

import math.Base._
import scala.reflect.ClassTag

/**
  * Marker trait for non-enumerable sets.
  * @tparam T value type
  * @tparam S set type
  */
trait NonEnumerable[T, S <: Set[T]] extends Set[T]:
  this: BigSet[T] =>
  
  private def notEnumerable = 
    throw new UnsupportedOperationException(s"$name is not enumerable")

  /**
    * Have to implement it for a set. But can't do it.
    * @return nothing
    * @throws an exception
    */
  override def isEmpty: Boolean = notEnumerable

  /**
    * Have to implement for a set. 
    * But it's not enumerable, so no iterator.
    * @return nothing
    */
  def iterator: Iterator[T] = notEnumerable

  /**
    * Have to implement for a set.
    * But it's not enumerable, so no array.
    * @tparam S value type
    * @return nothing
    * @throws an exception
    */
  override def toArray[S >: T : ClassTag]: Array[S] = notEnumerable

  /**
    * Have to implement it for a set. But can't do it.
    * @param elem value to insert
    * @return nothing
    * @throws an exception
    */
  override def incl(elem: T): S = itsImmutable

  /**
    * Have to implement it for a set. But can't do it.
    * @param elem value to exclude
    * @return nothing
    * @throws an exception
    */
  override def excl(elem: T): S = itsImmutable
