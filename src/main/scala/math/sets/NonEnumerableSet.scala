package math.sets

import scala.reflect.ClassTag

trait NonEnumerableSet[T] extends Set[T] {
  private def notEnumerable = 
    throw new UnsupportedOperationException("This set is not enumerable")

  override def isEmpty: Boolean = notEnumerable

  def iterator: Iterator[T] = notEnumerable

  override def toArray[S >: T : ClassTag]: Array[S] = notEnumerable

  override def +(elem: T): Set[T] = notEnumerable

  override def -(elem: T): Set[T] = notEnumerable

}
