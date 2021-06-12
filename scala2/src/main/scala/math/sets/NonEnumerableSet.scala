package math.sets

import math.Base._
import scala.reflect.ClassTag

trait NonEnumerableSet[T] extends Set[T] { thisOne: BigSet[T] =>
  private def notEnumerable = 
    throw new UnsupportedOperationException("This set is not enumerable")

  override def isEmpty: Boolean = notEnumerable

  def iterator: Iterator[T] = notEnumerable

  override def toArray[S >: T : ClassTag]: Array[S] = notEnumerable

  override def incl(elem: T): BigSet[T] = itsImmutable

  override def excl(elem: T): BigSet[T] = itsImmutable

}
