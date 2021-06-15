package math.sets

import math.Base._
import scala.reflect.ClassTag

trait NonEnumerable[T] extends IterableOnce[T] { thisOne: BigSet[T] =>
  def name: String = "This set"
  
  private def notEnumerable = 
    throw new UnsupportedOperationException(s"$name is not enumerable")

  override def isEmpty: Boolean = notEnumerable

  def iterator: Iterator[T] = notEnumerable

  override def toArray[S >: T : ClassTag]: Array[S] = notEnumerable

  override def incl(elem: T): BigSet[T] = itsImmutable

  override def excl(elem: T): BigSet[T] = itsImmutable

}
