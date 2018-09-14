package math.cat

import scala.reflect.ClassTag

trait EnumerableSet[T] extends Set[T] { thisOne: BigSet[T] =>

  override def +(value: T): BigSet[T] =
    if (contains(value)) thisOne else new BigSet[T] with EnumerableSet[T] {
    override def contains(another: T): Boolean = another == value || (thisOne contains another)

    override def iterator: Iterator[T] = List(value).iterator ++ thisOne.iterator
  }

  override def -(value: T): BigSet[T] =
    if (!contains(value)) thisOne else new BigSet[T] with EnumerableSet[T] {
    override def contains(another: T): Boolean = another != value && (thisOne contains another)

    override def iterator: Iterator[T] = thisOne.iterator filter (value !=)
  }


}
