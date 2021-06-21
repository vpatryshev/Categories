package math.sets

import scala.language.postfixOps

/**
  * A trait for enumerable sets
  * @tparam T value type
  */
trait EnumerableSet[T] extends Set[T]:
  thisSet: BigSet[T] =>

  /**
    * Add an element to this set
    * @param x the element
    * @return a new BigSet that contains all previous elements and the new one
    */
  override def incl(x: T): BigSet[T] =
    if contains(x) then thisSet
    else new BigSet[T] with EnumerableSet[T]:
           override def contains(y: T): Boolean = y == x || (thisSet contains y)
           override def iterator: Iterator[T] = List(x).iterator ++ thisSet.iterator

  /**
    * Delete an element from this set
    * @param x the element
    * @return a new BigSet that contains all previous elements except `x`
    */
  override def excl(x: T): BigSet[T] =
    if !contains(x) then thisSet
    else new BigSet[T] with EnumerableSet[T]:
           override def contains(y: T): Boolean = y != x && (thisSet contains y)
           override def iterator: Iterator[T] = thisSet.iterator filter (x !=)
