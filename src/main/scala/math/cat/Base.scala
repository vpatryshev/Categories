package math.cat

import language.postfixOps
import scala.collection.breakOut

/**
  * This class, I hope, exists only temporarily, being translated from Java.
  * It will go once conversion to Scala is done.
  * Base tools used in this package.
  *
  * @author Vlad Patryshev
  *         All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
  *         Credits: http://antilamer.livejournal.com/245962.html
  */
object Base {
  /**
    * Given an array of keys and an array of values, builds a map
    * that maps these keys to the values.
    *
    * @tparam K    key type
    * @tparam V    value type
    * @param keys   the array of keys
    * @param values the array of values
    * @return a newly-built map
    */
  def buildMap[K, V](keys: Array[K], values: Iterable[V]): Map[K, V] = (keys zip values)(breakOut)

  /**
    * An empty iterator. This is a factory method,
    * to avoid type problems.
    *
    * @tparam T element type (there are no elements actually)
    * @return an iterator that has no next.
    */
  @deprecated
  def emptyIterator[T]: Iterator[T] = Iterator.empty

  /**
    * Builds a list out of given vararg elements.
    *
    * @tparam T      element type
    * @param elements the elements
    * @return a list of all these elements
    */
  @deprecated
  def list[T](elements: T*): List[T] = elements.toList

  /**
    * Creates a (virtual) disjoint union set from given collections.
    * Collections are supposed not to have repeating elements; they
    * also should be disjoint. Neither of these conditions is checked.
    *
    * @param collections vararg list of participating collections
    * @return a set that contains all elements of those collections
    */
  @deprecated("neither the old one nor the new one check for disjunction, so there")
  private[cat] def disjointUnion[A](collections: Seq[A]*): Set[A] =
    (Set.empty[A] /: collections)(_ ++ _)

  /**
    * Builds an inverse map. Assumes that the map is inversible.
    *
    * @tparam A map key type
    * @tparam B map value type
    * @param m the map to invert
    * @return inverse for map m
    */
  def inverse[A, B](m: Map[A, B]): Map[B, A] = {
    
    val result: Map[B, A] = (m map {
      case (k, v) => v -> k
    })(breakOut)
        
    require(result.size == m.size, "map not invertible")
    result
  }

  /**
    * Selects one of non-null elements of a given Iterable of arguments.
    *
    * @tparam T        element type
    * @param candidates the candidates to choose from
    * @return optional candidate
    */
  @deprecated
  def oneOf[T](candidates: Iterable[T]): Option[T] = candidates.headOption

  /**
    * Selects one of non-null elements of a given vararg of arguments.
    *
    * @tparam T        element type
    * @param candidates the candidates to choose from
    * @return optionally, a candidate that is not null
    */
  @deprecated
  def oneOf[T](candidates: T*): Option[T] = oneOf(candidates filter (null !=))

  /**
    * Builds a Cartesian product of two sets.
    *
    * @tparam X element type of first set
    * @tparam Y element type of second set
    * @param xs first set
    * @param ys second set
    * @return Cartesian product of two sets: the set of all possible pairs.
    */
  def setProduct[X, Y](xs: Set[X], ys: Set[Y]): Set[Pair[X, Y]] = {
    (for {
      x <- xs; y <- ys
    } yield Pair(x, y))(breakOut)
  }

  /**
    * Flattens an Iterable of Iterables.
    *
    * @tparam T                 element type
    * @param iterableOfIterables an Iterable that lists Iterables to join
    * @return an Iterable that scans over elements of the listed Iterables
    */
  @deprecated
  def flatten[T](iterableOfIterables: Iterable[_ <: Iterable[T]]): Iterable[T] =
    iterableOfIterables.flatten

  /**
    * Flattens a list of lists.
    *
    * @tparam T         list element type
    * @param listofLists a list of lists
    * @return a list of elements of all the inner lists, in the right order.
    */
  @deprecated
  def flattenList[T](listofLists: List[_ <: List[T]]): List[T] =
    listofLists.flatten

  @deprecated
  def flatten[T](listofLists: List[_ <: List[T]]): List[T] = flattenList(listofLists)

  /**
    * Concatenates a sequence of iterables
    *
    * @tparam T       element type
    * @param iterables the iterables to concatenate
    * @return a flat iterable, listing elements of the given iterables
    */
  @deprecated
  def concat[T](iterables: Iterable[T]*): Iterable[T] = iterables.flatten

  /**
    * Concatenates a vararg of lists.
    *
    * @tparam T   element type
    * @param lists the lists to concatenate.
    * @return the list produced by concatenating given lists
    */
  @deprecated
  def concat[T](lists: List[T]*): List[T] = lists.toList.flatten

  /**
    * Splits an Iterable into head and tail.
    *
    * @tparam T      element type
    * @param iterable the Iterable to split
    * @return a pair consisting of the Iterable's head and tail.
    */
  @deprecated
  def split[T](iterable: Iterable[T]): Pair[Option[T], Iterable[T]] =
    Pair(iterable.headOption, iterable.tail)

  /**
    * Spits a list into head and tail pair.
    *
    * @tparam T  element type
    * @param list the list to split
    * @return a pair consisting of head and tail
    */
  @deprecated
  def split[T](list: List[T]): Pair[Option[T], List[T]] = Pair(list.headOption, list.tail)

  /**
    * Count entries in an Iterable.
    *
    * @param iterable the iterable to scan
    * @return number of entries in the iterable
    */
  @deprecated
  def countEntries(iterable: Iterable[_]): Int = iterable.size

  /**
    * Builds a (virtual) list of integers from 0 to n-1.
    *
    * @param n number of elements in the list
    * @return the list
    */
  @deprecated
  def range(n: Int): List[Int] = range(0, n)

  /**
    * Builds a (virtual) list of integers from a to b-1.
    *
    * @param a first integer
    * @param b next after the last integer
    * @return the list
    */
  @deprecated
  def range(a: Int, b: Int): List[Int] = range(a, b, 1)

  /**
    * Builds a (virtual) list of integers from a to b-c, with a step c.
    *
    * @param a first integer
    * @param b next after the last integer
    * @param c step
    * @return the list
    */
  @deprecated
  def range(a: Int, b: Int, c: Int): List[Int] = Range(a, b, c).toList

  /**
    * Zips two lists.
    *
    * @tparam A element type of first list
    * @tparam B element type of second list
    * @param as first list
    * @param bs second list
    * @return the list of pairs of parallel elements of as and bs.
    */
  @deprecated
  def zip[A, B](as: List[A], bs: List[B]): List[Pair[A, B]] =
    (as zip bs) map { case (a, b) => Pair(a, b)}

  /**
    * Checks if two objects are equal.
    *
    * @param a first object
    * @param b second object
    * @return true if objects are equal, in Java sense
    */
  @deprecated
  protected def equal(a: Any, b: Any): Boolean = a == b

  /**
    * @{inheritDoc}
    */
  @deprecated
  protected def hashCode(a: Any): Int = if (a == null) 0
    else a.hashCode

  /**
    * Builds a map that returns a list element by its index.
    *
    * @tparam X  element type
    * @param list the list
    * @return the
    */
  def toMap[X](list: List[X]): Map[Int, X] =
    list.zipWithIndex map { case (x, i) => i -> x } toMap

  def id[T](set: Set[T]): Map[T, T] = set.map(t => t -> t)(breakOut)
}
