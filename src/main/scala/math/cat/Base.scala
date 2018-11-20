package math.cat

import language.postfixOps
import scala.collection.breakOut

/**
  * This class, I hope, exists only temporarily, being translated from Java.
  * It will go once conversion to Scala is done.
  * Base tools used in this package.
  */
object Base {

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
  def oneOf[T](candidates: T*): Option[T] = candidates find (null !=)

  /**
    * Builds a Cartesian product of two sets.
    *
    * @tparam X element type of first set
    * @tparam Y element type of second set
    * @param xs first set
    * @param ys second set
    * @return Cartesian product of two sets: the set of all possible pairs.
    */
  def setProduct[X, Y](xs: Set[X], ys: Set[Y]): Set[(X, Y)] = {
    (for {
      x <- xs; y <- ys
    } yield(x, y))(breakOut)
  }

  /**
    * Splits an Iterable into head and tail.
    *
    * @tparam T      element type
    * @param iterable the Iterable to split
    * @return a pair consisting of the Iterable's head and tail.
    */
  @deprecated
  def split[T](iterable: Iterable[T]): (Option[T], Iterable[T]) =
    (iterable.headOption, iterable.tail)

  /**
    * Spits a list into head and tail pair.
    *
    * @tparam T  element type
    * @param list the list to split
    * @return a pair consisting of head and tail
    */
  @deprecated
  def split[T](list: List[T]): (Option[T], List[T]) = (list.headOption, list.tail)

  /**
    * Builds a (virtual) list of integers from 0 to n-1.
    *
    * @param n number of elements in the list
    * @return the list
    */
  def range(n: Int): List[Int] = range(0, n)

  /**
    * Builds a (virtual) list of integers from a to b-1.
    *
    * @param a first integer
    * @param b next after the last integer
    * @return the list
    */
  def range(a: Int, b: Int): List[Int] = range(a, b, 1)

  /**
    * Builds a (virtual) list of integers from a to b-c, with a step c.
    *
    * @param a first integer
    * @param b next after the last integer
    * @param c step
    * @return the list
    */
  def range(a: Int, b: Int, c: Int): List[Int] = Range(a, b, c).toList

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
