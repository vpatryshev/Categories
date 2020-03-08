package math

import scalakittens.Result

import scala.language.postfixOps
import scala.collection.breakOut

/**
  * This class, I hope, exists only temporarily, being translated from Java.
  * It will go once conversion to Scala is done.
  * Base tools used in this package.
  */
object Base {
  type IntMap[X] = Map[Int, X]

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
      case (k, v) ⇒ v → k
    })(breakOut)
        
    require(result.size == m.size, "map not invertible")
    result
  }

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
      x ← xs; y ← ys
    } yield(x, y))(breakOut)
  }

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
  def toMap[X](list: List[X]): IntMap[X] =
    list.zipWithIndex map { case (x, i) ⇒ i → x } toMap

  implicit class Optimist[T](opt: Option[T]) {
    def iHope: T = opt.getOrElse(throw new InstantiationException("Oops, no value"))
  }

}
