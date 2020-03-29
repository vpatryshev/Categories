package math

import scalakittens.Result

import scala.language.postfixOps
import scala.collection.breakOut

/**
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

  /**
    * Concatenates two strings with a connector; inserte
    * @param first
    * @param conn
    * @param second
    * @return
    */
  def concat(first: Any, conn: String, second: Any): String = {
    val s10 = String valueOf first trim
    val s1 = if (s10 contains " ") s"($s10)" else s10
    val s20 = String valueOf second trim
    val s2 = if (s20 contains " ") s"($s20)" else s20
    val insert = if (s1.length > 1 || s2.length > 1) s" $conn " else conn
    s1 + insert + s2
  }

}
