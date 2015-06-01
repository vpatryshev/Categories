package math.cat


import scala.Product2
import Functions._

trait Pair[X,Y] extends Product2[X,Y] {

}

/**
 * Elementary pair handling
 */
case class BasePair[X,Y](x:X, y:Y) extends Pair[X, Y] {

}

object Pair {
  def pair[X,Y](x:X, y:Y) = BasePair(x,y)

  /**
   * Builds a function that builds pairs attaching x as the first element.
   *
   * @tparam X first component type
   * @tparam Y second component type
   * @param x the element to attach.
   * @return a function y -> (x, y)
   */
  def withLeft[X, Y](x: X): Injection[Y, Pair[X,Y]] = new Injection[Y, Pair[X,Y]] {
    def apply(y: Y) = pair(x, y)
  }

  /**
   * Builds a function that builds pairs attaching y as the second element.
   *
   * @tparam X first component type
   * @tparam Y second component type
   * @param y the element to attach.
   * @return a function x -> (x, y)
   */
  def withRight[X, Y](y: Y): Injection[X, Pair[X,Y]] = new Injection[X, Pair[X,Y]] {
    def apply(x: X) = pair(x, y)
  }
}