package math.cat


import math.cat.Functions._

object Pair {

  /**
   * Builds a function that builds pairs attaching x as the first element.
   *
   * @tparam X first component type
   * @tparam Y second component type
   * @param x the element to attach.
   * @return a function y -> (x, y)
   */
  def withLeft[X, Y](x: X): Injection[Y, (X,Y)] = new Injection[Y, (X,Y)] {
    def apply(y: Y): (X, Y) = (x, y)
  }

  /**
   * Builds a function that builds pairs attaching y as the second element.
   *
   * @tparam X first component type
   * @tparam Y second component type
   * @param y the element to attach.
   * @return a function x -> (x, y)
   */
  def withRight[X, Y](y: Y): Injection[X, (X,Y)] = new Injection[X, (X,Y)] {
    def apply(x: X): (X, Y) = (x, y)
  }
}