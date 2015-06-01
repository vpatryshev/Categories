package math.cat

object Functions {

  type Function[X, Y] = X => Y

  /**
   * Injection class.
   * Injection is a function that maps two different arguments to two different values.
   * What is good about an Injection is that when it maps a set, the resulting set
   * has the same number of elements.
   * @tparam X domain type
   * @tparam Y codomain type
   */
  abstract class Injection[X, Y] extends Function[X, Y] { self =>
    /**
     * Composes this Injection with another function; if the other function
     * is also an Injection, the result is an Injection too.
     *
     * @param g another function
     * @return a composition
     *         TODO(vpatryshev): figure out how to use double dispatch here.
     */
    def andThen[Z](g: Injection[Y, Z]): Injection[X, Z] = {
      new Injection[X, Z] { override def apply(x: X): Z = g(self(x)) }
    }

    /**
     * Maps a set to another set using this function
     * @param domain set to map
     * @return a set of function values on this set, since the function is an injection, it's a set
     */
    def map(domain: Set[_ <: X]): Set[Y] = {
      return new Set[Y] {
//        override def iterator: Iterator[Y] = {
//          return map(domain.iterator)
//        }
        override def size: Int = {
          return domain.size
        }

        override def contains(elem: Y): Boolean = ???

        override def +(elem: Y): Set[Y] = ???

        override def -(elem: Y): Set[Y] = ???

        override def iterator: Iterator[Y] = ???
      }
    }
  }

}
