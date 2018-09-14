package math.cat

/**
 * Representing binary relationships here
 */

abstract class BinaryRelationship[X, Y] extends Function2[X, Y, Boolean] {}

object BinaryRelationship {
  /**
   * Creates a relationship that checks against a set of given pairs.
   *
   * @param pairs the set
   * @return the predicate
   * @tparam X first argument type
   * @tparam Y second argument type
   */
  def apply[X, Y](pairs: Set[(X, Y)]): BinaryRelationship[X, Y] =
    new BinaryRelationship[X, Y] {
      def apply(x: X, y: Y): Boolean = pairs.contains((x, y))
    }

  implicit def apply[X, Y](f: Function2[X, Y, Boolean]) =
    new BinaryRelationship[X, Y] {
      def apply(x: X, y: Y) = f.apply(x, y)
    }
}