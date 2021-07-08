package math.sets
import scala.language.{implicitConversions, postfixOps}

/**
 * Representing binary relations
 */
abstract class BinaryRelation[X, Y] extends Function2[X, Y, Boolean]

object BinaryRelation:
  /**
   * Creates a relation that checks against a set of given pairs.
   *
   * @param pairs the set
   * @return the predicate
   * @tparam X first argument type
   * @tparam Y second argument type
   */
  def apply[X, Y](pairs: Set[(X, Y)]): BinaryRelation[X, Y] =
    new BinaryRelation[X, Y]:
      def apply(x: X, y: Y): Boolean = pairs.contains((x, y))

  /**
    * Represents a function of two arguments as a binary relation
    * @param f the function
    * @tparam X first argument type
    * @tparam Y second argument type
    * @return a binary relation representing the given function
    */
  implicit def apply[X, Y](f: Function2[X, Y, Boolean]): BinaryRelation[X, Y] =
    new BinaryRelation[X, Y]:
      def apply(x: X, y: Y) = f(x, y)
