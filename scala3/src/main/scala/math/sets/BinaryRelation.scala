package math.sets
import scala.language.postfixOps
import scala.language.implicitConversions

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

//  given Conversion[X, Y] with
//    def apply(str: String): Token = new KeyWord(str)
  
  implicit def apply[X, Y](f: Function2[X, Y, Boolean]): BinaryRelation[X, Y] =
    new BinaryRelation[X, Y]:
      def apply(x: X, y: Y) = f(x, y)
