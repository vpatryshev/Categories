package math.cat
import scala.Product2
/**
 * Contains lazy pair implementation.
 *
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 *
 */

case class LazyPair[X, Y](_1: X, f: X => Y)
  extends Product2[X, Y]
{
  private lazy val y = f(_1)
  override def toString() = "(" + _1 + "," + _2 + ")"
  override def _2 = y
}
