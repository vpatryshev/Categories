package math.cat
import scala.Product2
/**
 * Contains lazy pair implementation.
 *
 * @author Vlad Patryshev
 * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
 *
 */

case class LazyPair[X, Y](_1: X, f: X => Y)
  extends Product2[X, Y]
{
  private lazy val y = f(_1)
  override def toString = s"(${_1},${_2})"
  override def _2: Y = y
}
