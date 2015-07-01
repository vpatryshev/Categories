package math.cat

/**
 * Natural numbers class.
 *
 * @author Vlad Patryshev
 * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
 * 
 */
object N extends IntSetWithoutSomeValues(Set.empty[Int]) {}

private[cat] class IntSetWithoutSomeValues(values: Set[Int]) extends Set[Int] {
  override def hasDefiniteSize = false

  override def contains(elem: Int): Boolean = !values.contains(elem)

  override def +(elem: Int): Set[Int] = new IntSetWithoutSomeValues(values - elem)

  override def -(elem: Int): Set[Int] = new IntSetWithoutSomeValues(values + elem)

  override def iterator: Iterator[Int] = new Iterator[Int] {
    private var i = -1
    override def hasNext: Boolean = true

    override def next(): Int = {
      do {
        i += 1
      } while (values contains(i))
      i
    }
  }
}
