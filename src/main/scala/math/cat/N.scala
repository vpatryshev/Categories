package math.cat

/**
 * Natural numbers class.
 *
 * @author Vlad Patryshev
 * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
 * 
 */
object N extends IntSetWithoutSomeValues(Set.empty[BigInt]) {}

private[cat] class IntSetWithoutSomeValues(values: Set[BigInt]) extends Set[BigInt] {
  override def hasDefiniteSize = false

  override def contains(elem: BigInt): Boolean = !values.contains(elem)

  override def +(elem: BigInt): Set[BigInt] = new IntSetWithoutSomeValues(values - elem)

  override def -(elem: BigInt): Set[BigInt] = new IntSetWithoutSomeValues(values + elem)

  override def iterator: Iterator[BigInt] = new Iterator[BigInt] {
    private var i:BigInt = -1
    override def hasNext: Boolean = true

    override def next(): BigInt = {
      do {
        i += 1
      } while (values contains(i))
      i
    }
  }
}
