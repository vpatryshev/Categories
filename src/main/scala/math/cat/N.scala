package math.cat

/**
 * Set of natural numbers.
 *
 * @author Vlad Patryshev
 * 
 */
object N extends BigSet[BigInt] with EnumerableSet[BigInt] {
  
  override def iterator: Iterator[BigInt] = new Iterator[BigInt] {
    private var n: BigInt = -1
  
    override def hasNext: Boolean = true

    override def next(): BigInt = { n += 1; n }
  }
  
  override def contains(n: BigInt): Boolean = n >= 0
}
