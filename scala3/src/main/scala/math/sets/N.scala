package math.sets

import math.Base._

/**
 * Set of natural numbers.
 */
object N extends BigSet[BigInt] with EnumerableSet[BigInt]:

  /**
    * Name of the set of natural numbers
    * @return "ℕ"
    */
  override def name: String = "ℕ"

  /**
    * Iterator over natural numbers
    * @return an iterator of natural numbers
    */
  override def iterator: Iterator[BigInt] =
    new Iterator[BigInt]:
      private var n: BigInt = -1
  
      override def hasNext: Boolean = true

      private val tooMany: Int = 1000
    
      override def next(): BigInt =
        n += 1
//      if n > tooMany // this is good for testing
//        throw new IllegalStateException("fuck...")
//
        n

  override def contains(n: BigInt): Boolean = n >= 0
