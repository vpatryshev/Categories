package math.sets

import math.Base._

/**
 * Set of natural numbers.
 */
object N extends BigSet[BigInt]("ℕ") with EnumerableSet[BigInt]:

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
//        throw new IllegalStateException("ok, I'm tired...")
//
        n

  override def contains(n: BigInt): Boolean = n >= 0
