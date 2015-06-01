package math.cat;

import scala.collection.Set

/**
 * Implementation of partially ordered set.
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 * 
 * @param [T] poset element type
 * @param underlyingSet the set of elements
 * le a function that compares two elements a and b, returning true iff b >= a
 *
 */
class PoSet[T] (theElements: scala.collection.Set[T], comparator: (T, T) => Boolean) extends Set[T] {
  val underlyingSet = theElements
  val le = comparator
  validate

  override def contains(t: T): Boolean = underlyingSet contains t
  
  /**
   * Iterates over elements of this poset
   */
  override def iterator: Iterator[T] = underlyingSet.iterator
  /**
   * Size of this poset
   */
  override def size = underlyingSet.size

  override def hashCode = iterator.hashCode
  
  override def equals(x: Any): Boolean = {
    x match {
      case other: PoSet[_] => other.equals(this)
      case _ => false
    }
  }

  private def validate {
    for(x <- iterator) require(le(x, x), " reflexivity broken at " + x)
    
    for(x <- iterator; y <- iterator) {
      if (le(x, y) && le(y, x)) require(x == y, " antisymmetry broken at " + x + ", " + y)
    }
    
    for(x <- iterator; y <- iterator; z <- iterator) {
      if (le(x, y) && le(y, z)) require(le(x, z), "transitivity broken at " + x + ", " + y + ", " + z)
    }
  }
  
  /**
   * Two posets are equal if they have the same elements and partial order is the same.
   *
   * @param other other poset to compare
   * @return true if these two posets are equal
   */
  private def equals(other: PoSet[T]): Boolean = {
    val isEqual = underlyingSet == other.underlyingSet
    val product = Sets.product(underlyingSet, underlyingSet)
    (isEqual /: product) ((bool: Boolean, p: (T, T)) => bool && (le(p._1, p._2) == other.le(p._1, p._2)))
  }
  
  /**
   * Builds a new poset out of this one, with the inverted order.
   *
   * @return a new poset with the order that is opposite to the original.
   */
  def unary_op = new PoSet[T](underlyingSet, ((x: T, y: T) => le(y, x)));

  override def toString = {
    def orderedPairs = Sets.product(underlyingSet, underlyingSet) filter ((p: (T, T)) => le(p._1, p._2))
    underlyingSet.toString + "{" + (orderedPairs map (p => "" + p._1 + " <= " + p._2)) + "}"
  }
}

object PoSet {

  /**
   * Builds a poset based on pairs of elements that define the partial order.
   * 
   * @param [T] element type 
   * @param elements elements of this poset
   * @param pairs    pairs of comparable elements
   * @return a new poset built on the data provided
   */
  def apply[T](theElements: Set[T], pairs: Set[(T, T)]) = 
      new PoSet(theElements, (a: T, b: T) => pairs.contains((a, b)))

  def apply[T](theElements: Set[T], comparator: (T, T) => Boolean) = new PoSet(theElements, comparator)

  implicit def discretePoSet[T](set: Set[T]) = apply(set, Set.empty[(T, T)])

  /**
   * Parses a poset from a kind of string that is produced by toString()
   *
   * @param source source string
   * @return a parsed poset
   */
  def apply(source: String): PoSet[String] = {
    val splitAt = source.indexOf("]{"); // separates elements from comparisions
    val pairsAsStrings = source.substring(splitAt + 2, source.length() - 1) split ",\\s*"
    var pairsAsArrays = pairsAsStrings.map(_ split "<=").filter(_.length == 2)
    def stringifiedPairs = pairsAsArrays.map((a: Array[String]) => (a(0), a(1)))
    val pairs = (Set.empty[(String, String)] /: stringifiedPairs)(_+_)
    val underlyingSet = Sets.parseSet(source.substring(0, splitAt + 1))
    apply(underlyingSet, pairs)
  }  
}
