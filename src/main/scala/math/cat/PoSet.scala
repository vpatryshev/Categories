package math.cat;

import java.io.Reader

import util.parsing.combinator.JavaTokenParsers
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
  // note: this function is a property of poset, not a property of its elements
  def le(x: T, y: T): Boolean = comparator(x, y)
  def le(p : (T, T)): Boolean = comparator(p._1, p._2)
  validate

  override def contains(t: T): Boolean = underlyingSet contains t
  
  /**
   * Iterates over elements of this poset
   */
  override def elements: Iterator[T] = underlyingSet.iterator
  override def iterator: Iterator[T] = underlyingSet.iterator

  /**
   * Size of this poset
   */
  override def size = underlyingSet.size

  override def hashCode = elements.hashCode

  override def equals(x: Any): Boolean = {
    x match {
      case other: PoSet[_] => other.asInstanceOf[PoSet[T]].equal(this)
      case _ => false
    }
  }

  private def validate {
    for(x <- elements) require(le(x, x), " reflexivity broken at " + x)
    
    for(x <- elements; y <- elements) {
      if (le(x, y) && le(y, x)) require(x == y, " antisymmetry broken at " + x + ", " + y)
    }
    
    for(x <- elements; y <- elements; z <- elements) {
      if (le(x, y) && le(y, z)) require(le(x, z), "transitivity broken at " + x + ", " + y + ", " + z)
    }
  }
  
  /**
   * Two posets are equal if they have the same elements and partial order is the same.
   *
   * @param other other poset to compare
   * @return true if these two posets are equal
   */
  private def equal(other: PoSet[T]): Boolean = {
    val isEqual = underlyingSet == other.underlyingSet
    val product = Sets.product(underlyingSet, underlyingSet)
    (isEqual /: product) ((bool, p) => bool && (le(p) == other.le(p)))
  }
  
  /**
   * Builds a new poset out of this one, with the inverted order.
   *
   * @return a new poset with the order that is opposite to the original.
   */
  def unary_~ = new PoSet[T](underlyingSet, ((x: T, y: T) => le(y, x)));

  override def toString = {
    def orderedPairs = Sets.product(underlyingSet, underlyingSet) filter ((p: (T, T)) => le(p))
    "[" + (underlyingSet mkString ", ") + "]{" +
            ((orderedPairs map (p => "" + p._1 + " <= " + p._2)) mkString ", ") + "}"
  }
  def -(x: T) = Sets.requireImmutability
  def +(x: T) = Sets.requireImmutability
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
      new PoSet(theElements, (a: T, b: T) => a == b || pairs.contains((a, b)))

  def apply[T](theElements: Set[T], pairs: List[(T, T)]) = 
      new PoSet(theElements, (a: T, b: T) => a == b || pairs.contains((a, b)))

  def apply[T](theElements: Set[T], comparator: (T, T) => Boolean) = new PoSet(theElements, comparator)

  def apply[T](comparator: (T, T) => Boolean, theElements: T*) = {
    val s: Set[T] = Set(theElements: _*)
    new PoSet(Set(theElements: _*), comparator)
  }

  def apply[T](set: Set[T]): PoSet[T] = apply(set, Set.empty[(T, T)])

  class Parser extends Sets.Parser {
    def poset: Parser[PoSet[String]] = "("~set~","~"{"~repsep(pair, ",")~"})"  ^^ {case "("~s~","~"{"~m~"})" => PoSet(s, m)}
    def pair: Parser[(String, String)] = member~"<="~member ^^ {case x~"<="~y => (x, y)}
    override def read(input: CharSequence) = parseAll(poset, input).get
    override def read(input: Reader) = parseAll(poset, input).get
  }

  def apply(input: Reader) = (new Parser).read(input)

  def apply(input: CharSequence) = (new Parser).read(input)

  /**
   * Builds a linear poset consisting of a range of integers, with their natural order.
   *
   * @param from the first integer in the range
   * @param to   the last intger in the range (included)
   * @param step distance between two consecutive elements
   * @return a new poset
   */
  def range(from: Int, to: Int, step: Int): PoSet[Int] = {
    new PoSet(Sets.range(from, to, step), (x: Int, y: Int) => x <= y)
  }
}
