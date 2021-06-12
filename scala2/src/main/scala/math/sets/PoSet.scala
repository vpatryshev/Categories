package math.sets

import java.io.Reader

import math.Base._

/**
 * Implementation of partially ordered set.
 * 
 * @tparam T poset element type
 * @param underlyingSet the set of elements
 * `le` a function that compares two elements a and b, returning true iff b >= a
 *
 */
class PoSet[T] (val underlyingSet: Set[T], comparator: (T, T) => Boolean) extends Set[T] {
  // note: this function is a property of poset, not a property of its elements
  def le(x: T, y: T): Boolean = comparator(x, y)
  def le(p : (T, T)): Boolean = comparator(p._1, p._2)
  validate()

  override def contains(t: T): Boolean = underlyingSet contains t
  
  /**
   * Iterates over elements of this poset
   */
//  override def seq: Set[T] = underlyingSet
  override def iterator: Iterator[T] = underlyingSet.iterator

  /**
   * Size of this poset
   */
  override def size: Int = underlyingSet.size

  override def hashCode: Int = underlyingSet.hashCode * 1021 + comparator.hashCode

  override def equals(x: Any): Boolean = {
    x match {
      case other: PoSet[T] => equal(other)
      case _ => false
    }
  }

  /**
    * Two posets are equal if they have the same elements and partial order is the same.
    *
    * @param other other poset to compare
    * @return true if these two posets are equal
    */
  private[sets] def equal(other: PoSet[T]): Boolean = {
    val isEqual = underlyingSet == other.underlyingSet
    val product = Sets.product2(underlyingSet, underlyingSet)
    isEqual && product.forall(p => le(p) == other.le(p))
  }

  protected def validate(): Unit = if (Sets.isFinite(underlyingSet)) {
    for {x <- underlyingSet} require(le(x, x), " reflexivity broken at " + x)
    
    for {x <- underlyingSet; y <- underlyingSet} {
      if (le(x, y) && le(y, x)) require(x == y, " antisymmetry broken at " + x + ", " + y)
    }
    
    for {x <- underlyingSet; y <- underlyingSet; z <- underlyingSet} {
      if (le(x, y) && le(y, z)) require(le(x, z), "transitivity broken at " + x + ", " + y + ", " + z)
    }
  }
  
  /**
   * Builds a new poset out of this one, with the inverted order.
   *
   * @return a new poset with the order that is opposite to the original.
   */
  def unary_~ = new PoSet[T](underlyingSet, (x: T, y: T) => le(y, x))

  override def toString: String = {
    def orderedPairs = Sets.product2(underlyingSet, underlyingSet) filter ((p: (T, T)) => le(p))
    "({" + (underlyingSet mkString ", ") + "}, {" +
            ((orderedPairs map (p => "" + p._1 + " <= " + p._2)) mkString ", ") + "})"
  }
  override def excl(x: T): Set[T] = itsImmutable
  override def incl(x: T): Set[T] = itsImmutable
}

object PoSet {

  /**
   * Builds a poset based on pairs of elements that define the partial order.
   * 
   * @tparam T element type
   * @param theElements elements of this poset
   * @param pairs    pairs of comparable elements
   * @return a new poset built on the data provided
   */
  def apply[T](theElements: Set[T], pairs: Set[(T, T)]) =
      new PoSet(theElements, (a: T, b: T) => a == b || pairs.contains((a, b)))

  def apply[T](theElements: Set[T], pairs: List[(T, T)]) = 
      new PoSet(theElements, (a: T, b: T) => a == b || pairs.contains((a, b)))

  def apply[T](theElements: Set[T], comparator: (T, T) => Boolean) = new PoSet(theElements, comparator)

  def apply[T](comparator: (T, T) => Boolean, theElements: T*): PoSet[T] = {
    new PoSet(Set(theElements: _*), comparator)
  }

  def apply[T](setOfElements: Set[T]): PoSet[T] = apply(setOfElements, Set.empty[(T, T)])

  class Parser extends Sets.Parser {
    def poset: Parser[PoSet[String]] = "("~parserOfSet~","~"{"~repsep(pair, ",")~"}"~")"  ^^ {
      case "("~s~","~"{"~m~"}"~")" => PoSet(s, m)
    }
    def pair: Parser[(String, String)] = word~"<="~word ^^ {case x~"<="~y => (x, y)}

    private def explain(pr: ParseResult[PoSet[String]]): PoSet[String] = {
      pr match {
        case Success(poset: PoSet[String], _) => poset
        case e: NoSuccess =>
          throw new IllegalArgumentException(s"Failed to parse graph: $e")
      }
    }

    override def read(input: CharSequence): PoSet[String] = {
      val pr: ParseResult[PoSet[String]] = parseAll(poset, input)
      explain(pr)
    }

    override def read(input: Reader): PoSet[String] = explain(parseAll(poset, input))
  }

  def apply(input: Reader): PoSet[String] = (new Parser).read(input)

  def apply(input: CharSequence): PoSet[String] = (new Parser).read(input)



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
  
  private val comparator: (BigInt, BigInt) => Boolean = _ <= _

  lazy val ofNaturalNumbers: PoSet[BigInt] =
    new PoSet(N, comparator) {
    override def validate(): Unit = ()
  }
}
