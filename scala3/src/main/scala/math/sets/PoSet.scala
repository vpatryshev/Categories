package math.sets

import math.Base._
import math.sets.Sets._
import scalakittens.{Good, Result}

import java.io.Reader

/**
 * Implementation of partially ordered set.
 * 
 * @tparam T poset element type
 * @param elements the set of elements
 * `le` a function that compares two elements a and b, returning true iff b >= a
 *
 */
class PoSet[T](val elements: Set[T], comparator: (T, T) => Boolean) extends Set[T]:

  /**
    * Checks whether one element is less or equal to another
    * Note: this function is a property of poset, not a property of its elements
    * @param x one element
    * @param y another element
    * @return true if x < y
    */
  def le(x: T, y: T): Boolean = comparator(x, y)

  /**
    * Checks whether one element is less or equal to another
    * Note: this function is a property of poset, not a property of its elements
    * @param p pair of elements
    * @return true if p._1 < p._2
    */
  def le(p : (T, T)): Boolean = comparator(p._1, p._2)

  /**
    * Checks whether this poset contains an element
    * @param t an element to check
    * @return true if this poset contains `t`
    */
  override def contains(t: T): Boolean = elements contains t
  
  /**
   * Iterates over elements of this poset
   */
  override def iterator: Iterator[T] = elements.iterator

  /**
   * Size of this poset
   */
  override def size: Int = elements.size

  override def hashCode: Int = elements.hashCode * 1021 + comparator.hashCode

  /**
    * Two posets are equal if they have the same elements and partial order is the same.
    *
    * @param other other poset to compare
    * @return true if these two posets are equal
    */
  override def equals(x: Any): Boolean = {
    x match
      case other: PoSet[_] => equal(other)
      case _ => false
  }

  private[this] def equal(other: PoSet[_]): Boolean =
    (other eq this) || {
      val setsAreEqual = elements == other.elements
      setsAreEqual && {
        val product = Sets.product2(other.elements, other.elements)
        product.forall(p => le(p.asInstanceOf[(T, T)]) == other.le(p))
      }
    }
    
  validatePoset()


  protected def validatePoset(): Unit = if (Sets.isFinite(elements))
    for {x <- elements} require(le(x, x), " reflexivity broken at " + x)

    for {x <- elements; y <- elements}
      if (le(x, y) && le(y, x)) require(x == y, " antisymmetry broken at " + x + ", " + y)

    for {x <- elements; y <- elements; z <- elements}
      if (le(x, y) && le(y, z)) require(le(x, z), "transitivity broken at " + x + ", " + y + ", " + z)
    
  
  /**
   * Builds a new poset out of this one, with the inverted order.
   *
   * @return a new poset with the order that is opposite to the original.
   */
  def unary_~ = new PoSet[T](elements, (x: T, y: T) => le(y, x))

  override def toString: String =
    def orderedPairs = Sets.product2(elements, elements) filter ((p: (T, T)) => le(p))
    "({" + (elements mkString ", ") + "}, {" +
            ((orderedPairs map (p => "" + p._1 + " <= " + p._2)) mkString ", ") + "})"
  
  override def excl(x: T): Set[T] = itsImmutable
  override def incl(x: T): Set[T] = itsImmutable

object PoSet:

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

  def apply[T](theElements: Set[T], comparator: (T, T) => Boolean) =
    new PoSet(theElements, comparator)

  def apply[T](comparator: (T, T) => Boolean, theElements: T*): PoSet[T] =
    new PoSet(Set(theElements: _*), comparator)

  def apply[T](setOfElements: Set[T]): PoSet[T] = apply(setOfElements, Nil)

  class PosetParser extends Sets.SetParser {
    def poset: Parser[Result[PoSet[String]]] = 
      "("~parserOfSet~","~"{"~repsep(pair, ",")~"}"~")"  ^^ {
        case "("~sOpt~","~"{"~mOpt~"}"~")" =>
          (sOpt andAlso Result.traverse(mOpt)).map {
            case (s, m) => PoSet(s, m.toList)
          }
        case nonsense => Result.error(s"Failed to parse $nonsense")
      }
      
    def pair: Parser[Result[(String, String)]] = word~"<="~word ^^ {
      case x~"<="~y => Good((x, y))
      case nonsense => Result.error(s"Failed to parse $nonsense")
    }

    private def explain(pr: ParseResult[Result[PoSet[String]]]): Result[PoSet[String]] =
      pr match
        case Success(poset, _) => poset
        case e: NoSuccess => Result.error(s"Failed to parse graph: $e")

    override def read(input: CharSequence): Result[PoSet[String]] =
      val pr: ParseResult[Result[PoSet[String]]] = parseAll(poset, input)
      explain(pr)

    override def read(input: Reader): Result[PoSet[String]] = explain(parseAll(poset, input))
  }

  def apply(input: Reader): Result[PoSet[String]] = (new PosetParser).read(input)

  def apply(input: CharSequence): Result[PoSet[String]] = (new PosetParser).read(input)

  /**
   * Builds a linear poset consisting of a range of integers, with their natural order.
   *
   * @param from the first integer in the range
   * @param to   the last intger in the range (included)
   * @param step distance between two consecutive elements
   * @return a new poset
   */
  def range(from: Int, to: Int, step: Int): PoSet[Int] =
    new PoSet(Sets.range(from, to, step), (x: Int, y: Int) => x <= y)
  
  private val comparator: (BigInt, BigInt) => Boolean = _ <= _

  lazy val ofNaturalNumbers: PoSet[BigInt] =
    new PoSet(N, comparator):
      override def validatePoset(): Unit = ()
