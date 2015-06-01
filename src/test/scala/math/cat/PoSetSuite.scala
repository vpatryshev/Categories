package math.cat



import org.scalatest.junit.JUnit3Suite
import org.scalatest.prop.Checkers
import org.scalacheck.Prop.forAll
import Sets._
import PoSet._

/**
 * Test suite for PoSet class
 * @author vpatryshev
 */
class PoSetSuite extends JUnit3Suite with Checkers {

  def testConstructor_plain_pairs {
    val sut = PoSet(Set("a", "b", "c"), Set(("a", "b"), ("a", "c"), ("b", "c")))
    assert(sut.le("a", "c"))
  }

   def testConstructor_plain_comparator {
     val sut = PoSet(Set("a", "b", "c"), (a: String, b: String) => a <= b)
     assert(sut.le("a", "c"))
  }

  def testConstructor_negativeTransitivity {
    try {
      PoSet(Set("a", "b", "c"), Set(("a", "b"), ("b", "c")))
      fail("Validator should have thrown an exception")
    } catch {
      case e: IllegalArgumentException => // as expected
    }
  }

  def testConstructor_negativeAntireflexivity {
    try {
      val sut = PoSet(Set("a", "b", "c"), Set(("a", "b"), ("b", "a")))
      fail("Validator should have thrown an exception")
    } catch {
      case e: IllegalArgumentException => // as expected
    }
  }

  def testEquals_positive {
    val sut1 = PoSet(Set("a", "b", "c"), Set(("a", "b"), ("a", "c"), ("b", "c")))
    val sut2 = PoSet(Set("c", "a", "b"), (x: String, y: String) => x <= y)
    assert(sut1 == sut2)
  }

  def testEquals_negative {
    val sut1 = PoSet(Set("a", "b", "c"), Set(("a", "b"), ("a", "c"), ("c", "b")))
    def naturalOrder(p: (String, String)) = p._1 <= p._2
    val sut2 = PoSet(Set("c", "a", "b"), (x: String, y: String) => x <= y)
    assert(sut1 != sut2)
  }

  def testEquals_differentTypes {
    val sut1 = PoSet(Set("1", "2", "3"), Set(("1", "2"), ("1", "3"), ("2", "3")))
    val sut2 = PoSet(Set(1, 2, 3), Set((1, 2), (1, 3), (2, 3)))
    assert(sut1 != sut2)
  }

   def testUnaryOp {
     val sut = PoSet(Set("a", "b", "c"), (a: String, b: String) => a <= b)
     val opsut = ~sut

     assert(opsut.le("c", "a"))
     assert(sut == ~opsut)
  }

  def testDiscrete {
    val sut:PoSet[Int] = PoSet(Set(1, 2, 3))
    assert(sut.le(3, 3))
    assert(!sut.le(2, 3))
    assert(!sut.le(3, 2))
    assert(sut == ~sut)
  }

  def testUnderlyingSet {
    val sut = PoSet(Set("a", "b", "c"), (a: String, b: String) => a <= b)
    assert(sut.underlyingSet == Set("a", "b", "c"))
  }

  def testParser {
    val expected = PoSet(Set("a", "b", "c"), (a: String, b: String) => a <= b)
    val actual: PoSet[String] = PoSet("( { a, b, c},  { a <= b, b <= c, a <= c})")
    assert(3 == actual.size)
    println(actual)
    assert(actual.underlyingSet == Set("a", "b", "c"))
    assert(actual.le("a", "b"))
    assert(actual.le("b", "c"))
    assert(actual.le("a", "c"))
    assert(expected == actual)
  }

  def testRange {
    val sut = PoSet.range(0, 3, 1)
    assert(sut contains 2)
    assert(sut.le(1, 2))
    assert(sut.le(0, 2))
    assert(sut.le(0, 1))
  }
}