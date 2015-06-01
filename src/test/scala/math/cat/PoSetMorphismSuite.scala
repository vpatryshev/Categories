package math.cat

import org.scalatest.junit.JUnit3Suite
import org.scalatest.prop.Checkers
//import org.scalacheck.Prop.forAll
//import math.cat.PoSet._
//

/**
 * Test suite for PoSetMorphism class
 * @author vpatryshev
 */
class PoSetMorphismSuite extends JUnit3Suite with Checkers {
  val intComparator = (x:Int, y:Int) => x <= y
  val stringComparator = (x:String, y:String) => x <= y

  def testConstructor {
    val x = PoSet(intComparator, 1, 2, 3, 4, 5)
    val y = PoSet(stringComparator, "#1", "#2", "#3", "#4", "#5")
    val sut = PoSetMorphism(x, y, (n: Int) => "#" + n)
    assert("#3" == sut(3))
    assert(x == sut.d0)
    assert(y == sut.d1)
  }

  def testConstructor_negative_badCodomain {
    val x = PoSet(intComparator, 1, 2, 3, 4, 5)
    val y = PoSet(stringComparator, "#1", "#2", "#3", "#5")

    try {
      PoSetMorphism(x, y, (n: Int) => "#" + n)
      fail("Validator should have thrown an exception")
    } catch {
      case e: IllegalArgumentException => print(e)// as expected
    }
  }

  def testConstructor_negative_lostOrder {
    val x = PoSet(intComparator, 1, 2, 3, 4, 5)
    val y = PoSet(intComparator, 1, 2, 3, 4, 5)

    try {
      PoSetMorphism(x, y, (n: Int) => 1 + (n - 3) * (n - 3))
      fail("Validator should have thrown an exception")
    } catch {
      case e: IllegalArgumentException => print(e)// as expected
    }
  }

  def testEquals {
    val x1 = PoSet(intComparator, 1, 2, 3, 4, 5)
    val y1 = PoSet(stringComparator, "#1", "#2", "#3", "#4", "#5")
    val x2 = PoSet(intComparator, 5, 4, 3, 2, 1)
    val y2 = PoSet(stringComparator, "#5", "#4", "#3", "#2", "#1")
    val sut1 = PoSetMorphism(x1, y1, (n: Int) => "#" + n)
    val sut2 = PoSetMorphism(x2, y2,
      Map(1 -> "#1", 2 -> "#2", 3 -> "#3", 4 -> "#4", 5 -> "#5"))
    assert(sut1 == sut2)
    val sut3 = PoSetMorphism(x2, y2, Map(1 -> "#1", 2 -> "#2", 3 -> "#3", 4 -> "#4", 5 -> "#4"))
    assert(sut1 != sut3)
  }

  def testCompose {
    val f = PoSetMorphism(PoSet(intComparator, 11, 12, 13, 14, 15), PoSet(intComparator, 1, 2, 3, 4, 5), (n: Int) => n - 10)
    val g = PoSetMorphism(PoSet(intComparator, 1, 2, 3, 4, 5), PoSet(stringComparator, "#1", "#2", "#3", "#4", "#5"), (n: Int) => "#" + n)
    val h = f compose g;
    assert(h == PoSetMorphism(PoSet(intComparator, 11, 12, 13, 14, 15), PoSet(stringComparator, "#1", "#2", "#3", "#4", "#5"), (n: Int) => "#" + (n - 10)))
  }

  def testUnit {
    val set = PoSet(stringComparator, "1", "haha", "2.71828")
    val sut = PoSetMorphism.unit(set)
    assert(PoSet(stringComparator, "2.71828", "1", "haha") == sut.d0)
    assert(PoSet(stringComparator, "2.71828", "1", "haha") == sut.d1)
    assert("haha" == sut("haha"))
  }

  def testConst {
    val strings = PoSet(stringComparator, "a", "b", "c")
    val ints = PoSet(intComparator, 1, 2, 3)
    val sut = PoSetMorphism.const(strings, ints, 2)
    assert(strings == sut.d0)
    assert(ints == sut.d1)
    assert(2 == sut("a"))
  }

  def testRange {
    val sut = PoSet.range(2, 11, 3)
    assert(sut.contains(8))
    assert(!sut.contains(11))
    assert(sut.le(5, 8))
  }
}
