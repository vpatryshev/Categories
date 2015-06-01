package math.cat




import org.scalatest.junit.JUnit3Suite
import Sets._

/**
 * Test suite for SetMorphism class
 * @author vpatryshev
 */
class SetMorphismSuite extends JUnit3Suite with Assert {

  def testConstructor {
    val x = Set(1, 2, 3, 4, 5)
    val y = Set("#1", "#2", "#3", "#4", "#5")
    val sut = SetMorphism(x, y, (n: Int) => "#" + n)
    assert("#3" == sut(3))
    assert(x == sut.d0)
    assert(y == sut.d1)
  }

  def testConstructor_negative {
    val x = Set(1, 2, 3, 4, 5)
    val y = Set("#1", "#2", "#3", "#5")

    try {
      SetMorphism(x, y, (n: Int) => "#" + n)
      fail("Validator should have thrown an exception")
    } catch {
      case e: IllegalArgumentException => // as expected
    }
  }

  def testEquals {
    val sut1 = SetMorphism(Set(1, 2, 3, 4, 5), Set("#1", "#2", "#3", "#4", "#5"), (n: Int) => "#" + n)
    val sut2 = SetMorphism(Set(5, 4, 3, 2, 1), Set("#5", "#4", "#3", "#2", "#1"),
                           Map(1 -> "#1", 2 -> "#2", 3 -> "#3", 4 -> "#4", 5 -> "#5"))
    assert(sut1 == sut2)
    val sut3 = SetMorphism(Set(5, 4, 3, 2, 1), Set("#5", "#4", "#3", "#2", "#1"),
                           Map(1 -> "#1", 2 -> "#2", 3 -> "#3", 4 -> "#4", 5 -> "#3"))
    assert(sut1 != sut3)
  }

  def testCompose {
    val f = SetMorphism(Set(11, 12, 13, 14, 15), Set(1, 2, 3, 4, 5), (n: Int) => n - 10)
    val g = SetMorphism(Set(1, 2, 3, 4, 5), Set("#1", "#2", "#3", "#4", "#5"), (n: Int) => "#" + n)
    val h = f compose g;
    assert(h == SetMorphism(Set(11, 12, 13, 14, 15), Set("#1", "#2", "#3", "#4", "#5"), (n: Int) => "#" + (n - 10)))
  }

  def testRevert {
    val sut1 = SetMorphism(Set("abc", "x", "def"), Set(1, 2, 3, 4, 5), (s:String) => s.length)
    val sut2 = SetMorphism(Set(5, 4, 3, 2, 1), Set(Set("abc", "def"), Set("x"), Set()),
                           Map(1 -> Set("x"), 2 -> Set(), 3 -> Set("abc", "def"), 4 -> Set(), 5 -> Set()))
    assert(sut2 == sut1.revert)
  }

  def testUnit {
    val set = Set(1, "haha", 2.71828)
    val sut = SetMorphism.unit(set)
    assert(Set(2.71828, 1, "haha") == sut.d0)
    assert(Set(2.71828, 1, "haha") == sut.d1)
    assert("haha" == sut("haha"))
  }

  def testConst {
    val strings = Set("a", "b", "c")
    val ints = Set(1, 2, 3)
    val two:Int = 2
    val sut = SetMorphism.const(strings, ints, two)
    assert(strings == sut.d0)
    assert(ints == sut.d1)
    assert(2 == sut("a"))
  }

  def testHom {
    val d0 = Set("1", "2")
    val d1 = Set("x", "y", "z")
    assert(d0 != d1)
    val actual = SetMorphism.hom(d0, d1)
    assert(actual.size == 9)
    assert(actual contains SetMorphism(d0, d1, Map("1" -> "y", "2" -> "x")))
  }

  def testVariance_byX {
    val x0 = Set(1, 2, 3, 4, 5)
    val x = Sets.setOf(x0, x0.size, x0.contains _)
    val y = Set("#1", "#2", "#3", "#4", "#5")
    val sut = SetMorphism(x, y, (n: Int) => "#" + n)
    assert("#3" == sut(3))
  }

  def testVariance_byY {
    val x = Set(1, 2, 3, 4, 5)
    val y0 = Set("#1", "#2", "#3", "#4", "#5")
    val y = Sets.setOf(y0, y0.size, y0.contains _)
    val sut = SetMorphism(x, y, (n: Int) => "#" + n)
    assert("#3" == sut(3))
  }

  def testProduct {
    val x = Set(0,1,2,3,4,5)
    val y = Set(0,1,2)
    val sut = SetMorphism(x, y, (x:Int) => x % 3).product
    def m(x:Int,y:Int,z:Int) = Map(0 -> x, 1 -> y, 2 -> z)
    assert(sut == Set(m(0,1,2), m(0,1,5),
                      m(0,4,2), m(0,4,5),
                      m(3,1,2), m(3,1,5),
                      m(3,4,2), m(3,4,5)), " got " + sut)
  }

  def testPullback {
    val xs = Set(1,2,3,4,5)
    val ys = Set(2,4,6,8,10)
    val f = SetMorphism("f", xs, N, (x:Int) => x/2)
    val g = SetMorphism("g", ys, N, (y:Int) => y/5)
    val (left, right) = SetMorphism.pullback(f, g)
    val pullbackSet = Set((1,2),(1,4),(2,6),(2,8),(3,6),(3,8),(4,10),(5,10))
    for (p <- pullbackSet) {
      assertEquals(p._1, left(p))
      assertEquals(p._2, right(p))
    }
  }
}
