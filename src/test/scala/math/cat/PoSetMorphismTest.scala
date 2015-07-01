package math.cat

import org.specs2.mutable._

/**
 * Test suite for PoSetMorphism class
 * @author vpatryshev
 */
class PoSetMorphismTest extends Specification {
  val intComparator = (x:Int, y:Int) => x <= y
  val stringComparator = (x:String, y:String) => x <= y

  "PoSetMorphism" >> {

    "Constructor" >> {
      val x = PoSet(intComparator, 1, 2, 3, 4, 5)
      val y = PoSet(stringComparator, "#1", "#2", "#3", "#4", "#5")
      val sut = PoSetMorphism(x, y, (n: Int) => "#" + n)
      ("#3" == sut(3)) must beTrue
      (x == sut.d0) must beTrue
      (y == sut.d1) must beTrue
    }

    "Constructor_negative_badCodomain" >> {
      val x = PoSet(intComparator, 1, 2, 3, 4, 5)
      val y = PoSet(stringComparator, "#1", "#2", "#3", "#5")

      try {
        PoSetMorphism(x, y, (n: Int) => "#" + n)
        failure("Validator should have thrown an exception")
      } catch {
        case e: IllegalArgumentException => print(e) // as expected
      }
      true
    }

    "Constructor_negative_lostOrder" >> {
      val x = PoSet(intComparator, 1, 2, 3, 4, 5)
      val y = PoSet(intComparator, 1, 2, 3, 4, 5)

      try {
        PoSetMorphism(x, y, (n: Int) => 1 + (n - 3) * (n - 3))
        failure("Validator should have thrown an exception")
      } catch {
        case e: IllegalArgumentException => print(e) // as expected
      }
      true
    }

    "Equals" >> {
      val x1 = PoSet(intComparator, 1, 2, 3, 4, 5)
      val y1 = PoSet(stringComparator, "#1", "#2", "#3", "#4", "#5")
      val x2 = PoSet(intComparator, 5, 4, 3, 2, 1)
      val y2 = PoSet(stringComparator, "#5", "#4", "#3", "#2", "#1")
      val sut1 = PoSetMorphism(x1, y1, (n: Int) => "#" + n)
      val sut2 = PoSetMorphism(x2, y2,
        Map(1 -> "#1", 2 -> "#2", 3 -> "#3", 4 -> "#4", 5 -> "#5"))
      (sut1 == sut2) must beTrue
      val sut3 = PoSetMorphism(x2, y2, Map(1 -> "#1", 2 -> "#2", 3 -> "#3", 4 -> "#4", 5 -> "#4"))
      (sut1 != sut3) must beTrue
    }

    "Compose" >> {
      val f = PoSetMorphism(PoSet(intComparator, 11, 12, 13, 14, 15), PoSet(intComparator, 1, 2, 3, 4, 5), (n: Int) => n - 10)
      val g = PoSetMorphism(PoSet(intComparator, 1, 2, 3, 4, 5), PoSet(stringComparator, "#1", "#2", "#3", "#4", "#5"), (n: Int) => "#" + n)
      val h = f compose g
      (h == PoSetMorphism(PoSet(intComparator, 11, 12, 13, 14, 15), PoSet(stringComparator, "#1", "#2", "#3", "#4", "#5"), (n: Int) => "#" + (n - 10))) must beTrue
    }

    "Unit" >> {
      val set = PoSet(stringComparator, "1", "haha", "2.71828")
      val sut = PoSetMorphism.unit(set)
      (PoSet(stringComparator, "2.71828", "1", "haha") == sut.d0) must beTrue
      (PoSet(stringComparator, "2.71828", "1", "haha") == sut.d1) must beTrue
      ("haha" == sut("haha")) must beTrue
    }

    "Const" >> {
      val strings = PoSet(stringComparator, "a", "b", "c")
      val ints = PoSet(intComparator, 1, 2, 3)
      val sut = PoSetMorphism.const(strings, ints, 2)
      (strings == sut.d0) must beTrue
      (ints == sut.d1) must beTrue
      (2 == sut("a")) must beTrue
    }

    "Range" >> {
      val sut = PoSet.range(2, 11, 3)
      (sut.contains(8)) must beTrue
      (!sut.contains(11)) must beTrue
      (sut.le(5, 8)) must beTrue
    }
  }
}
