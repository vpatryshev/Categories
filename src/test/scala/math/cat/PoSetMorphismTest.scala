package math.cat

import math.sets.PoSet
import org.specs2.mutable._
import scalakittens.Good
import math.Base._

/**
 * Test suite for PoSetMorphism class
 */
class PoSetMorphismTest extends Specification {
  private val intComparator = (x:Int, y:Int) => x <= y
  private val stringComparator = (x:String, y:String) => x <= y

  "PoSetMorphism" >> {

    "Constructor" >> {
      val x = PoSet(intComparator, 1, 2, 3, 4, 5)
      val y = PoSet(stringComparator, "#1", "#2", "#3", "#4", "#5")
      PoSetMorphism.build(x, y, (n: Int) => "#" + n) match {
        case Good(sut) =>
          sut(3) === "#3"
          sut.d0 === x
          sut.d1 === y
        case nogood => failure(nogood.toString)
      }
      ok
    }

    "Constructor_negative_badCodomain" >> {
      val x = PoSet(intComparator, 1, 2, 3, 4, 5)
      val y = PoSet(stringComparator, "#1", "#2", "#3", "#5")

      PoSetMorphism.build(x, y, (n: Int) => "#" + n) match {
        case Good(sut) => failure(s"expected an error, got $sut")
        case nogood => ok
      }
      ok
    }

    "Constructor_negative_lostOrder" >> {
      val x = PoSet(intComparator, 1, 2, 3, 4, 5)
      val y = PoSet(intComparator, 1, 2, 3, 4, 5)
      val sut = PoSetMorphism.build(x, y, (n: Int) => 1 + (n - 3) * (n - 3))
      sut.isBad === true
    }

    "Equals" >> {
      val x1 = PoSet(intComparator, 1, 2, 3, 4, 5)
      val y1 = PoSet(stringComparator, "#1", "#2", "#3", "#4", "#5")
      val x2 = PoSet(intComparator, 5, 4, 3, 2, 1)
      val y2 = PoSet(stringComparator, "#5", "#4", "#3", "#2", "#1")
      val sut1 = PoSetMorphism.build(x1, y1, (n: Int) => "#" + n)
      val sut2 = PoSetMorphism.build(x2, y2,
        Map(1 -> "#1", 2 -> "#2", 3 -> "#3", 4 -> "#4", 5 -> "#5"))
      sut1 === sut2
      val sut3 = PoSetMorphism.build(x2, y2, Map(1 -> "#1", 2 -> "#2", 3 -> "#3", 4 -> "#4", 5 -> "#4"))
      sut1 !== sut3
    }

    "Compose" >> {
      val fOpt = PoSetMorphism.build(PoSet(intComparator, 11, 12, 13, 14, 15), PoSet(intComparator, 1, 2, 3, 4, 5), (n: Int) => n - 10)
      val gOpt = PoSetMorphism.build(PoSet(intComparator, 1, 2, 3, 4, 5), PoSet(stringComparator, "#1", "#2", "#3", "#4", "#5"), (n: Int) => "#" + n)
      
      fOpt andAlso gOpt match {
        case Good((f, g)) =>
          val h = f compose g
          h === PoSetMorphism.build(PoSet(intComparator, 11, 12, 13, 14, 15), PoSet(stringComparator, "#1", "#2", "#3", "#4", "#5"), (n: Int) => "#" + (n - 10)).iHope
        case none => failure(s"failed to compose: $none")
      }
      ok
    }

    "id" >> {
      val s = PoSet(stringComparator, "1", "haha", "2.71828")
      val sut = PoSetMorphism.id(s)
      (PoSet(stringComparator, "2.71828", "1", "haha") == sut.d0) must beTrue
      (PoSet(stringComparator, "2.71828", "1", "haha") == sut.d1) must beTrue
      sut("haha") === "haha"
    }

    "Const" >> {
      val strings = PoSet(stringComparator, "a", "b", "c")
      val ints = PoSet(intComparator, 1, 2, 3)
      val sut = PoSetMorphism.const(strings, ints, 2)
      sut.d0 === strings
      sut.d1 === ints
      sut("a") === 2
    }

    "Range" >> {
      val sut = PoSet.range(2, 11, 3)
      sut must contain(8)
      sut must not contain 11
      sut.le(5, 8) must beTrue
    }
  }
}
