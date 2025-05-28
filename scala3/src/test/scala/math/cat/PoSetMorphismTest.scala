package math.cat

import math.Test
import math.cat.SetMorphism.*
import math.sets.Sets.*
import math.sets.{N, PoSet, Sets}
import org.specs2.execute.Result as MatchResult
import org.specs2.mutable.*
import scalakittens.*

import scala.language.implicitConversions

/**
 * Test suite for PoSetMorphism class
 */
class PoSetMorphismTest extends Test:
  private val intComparator = (x:Int, y:Int) => x <= y
  private val stringComparator = (x:String, y:String) => x <= y

  "PoSetMorphism" should :

    "Constructor" in :
      val x = PoSet(intComparator, 1, 2, 3, 4, 5)
      val y = PoSet(stringComparator, "#1", "#2", "#3", "#4", "#5")
      
      PoSetMorphism.build(x, y, (n: Int) => s"#$n") match
        case Good(sut) =>
          sut(3) === "#3"
          sut.d0 must be_==(x)
          sut.d1 must be_==(y)
        case nogood => failure(nogood.toString)

      ok

    "Constructor_negative_badCodomain" in :
      val x = PoSet(intComparator, 1, 2, 3, 4, 5)
      val y = PoSet(stringComparator, "#1", "#2", "#3", "#5")

      PoSetMorphism.build(x, y, (n: Int) => s"#$n") match
        case Good(sut) => failure(s"expected an error, got $sut")
        case nogood => ok

      ok

    "Constructor_negative_lostOrder" in :
      val x = PoSet(intComparator, 1, 2, 3, 4, 5)
      val y = PoSet(intComparator, 1, 2, 3, 4, 5)
      val sut = PoSetMorphism.build(x, y, (n: Int) => 1 + (n - 3) * (n - 3))
      sut.isBad must beTrue

    "Equals" in :
      val x1 = PoSet(intComparator, 1, 2, 3, 4, 5)
      val y1 = PoSet(stringComparator, "#1", "#2", "#3", "#4", "#5")
      val x2 = PoSet(intComparator, 5, 4, 3, 2, 1)
      val y2 = PoSet(stringComparator, "#5", "#4", "#3", "#2", "#1")
      val sut1 = PoSetMorphism.build(x1, y1, (n: Int) => "#" + n)
      val sut2 = PoSetMorphism.build(x2, y2,
        Map(1 -> "#1", 2 -> "#2", 3 -> "#3", 4 -> "#4", 5 -> "#5"))
      sut1 must be_==(sut2)
      val sut3 = PoSetMorphism.build(x2, y2, Map(1 -> "#1", 2 -> "#2", 3 -> "#3", 4 -> "#4", 5 -> "#4"))
      sut1 !== sut3

    "Compose" in :
      val fOpt = PoSetMorphism.build(PoSet(intComparator, 11, 12, 13, 14, 15), PoSet(intComparator, 1, 2, 3, 4, 5), (n: Int) => n - 10)
      val gOpt = PoSetMorphism.build(PoSet(intComparator, 1, 2, 3, 4, 5), PoSet(stringComparator, "#1", "#2", "#3", "#4", "#5"), (n: Int) => "#" + n)
      
      fOpt andAlso gOpt match
        case Good((f, g)) =>
          val h = f andThen g
          h must be_==
            (PoSetMorphism.build(PoSet(intComparator, 11, 12, 13, 14, 15), PoSet(stringComparator, "#1", "#2", "#3", "#4", "#5"), (n: Int) => "#" + (n - 10)).asOption)
        case none => failure(s"failed to compose: $none")

      ok

    "id" in :
      val s = PoSet(stringComparator, "1", "haha", "2.71828")
      val sut = PoSetMorphism.id(s)
      sut.d0 must be_==(PoSet(stringComparator, "2.71828", "1", "haha"))
      sut.d1 must be_==(PoSet(stringComparator, "2.71828", "1", "haha"))
      val actual = sut("haha")
      actual must be_==("haha")

    "Range" in :
      val sut = PoSet.range(2, 11, 3)
      sut must contain(8)
      sut must be_==(Set(2, 3, 8))
      val actual: Boolean = sut.le(5, 8)
      actual must beTrue

    "Const" in :
      val strings = PoSet(stringComparator, "a", "b", "c")
      val ints = PoSet(intComparator, 1, 2, 3)
      val sut = PoSetMorphism.const(strings, ints, 2)
      sut.d0 must be_==(strings)
      sut.d1 must be_==(ints)
      sut("a") === 2
