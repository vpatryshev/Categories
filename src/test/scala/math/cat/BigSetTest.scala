package math.cat

import java.util.Date

import j.math.cat.Functions.Bijection
import math.cat.Base._
import math.cat.Functions.Bijection
import org.specs2.matcher.MatchResult
import org.specs2.mutable._

class BigSetTest extends Specification {

  "BigSet" should {

    "get created from set" in {
      val sut = BigSet(Set("abc", "def"))
      sut.iterator.toList === "abc" :: "def" :: Nil
      sut.size === 2
      sut.toString === "Set(abc, def)"
      sut.hashCode == 42
      BigSet(Set.empty[System]).isEmpty === true
    }

    "get created from predicate" in {
      val sut = BigSet[String]((s: String) => s.length == 7 && s.charAt(1) == 'a')
      sut.size === Sets.InfiniteSize
      sut.hashCode * 0 === 0 // meaning, no exception thrown
      sut.toString === "A BIG SET"
      sut.iterator must throwA[UnsupportedOperationException]
      sut.contains("caramba") === true
      sut.contains("carramba") === false
      sut.contains("policia") === false
    }
    
    "filter properly" in {
      val sut =
        BigSet[String]((s: String) => s.length == 7 && s.charAt(1) == 'a') filter (_.head == 'b')
      sut.size === Sets.InfiniteSize
      sut.toString === "A BIG SET"
      sut.iterator must throwA[UnsupportedOperationException]
      sut.contains("bandana") === true
      sut.contains("caramba") === false
      sut.contains("carramba") === false
      sut.contains("policia") === false
    }

    "map properly" in {
      val iso = Functions.Bijection[String, String]((_:String).reverse, (_:String).reverse)
      val sut =
        BigSet[String]((s: String) => s.length == 7 && s.charAt(1) == 'a') map iso
      sut.size === Sets.InfiniteSize
      sut.toString === "A BIG SET"
      sut.iterator must throwA[UnsupportedOperationException]
      sut.contains("Esteban") === true
      sut.contains("caramba") === false
      sut.contains("carramba") === false
      sut.contains("policia") === false
    }
  }
}
