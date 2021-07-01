package math.sets

import org.specs2.mutable._
import testing.TestBase

class BigSetTest extends TestBase:

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
      val sut =
        BigSet.comprehension[String]((s: String) => s.length == 7 && s.charAt(1) == 'a', ".a.....")
      sut.size === Sets.InfiniteSize
      sut.hashCode * 0 === 0 // meaning, no exception thrown
      sut.toString === ".a....."
      sut.iterator must throwA[UnsupportedOperationException]
      sut.contains("caramba") === true
      sut.contains("carramba") === false
      sut.contains("policia") === false
    }
    
    "filter properly" in {
      val sut =
        BigSet.comprehension[String](
          (s: String) => s.length == 7 && s.charAt(1) == 'a',
          "Set of .a....."
        ) filter (_.head == 'b')
      sut.size === Sets.InfiniteSize
      sut.toString === "Set of .a....., filtered"
      sut.iterator must throwA[UnsupportedOperationException]
      sut.contains("bandana") === true
      sut.contains("caramba") === false
      sut.contains("carramba") === false
      sut.contains("policia") === false
    }

    "map properly" in {
      val flip = Functions.bijection[String, String]((_:String).reverse, (_:String).reverse)
      val sut =
        BigSet.comprehension[String]((s: String) => s.length == 7 && s.charAt(1) == 'a') map flip
      sut.size === Sets.InfiniteSize
      sut.toString === "Big Set with a predicate"
      sut.iterator must throwA[UnsupportedOperationException]
      sut.contains("Esteban") === true
      sut.contains("caramba") === false
      sut.contains("carramba") === false
      sut.contains("policia") === false
    }
  }
