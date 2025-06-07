package math.sets

import org.specs2.mutable._
import testing.TestBase

class BigSetTest extends TestBase:

  "BigSet" should :

    "get created from set" in :
      val sut = BigSet(Set("abc", "def"))
      sut.iterator.toList === "abc" :: "def" :: Nil
      sut.size must be_==(2)
      sut.toString === "Set(abc, def)"
      BigSet(Set.empty[System]).isEmpty must beTrue

    "get created from predicate" in :
      val sut =
        BigSet.comprehension[String]((s: String) => s.length == 7 && s.charAt(1) == 'a', ".a.....")
      sut.size must be_==(Sets.InfiniteSize)
      sut.hashCode * 0 must be_==(0) // meaning, no exception thrown
      sut.toString === ".a....."
      sut.iterator must throwA[UnsupportedOperationException]
      sut.contains("caramba") must beTrue
      sut.contains("carramba") must beFalse
      sut.contains("policia") must beFalse

    "filter properly" in :
      val sut =
        BigSet.comprehension[String](
          (s: String) => s.length == 7 && s.charAt(1) == 'a',
          "Set of .a....."
        ) filter (_.head == 'b')
      sut.size must be_==(Sets.InfiniteSize)
      sut.toString === "Set of .a....., filtered"
      sut.iterator must throwA[UnsupportedOperationException]
      sut.contains("bandana") must beTrue
      sut.contains("caramba") must beFalse
      sut.contains("carramba") must beFalse
      sut.contains("policia") must beFalse

    "map properly" in :
      val flip = Functions.bijection[String, String]((_:String).reverse, (_:String).reverse)
      val sut =
        BigSet.comprehension[String]((s: String) => s.length == 7 && s.charAt(1) == 'a') map flip
      sut.size must be_==(Sets.InfiniteSize)
      sut.toString === "Big Set with a predicate"
      sut.iterator must throwA[UnsupportedOperationException]
      sut.contains("Esteban") must beTrue
      sut.contains("caramba") must beFalse
      sut.contains("carramba") must beFalse
      sut.contains("policia") must beFalse
