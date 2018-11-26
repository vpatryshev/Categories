package math.cat

import org.specs2.mutable._
import SetCategory._
import SetFunction._
import math.sets.{BigSet, Sets}

/**
  * Prototype for all tests
  */
class SetCategoryTest extends Specification {
  val s1: Set[Any] = Set[Int](42).map(identity)
  val s2: Set[Any] = Sets.range(0, 7, 1).map(identity)
  val s3: Set[Any] = Set("hello", "cruel", "world").map(identity)
  
  "SetCategory" should {
    "buildGraph" in {
      val sets = BigSet(Set(s1, s2))
      val arrow = SetFunction("sample", s1, s2, _.asInstanceOf[Int]/7)
      val theGraph = graphOfSets(sets)
      theGraph.nodes === sets
      theGraph.arrows.contains(arrow) === true
    }
    
    "produce no coequalizer if category is too small" in {
      val f = SetFunction("f", s1, s2, _ => 3)
      val g = SetFunction("g", s1, s2, _ => 5)
      new SetCategory(BigSet(Set(s1, s2, s3))).coequalizer(f, g) === None
      val c2 = new SetCategory(BigSet(_.size % 2 == 0))
      val v2 = c2.coequalizer(f, g)
      v2 === None
    }
    
    "produce coequalizer of two" in {
      val f = SetFunction("f", s1, s2, _ => 3)
      val g = SetFunction("g", s1, s2, _ => 5)
      val actual = Setf.coequalizer(f, g)
      val expectedSet: Set[Any] = Set(Set(0), Set(1), Set(2), Set(3,5), Set(4), Set(6))
      def where(x: Any): Any = expectedSet.find({ case s: Set[Any] => s contains x}).getOrElse(Set.empty[Any])
      val expected = SetFunction("Factorset", s2, expectedSet, where)
      val same = actual contains expected
      actual === Some(expected)

      Setf.coequalizer(f, id(s3)) should throwA[IllegalArgumentException]
      Setf.coequalizer(f, id(s2)) should throwA[IllegalArgumentException]
      Setf.coequalizer(f, id(s1)) should throwA[IllegalArgumentException]
    }
  }

  "produce coequalizer of n" in {
    val f = SetFunction("f", s1, s2, _ => 3)
    val g = SetFunction("g", s1, s2, _ => 5)
    val h = SetFunction("h", s1, s2, _ => 1)
    val actual = Setf.coequalizer(f::g::h::Nil)
    val expectedSet: Set[Any] = Set(Set(0),  Set(2), Set(1,3,5), Set(4), Set(6))
    def where(x: Any): Any = expectedSet.find({ case s: Set[Any] => s contains x}).getOrElse(Set.empty[Any])
    val expected = SetFunction("Factorset", s2, expectedSet, where)
    val same = actual contains expected
    actual === Some(expected)

    def singleton(x: Any): Any = Set(x)
    val singletons: Set[Any] = s2 map singleton
    Setf.coequalizer(f::Nil) === Some(SetFunction("Factorset", s2, singletons, singleton))
    Setf.coequalizer(List.empty[SetFunction]) should throwA[IllegalArgumentException]
    Setf.coequalizer(f::id(s3)::Nil) should throwA[IllegalArgumentException]
    Setf.coequalizer(f::id(s2)::Nil) should throwA[IllegalArgumentException]
    Setf.coequalizer(f::id(s1)::Nil) should throwA[IllegalArgumentException]
    Setf.coequalizer(f::id(s3)::Nil) should throwA[IllegalArgumentException]
    Setf.coequalizer(f::id(s2)::Nil) should throwA[IllegalArgumentException]
    Setf.coequalizer(f::id(s1)::Nil) should throwA[IllegalArgumentException]
  }
  
//  "produce degree of 0" in {
//    Setf === null
//  }
}
