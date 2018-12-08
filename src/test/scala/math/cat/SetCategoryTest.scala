package math.cat

import org.specs2.mutable._
import SetCategory._
import SetFunction._
import math.sets.Sets.Untyped
import math.sets.{BigSet, Sets}

/**
  * Prototype for all tests
  */
class SetCategoryTest extends Specification {
  val s1: Untyped = Set[Int](42).map(identity)
  val s2: Untyped = Sets.range(0, 7, 1).map(identity)
  val s3: Untyped = Set("hello", "cruel", "world").map(identity)
  val s4: Untyped = Set("hello", "goodbye", "cruel", "world").map(identity)

  "SetCategory" should {
    "buildGraph" in {
      val sets = BigSet(Set(s1, s2))
      val arrow = SetFunction("sample", s1, s2, _.asInstanceOf[Int] / 7)
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

    def contains[T](x: T)(s: Any): Boolean = s match {
      case us: Set[T] => us(x)
      case _ => false
    }

    def whereIn(set: Untyped)(point: Any): Any =
      set find contains(point) getOrElse Sets.Empty


    "produce coequalizer of two" in {
      val f = SetFunction("f", s1, s2, _ => 3)
      val g = SetFunction("g", s1, s2, _ => 5)
      val actual = Setf.coequalizer(f, g)
      val expectedSet: Untyped = Set(Set(0), Set(1), Set(2), Set(3, 5), Set(4), Set(6))

      def where(x: Any): Any = whereIn(expectedSet)(x)

      val expected = SetFunction("Factorset", s2, expectedSet, where)
      val same = actual contains expected
      actual === Some(expected)

      Setf.coequalizer(f, id(s3)) should throwA[IllegalArgumentException]
      Setf.coequalizer(f, id(s2)) should throwA[IllegalArgumentException]
      Setf.coequalizer(f, id(s1)) should throwA[IllegalArgumentException]
    }

    "produce coequalizer of n" in {
      val f = SetFunction("f", s1, s2, _ => 3)
      val g = SetFunction("g", s1, s2, _ => 5)
      val h = SetFunction("h", s1, s2, _ => 1)
      val actual = Setf.coequalizer(f :: g :: h :: Nil)
      val expectedSet: Untyped = Set(Set(0), Set(2), Set(1, 3, 5), Set(4), Set(6))

      def where(x: Any): Any = whereIn(expectedSet)(x)

      val expected = SetFunction("Factorset", s2, expectedSet, where)
      val same = actual contains expected
      actual === Some(expected)

      def singleton(x: Any): Any = Set(x)

      val singletons: Untyped = s2 map singleton
      Setf.coequalizer(f :: Nil) === Some(SetFunction("Factorset", s2, singletons, singleton))
      Setf.coequalizer(List.empty[SetFunction]) should throwA[IllegalArgumentException]
      Setf.coequalizer(f :: id(s3) :: Nil) should throwA[IllegalArgumentException]
      Setf.coequalizer(f :: id(s2) :: Nil) should throwA[IllegalArgumentException]
      Setf.coequalizer(f :: id(s1) :: Nil) should throwA[IllegalArgumentException]
      Setf.coequalizer(f :: id(s3) :: Nil) should throwA[IllegalArgumentException]
      Setf.coequalizer(f :: id(s2) :: Nil) should throwA[IllegalArgumentException]
      Setf.coequalizer(f :: id(s1) :: Nil) should throwA[IllegalArgumentException]
    }

    "produce 0th degree of an object" in {
      Setf.degree(Set(1, 2, 3), 0).size === 1
      Setf.degree(Set.empty[Any], 0).size === 1
    }

    "produce 1st degree of an object" in {
      Setf.degree(Set(1, 2, 3), 1).map(_._1) === Some(Set(Map(0 -> 1), Map(0 -> 2), Map(0 -> 3)))
      val maybeZeroOne = Setf.degree(Set.empty[Any], 1).map(_._1)
      maybeZeroOne === Some(Set.empty[Any])
    }

    "produce 2nd degree of an object" in {
      Setf.degree(Set(1, 2, 3), 2).map(_._1) ===
        Some(Set(
          Map(1 -> 1, 0 -> 1),
          Map(1 -> 1, 0 -> 2),
          Map(1 -> 1, 0 -> 3),
          Map(1 -> 2, 0 -> 1),
          Map(1 -> 2, 0 -> 2),
          Map(1 -> 2, 0 -> 3),
          Map(1 -> 3, 0 -> 1),
          Map(1 -> 3, 0 -> 2),
          Map(1 -> 3, 0 -> 3))
        )
    }

    "produce 4th degree of an object" in {
      val s: Untyped = Sets.setOf(1, 2, 3)
      val sut = Setf.degree(s, 4).map(_._1)

      sut match {
        case Some(set: Set[Any]) =>
          set.size === 81
          for {
            a <- s
            b <- s
            c <- s
            d <- s
          } {
            val point = Map(0 -> a, 1 -> b, 2 -> c, 3 -> d)
            set(point) === true
          }
        case None => failure("must have built a 4th degree")
      }
      ok
    }
    
    "produce no equalizer if it's not in the category" in {
      // the category of even-sized sets
      val sut = new SetCategory(BigSet(_.size % 2 == 0))
      val f = SetFunction("f", s2, s4, n => if (n.toString.toInt < 3) "hello" else "goodbye")
      val g = SetFunction("g", s2, s4, n => if (n.toString.toInt < 1) "hello" else "goodbye")
      val eq = sut.equalizer(f, g)
      eq === None
    }

    "produce an empty equalizer if it exists" in {
      // the category of even-sized sets
      val sut = new SetCategory(BigSet(_.size % 2 == 0))
      val f = SetFunction("f", s2, s4, n => if (n.toString.toInt < 3) "hello" else "goodbye")
      val g = SetFunction("g", s2, s4, n => if (n.toString.toInt < 4) "cruel" else "world")
      sut.equalizer(f, g) match {
        case Some(inclusion) =>
          inclusion.d0 === Sets.Empty
          inclusion.d1 === s2
        case None => failure("Expected some equalizer")
      }
      ok
    }

    "produce an equalizer" in {
      // the category of even-sized sets
      val sut = Setf
      val f = SetFunction("f", s2, s4,
        n => if (n.toString.toInt < 3) "hello" else "goodbye")
      val g = SetFunction("g", s2, s4,
        n => if (n.toString.toInt < 1) "cruel" else if (n.toString.toInt < 5) "hello" else "goodbye")
      sut.equalizer(f, g) match {
        case Some(eq) =>
          eq.d0 === Set(1, 2, 5, 6)
          eq.d1 === s2
        case None => failure("Expected an equalizer")
      }
      ok
    }
  }
}
