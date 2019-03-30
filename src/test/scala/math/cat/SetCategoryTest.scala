package math.cat

import math.cat.SetCategory._
import math.cat.SetFunction._
import math.sets.Sets._
import math.sets.{BigSet, Sets}
import org.specs2.mutable._


/**
  * Set Category tests
  */
class SetCategoryTest extends Specification {
  val s1: set = Set[Int](42) untyped
  val s2: set = Sets.range(0, 7, 1) untyped
  val s3: set = Set("hello", "cruel", "world") untyped
  val s4: set = Set("hello", "goodbye", "cruel", "world") untyped
  private val allEvenSets: BigSet[Set[Any]] = BigSet.comprehension[Set[Any]](_.size % 2 == 0)
  val evenSets: SetCategory = new SetCategory(allEvenSets)
  val oddSets: SetCategory = new SetCategory(BigSet.comprehension[Set[Any]](_.size % 2 == 1))

  "SetCategory" should {
    "buildGraph" in {
      val sets = BigSet(Set(s1, s2))
      val arrow = SetFunction("sample", s1, s2, _.asInstanceOf[Int] / 7)
      val theGraph = graphOfSets(sets)
      import theGraph._
      theGraph.nodes === sets
      theGraph.arrows.contains(theGraph.arrow(arrow)) === true
    }

    "produce no coequalizer if category is too small" in {
      val f = SetFunction("f", s1, s2, _ => 3)
      val g = SetFunction("g", s1, s2, _ => 5)
      new SetCategory(BigSet(Set(s1, s2, s3))).coequalizer(f, g) === None
      val v2 = evenSets.coequalizer(f, g)
      v2 === None
    }

    def contains[T](x: T)(s: Any): Boolean = s match {
      case us: Set[T] => us(x)
      case _ => false
    }

    def whereIn(s: set)(point: Any): Any =
      s find contains(point) getOrElse Sets.Empty


    "produce coequalizer of two" in {
      val f = SetFunction("f", s1, s2, _ => 3)
      val g = SetFunction("g", s1, s2, _ => 5)
      val actual = Setf.coequalizer(f, g)
      val expectedSet: set = Set(Set(0), Set(1), Set(2), Set(3, 5), Set(4), Set(6))

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
      val expectedSet: set = Set(Set(0), Set(2), Set(1, 3, 5), Set(4), Set(6))

      def where(x: Any): Any = whereIn(expectedSet)(x)

      val expected = SetFunction("Factorset", s2, expectedSet, where)
      val same = actual contains expected
      actual === Some(expected)

      val singletons: set = s2 map singleton
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
      val source: set = Sets.setOf(1, 2, 3)
      val sut = Setf.degree(source, 4).map(_._1)

      sut match {
        case Some(s: set) =>
          s.size === 81
          for {
            a <- source
            b <- source
            c <- source
            d <- source
          } {
            val point = Map(0 -> a, 1 -> b, 2 -> c, 3 -> d)
            s(point) === true
          }
        case None => failure("must have built a 4th degree")
      }
      ok
    }

    "produce no equalizer if it's not in the category" in {
      // the category of even-sized sets
      val f = SetFunction("f", s2, s4, n => if (n.toString.toInt < 3) "hello" else "goodbye")
      val g = SetFunction("g", s2, s4, n => if (n.toString.toInt < 1) "hello" else "goodbye")
      val eq = evenSets.equalizer(f, g)
      eq === None
    }

    "produce an empty equalizer if it exists" in {
      val f = SetFunction("f", s2, s4, n => if (n.toString.toInt < 3) "hello" else "goodbye")
      val g = SetFunction("g", s2, s4, n => if (n.toString.toInt < 4) "cruel" else "world")
      evenSets.equalizer(f, g) match {
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

    "have an id" in {
      for {
        obj <- s4
      } Setf.id(s4)(obj) === obj

      ok
    }

    "have a 0" in {
      Setf.initial === Some(Sets.Empty)
      evenSets.initial === Some(Sets.Empty)
      oddSets.initial === None
    }

    "check for epi" in {
      val sut1 = SetFunction("inclusion", s3, s4, x => x)
      Setf.isEpimorphism(sut1) === false
      val sut2 = SetFunction("covering", s4, s3,
        {
          case "goodbye" => "hello"
          case x => x
        })
      Setf.isEpimorphism(sut2) === true
      Setf.isEpimorphism(Setf.id(s4)) === true
    }

    "check for mono" in {
      val sut1 = SetFunction("inclusion", s3, s4, x => x)
      Setf.isMonomorphism(sut1) === true
      val sut2 = SetFunction("covering", s4, s3,
        {
          case "goodbye" => "hello"
          case x => x
        })
      Setf.isMonomorphism(sut2) === false
      Setf.isMonomorphism(Setf.id(s4)) === true
    }

    "build product 3x3" in {
      Setf.product(Set(1, 2, 3), Set(2, 3, 4)) match {
        case None => failure("Where's my product?")
        case Some((p1, p2)) =>
          p1.d1 === Set(1, 2, 3)
          p2.d1 === Set(2, 3, 4)
          p1.d0 === p2.d0
          val sut = p1.d0
          sut.size === 9
          for {i <- 1 to 3; j <- 2 to 4} sut((i, j)) === true
      }
      ok
    }

    "have initial" in {
      Setf.initial.isDefined === true
    }

    "have terminal" in {
      Setf.terminal.isDefined === true
    }
    
    "build product 0x3" in {
      val sut = Setf.initial flatMap (empty => Setf.product(empty, Set(2, 3, 4)))
      sut match {
        case None => failure("Where's my product?")
        case Some((p1, p2)) =>
          p1.d1 === Sets.Empty
          p2.d1 === Set(2, 3, 4)
          p1.d0 === p2.d0
          p1.d0.isEmpty === true
      }
      ok
    }

    "build pullback 3x3" in {
      val a: set = Set(1, 2, 3)
      val b: set = Set(2, 3, 4)
      val c: set = Set(0, 1)
      val f = SetFunction("f", a, c, _.toString.toInt % 2)
      val g = SetFunction("g", b, c, x => (x.toString.toInt + 1) % 2)

      Setf.pullback(f, g) match {
        case None => failure("Where's my pullback?")
        case Some((p1, p2)) =>
          p1.d1 === a
          p2.d1 === b
          p1.d0 === p2.d0
          val sut = p1.d0
          sut.size === 5
          for {i <- 1 to 3; j <- 2 to 4} sut((i, j)) === ((i+j) %2 == 1)
      }
      ok
    }

    "build pushout 3+3" in {
      val a: set = Set(1, 2, 3)
      val b: set = Set(2, 3, 4)
      val c: set = Set(0, 1)
      val f = SetFunction("f", c, a, _.toString.toInt + 1)
      val g = SetFunction("g", c, b, x => x.toString.toInt + 2)

      Setf.pushout(f, g) match {
        case None => failure("Where's my pushout?")
        case Some((p1, p2)) =>
          p1.d0 === a
          p2.d0 === b
          p1.d1 === p2.d1
          val sut = p1.d1
          val expected = Set(
            Set(("x", 1), ("y", 2)),
            Set(("x", 2), ("y", 3)),
            Set(("x", 3)),
            Set(("y", 4)))
          
          sut === expected
      }
      ok
    }
    
    "have union" in {
      val a: set = Set(1, 2, 3)
      val b: set = Set(3, 4)
      Setf.union(a, b) match {
        case None => failure(s"Oops, failed to build a union of $a and $b")
        case Some((ix, iy)) =>
          ix.d0 === a
          iy.d0 === b
          val expected = Set(("x", 1), ("x", 2), ("x", 3), ("y", 3), ("y", 4))
          ix.d1 === expected
          iy.d1 === expected
          ix(2) === ("x", 2)
          ix(3) === ("x", 3)
          iy(3) === ("y", 3)
          iy(4) === ("y", 4)
      }
      ok
    }
    
    "have an inverse" in {
      val f = SetFunction("f", s2, s2, n => (n.toString.toInt + 1) % 7)
      val g = SetFunction("f", s2, s2, n => (n.toString.toInt + 6) % 7)
      val actual = Setf.inverse(f)
      actual === Some(g)
    }

    "not have an inverse" in {
      val f = SetFunction("f", s2, s4, n => if (n.toString.toInt < 3) "hello" else "goodbye")
      val actual = Setf.inverse(f)
      actual === None
    }

  }
}
