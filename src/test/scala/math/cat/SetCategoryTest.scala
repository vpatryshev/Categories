package math.cat

import math.cat.SetCategory._
import math.cat.SetFunction._
import math.sets.Sets._
import math.sets.{BigSet, Sets}
import org.specs2.mutable._
import scalakittens.Good


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
      val arrow = SetFunction.build("sample", s1, s2, _.asInstanceOf[Int] / 7) iHope
      val theGraph = graphOfSets(sets)
      theGraph.nodes === sets
      theGraph.arrows.contains(theGraph.arrow(arrow)) === true
    }

    "produce no coequalizer if category is too small" in {
      val f = SetFunction.build("f", s1, s2, _ ⇒ 3) iHope
      
      val g = SetFunction.build("g", s1, s2, _ ⇒ 5) iHope
      
      new SetCategory(BigSet(Set(s1, s2, s3))).coequalizer(f, g).isBad === true
      
      val v2 = evenSets.coequalizer(f, g)
      v2.isBad === true
    }

    def contains[T](x: T)(s: Any): Boolean = s match {
      case us: Set[T] ⇒ us(x)
      case _ ⇒ false
    }

    def whereIn(s: set)(point: Any): Any =
      s find contains(point) getOrElse Sets.Empty


    "produce coequalizer of two" in {
      val f = SetFunction.build("f", s1, s2, _ ⇒ 3) iHope
      val g = SetFunction.build("g", s1, s2, _ ⇒ 5) iHope
      val actual = Setf.coequalizer(f, g)
      val expectedSet: set = Set(Set(0), Set(1), Set(2), Set(3, 5), Set(4), Set(6))

      def where(x: Any): Any = whereIn(expectedSet)(x)

      val expected = SetFunction.build("Factorset", s2, expectedSet, where)
      actual === expected

      Setf.coequalizer(f, id(s3)).isBad === true
      Setf.coequalizer(f, id(s2)).isBad === true
      Setf.coequalizer(f, id(s1)).isBad === true
    }

    "produce coequalizer of n" in {
      val f = SetFunction.build("f", s1, s2, _ ⇒ 3).iHope
      val g = SetFunction.build("g", s1, s2, _ ⇒ 5).iHope
      val h = SetFunction.build("h", s1, s2, _ ⇒ 1).iHope
      val actual = Setf.coequalizer(f :: g :: h :: Nil)
      val expectedSet: set = Set(Set(0), Set(2), Set(1, 3, 5), Set(4), Set(6))

      def where(x: Any): Any = whereIn(expectedSet)(x)

      val expected = SetFunction.build("Factorset", s2, expectedSet, where)
      actual === expected

      val singletons: set = s2 map singleton
      Setf.coequalizer(f :: Nil) === SetFunction.build("Factorset", s2, singletons, singleton)
      Setf.coequalizer(List.empty[SetFunction]).isBad === true
      Setf.coequalizer(f :: id(s3) :: Nil).isBad === true
      Setf.coequalizer(f :: id(s2) :: Nil).isBad === true
      Setf.coequalizer(f :: id(s1) :: Nil).isBad === true
      Setf.coequalizer(f :: id(s3) :: Nil).isBad === true
      Setf.coequalizer(f :: id(s2) :: Nil).isBad === true
      Setf.coequalizer(f :: id(s1) :: Nil).isBad === true
    }

    "produce 0th degree of an object" in {
      Setf.degree(Set(1, 2, 3), 0).map(_._1.size) === Good(1)
      Setf.degree(Set.empty[Any], 0).map(_._1.size) === Good(1)
    }

    "produce 1st degree of an object" in {
      Setf.degree(Set(1, 2, 3), 1).map(_._1) === Good(Set(List(1), List(2), List(3)))
      val maybeZeroOne = Setf.degree(Set.empty[Any], 1).map(_._1)
      maybeZeroOne === Good(Set.empty[Any])
    }

    "produce 2nd degree of an object" in {
      Setf.degree(Set(1, 2, 3), 2).map(_._1) ===
        Good(Set(
          List(1,1),
          List(2,1),
          List(3,1),
          List(1,2),
          List(2,2),
          List(3,2),
          List(1,3),
          List(2,3),
          List(3,3))
        )
    }

    "produce 4th degree of an object" in {
      val source: set = Sets.setOf(1, 2, 3)
      val sut = Setf.degree(source, 4).map(_._1)

      sut match {
        case Good(s: set) ⇒
          s.size === 81
          for {
            a <- source
            b <- source
            c <- source
            d <- source
          } {
            val point = List(a,b,c,d)
            s(point) === true
          }
        case none ⇒ failure(s"must have built a 4th degree: $none")
      }
      ok
    }

    "produce no equalizer if it's not in the category" in {
      // the category of even-sized sets
      val f = SetFunction.build("f", s2, s4, n ⇒ if (n.toString.toInt < 3) "hello" else "goodbye")iHope
      val g = SetFunction.build("g", s2, s4, n ⇒ if (n.toString.toInt < 1) "hello" else "goodbye")iHope
      val eq = evenSets.equalizer(f, g)
      eq.isBad === true
    }

    "produce an empty equalizer if it exists" in {
      val f = SetFunction.build("f", s2, s4, n ⇒ if (n.toString.toInt < 3) "hello" else "goodbye").iHope
      val g = SetFunction.build("g", s2, s4, n ⇒ if (n.toString.toInt < 4) "cruel" else "world").iHope
      evenSets.equalizer(f, g) match {
        case Good(inclusion) ⇒
          inclusion.d0 === Sets.Empty
          inclusion.d1 === s2
        case none ⇒ failure(s"Expected some equalizer: $none")
      }
      ok
    }

    "produce an equalizer" in {
      // the category of even-sized sets
      val sut = Setf
      val f = SetFunction.build("f", s2, s4,
        n ⇒ if (n.toString.toInt < 3) "hello" else "goodbye").iHope
      val g = SetFunction.build("g", s2, s4,
        n ⇒ if (n.toString.toInt < 1) "cruel" else if (n.toString.toInt < 5) "hello" else "goodbye").iHope
      sut.equalizer(f, g) match {
        case Good(eq) ⇒
          eq.d0 === Set(1, 2, 5, 6)
          eq.d1 === s2
        case none ⇒ failure(s"Expected an equalizer: $none")
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
      Setf.initial === Good(Sets.Empty)
      evenSets.initial === Good(Sets.Empty)
      oddSets.initial.isBad must beTrue
    }

    "check for epi" in {
      val sut1 = SetFunction.build("inclusion", s3, s4, x ⇒ x).iHope
      Setf.isEpimorphism(sut1) === false
      val sut2 = SetFunction.build("covering", s4, s3,
        {
          case "goodbye" ⇒ "hello"
          case x ⇒ x
        }).iHope
      Setf.isEpimorphism(sut2) === true
      Setf.isEpimorphism(Setf.id(s4)) === true
    }

    "check for mono" in {
      val sut1 = SetFunction.build("inclusion", s3, s4, x ⇒ x).iHope
      Setf.isMonomorphism(sut1) === true
      val sut2 = SetFunction.build("covering", s4, s3,
        {
          case "goodbye" ⇒ "hello"
          case x ⇒ x
        }).iHope
      Setf.isMonomorphism(sut2) === false
      Setf.isMonomorphism(Setf.id(s4)) === true
    }

    "build product 3x3" in {
      Setf.product(Set(1, 2, 3), Set(2, 3, 4)) match {
        case Good((p1, p2)) ⇒
          p1.d1 === Set(1, 2, 3)
          p2.d1 === Set(2, 3, 4)
          p1.d0 === p2.d0
          val sut = p1.d0
          sut.size === 9
          for {i <- 1 to 3; j <- 2 to 4} sut((i, j)) === true
        case none ⇒ failure(s"Where's my product? $none")
      }
      ok
    }

    "have initial" in {
      Setf.initial match {
        case Good(s0) ⇒ s0 must be empty
        case none ⇒ failure(s"Oops, no initial: $none")
      }
      ok
    }

    "have terminal" in {
      Setf.terminal match {
        case Good(s1) ⇒
          s1.size === 1
          s1.contains(Sets.Empty) === true
        case none ⇒ failure(s"Oops, no terminal: $none")
      }
      
      ok
    }
    
    "build product 0x3" in {
      val sut = Setf.initial flatMap (empty ⇒ Setf.product(empty, Set(2, 3, 4)))
      sut match {
        case Good((p1, p2)) ⇒
          p1.d1 === Sets.Empty
          p2.d1 === Set(2, 3, 4)
          p1.d0 === p2.d0
          p1.d0.isEmpty === true
        case none ⇒ failure(s"Where's my product? $none")
      }
      ok
    }

    "build pullback 3x3" in {
      val a: set = Set(1, 2, 3)
      val b: set = Set(2, 3, 4)
      val c: set = Set(0, 1)
      val f = SetFunction.build("f", a, c, _.toString.toInt % 2).iHope
      val g = SetFunction.build("g", b, c, x ⇒ (x.toString.toInt + 1) % 2).iHope

      Setf.pullback(f, g) match {
        case Good((p1, p2)) ⇒
          p1.d1 === a
          p2.d1 === b
          p1.d0 === p2.d0
          val sut = p1.d0
          sut.size === 5
          for {i <- 1 to 3; j <- 2 to 4} sut((i, j)) === ((i+j) %2 == 1)
        case none ⇒ failure(s"Where's my pullback? $none")
      }
      ok
    }

    "build pushout 3+3" in {
      val a: set = Set(1, 2, 3)
      val b: set = Set(2, 3, 4)
      val c: set = Set(0, 1)
      val f = SetFunction.build("f", c, a, _.toString.toInt + 1).iHope
      val g = SetFunction.build("g", c, b, x ⇒ x.toString.toInt + 2).iHope

      val actual = Setf.pushout(f, g)
      actual match {
        case Good((p1, p2)) ⇒
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
        case none ⇒ failure(s"Where's my pushout? $none")
      }
      ok
    }
    
    "have union" in {
      val a: set = Set(1, 2, 3)
      val b: set = Set(3, 4)
      val actual = Setf.union(a, b)
      actual match {
        case Good((ix, iy)) ⇒
          ix.d0 === a
          iy.d0 === b
          val expected = Set(("x", 1), ("x", 2), ("x", 3), ("y", 3), ("y", 4))
          ix.d1 === expected
          iy.d1 === expected
          ix(2) === ("x", 2)
          ix(3) === ("x", 3)
          iy(3) === ("y", 3)
          iy(4) === ("y", 4)
        case none ⇒ failure(s"Oops, failed to build a union of $a and $b: $none")
      }
      ok
    }
    
    "have an inverse" in {
      val f = SetFunction.build("f", s2, s2, n ⇒ (n.toString.toInt + 1) % 7).iHope
      val g = SetFunction.build("f", s2, s2, n ⇒ (n.toString.toInt + 6) % 7).iHope
      val actual = Setf.inverse(f)
      actual === Good(g)
    }

    "not have an inverse" in {
      val f = SetFunction.build("f", s2, s4, n ⇒ if (n.toString.toInt < 3) "hello" else "goodbye").iHope
      val actual = Setf.inverse(f)
      actual.isBad must beTrue
    }

  }
}
