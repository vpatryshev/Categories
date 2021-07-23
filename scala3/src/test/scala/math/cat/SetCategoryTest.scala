package math.cat

import math.cat.SetCategory._
import math.cat.SetFunction._
import math.sets.Sets._
import math.sets.{BigSet, BinaryRelation, Sets}
import org.specs2.mutable._
import scalakittens.{Good, Result}

import scala.language.postfixOps
import SetFunction.fun

/**
  * Set Category tests
  */
class SetCategoryTest extends Specification:
  val s1: set = Set[Int](42) untyped
  val s2: set = Sets.range(0, 7, 1) untyped
  val s3: set = Set("hello", "cruel", "world") untyped
  val s4: set = Set("hello", "goodbye", "cruel", "world") untyped
  private val allEvenSets: BigSet[Set[Any]] =
    BigSet.comprehension[Set[Any]](_.size % 2 == 0)
  val evenSets: SetCategory = new SetCategory(allEvenSets)
  val oddSets: SetCategory =
    new SetCategory(BigSet.comprehension[Set[Any]](_.size % 2 == 1))

  "SetCategory" should {
    "buildGraph" in {
      val sets = BigSet(Set(s1, s2))
      val arrow = fun(s1,s2)("sample", _.toInt / 7)
      val theGraph = graphOfSets(sets)
      theGraph.nodes === sets
      theGraph.arrows.contains(theGraph.arrow(arrow)) === true
    }

    "produce no coequalizer if category is too small" in {
      val f = fun(s1,s2)("f", _ => 3)
      val g = fun(s1, s2)("g", _ => 5)
      
      new SetCategory(BigSet(Set(s1, s2, s3))).coequalizer(f, g).isBad === true
      
      val v2 = evenSets.coequalizer(f, g)
      v2.isBad === true
    }

    def contains[T](x: T)(s: Any): Boolean = s match {
      case us: Set[_] => us.asInstanceOf[Set[T]](x)
      case _ => false
    }

    def whereIn(s: set)(point: Any): Any =
      s find contains(point) getOrElse Sets.Empty


    "produce coequalizer of two" in {
      val f = fun(s1,s2)("f", _ => 3)
      val g = fun(s1,s2)("g", _ => 5)
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
      val f = fun(s1,s2)("f", _ => 3)
      val g = fun(s1,s2)("g", _ => 5)
      val h = fun(s1,s2)("h", _ => 1)
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
      Setf.degree(Empty, 0).map(_._1.size) === Good(1)
    }

    "produce 1st degree of an object" in {
      val actual = Setf.degree(Set(1, 2, 3), 1)
      actual.isGood === true
      actual.map(_._1) === Good(Set(List(1), List(2), List(3)))
      val maybeZeroOne = Setf.degree(Empty, 1).map(_._1)
      maybeZeroOne === Good(Empty)
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
      val source: set = setOf.elements(1, 2, 3)
      val sut: Result[set] = Setf.degree(source, 4).map(_._1)

      sut match {
        case Good(s) =>
          s.size === 81
          for
            a <- source
            b <- source
            c <- source
            d <- source
          do
            s(List(a,b,c,d)) === true
 
        case none => failure(s"must have built a 4th degree: $none")
      }
      ok
    }

    "produce no equalizer if it's not in the category" in {
      // the category of even-sized sets
      val f = fun(s2, s4)("f", 
        n => if n.toInt < 3 then "hello" else "goodbye")
      val g = fun(s2, s4)("g",
        n => if n.toInt < 1 then "hello" else "goodbye")
      val eq = evenSets.equalizer(f, g)
      eq.isBad === true
    }

    "produce an empty equalizer if it exists" in {
      val f = fun(s2, s4)("f",
        n => if n.toInt < 3 then "hello" else "goodbye")
      val g = fun(s2, s4)("g",
        n => if n.toInt < 4 then "cruel" else "world")
      
      evenSets.equalizer(f, g) match
        case Good(inclusion) =>
          inclusion.d0 === Sets.Empty
          inclusion.d1 === s2
        case none => failure(s"Expected some equalizer: $none")

      ok
    }

    "produce an equalizer" in {
      // the category of even-sized sets
      val sut = Setf
      val f = fun(s2, s4)("f",
        n => if n.toInt < 3 then "hello" else "goodbye")
      val g = fun(s2, s4)("g",
        n => if n.toInt < 1 then "cruel" else if n.toInt < 5 then "hello" else "goodbye")
      sut.equalizer(f, g) match {
        case Good(eq) =>
          eq.d0 === Set(1, 2, 5, 6)
          eq.d1 === s2
        case none => failure(s"Expected an equalizer: $none")
      }
      ok
    }

    "have an id" in {
      for
        obj <- s4
      do Setf.id(s4)(obj) === obj

      ok
    }

    "have a 0" in {
      Setf.initial === Good(Sets.Empty)
      evenSets.initial === Good(Sets.Empty)
      oddSets.initial.isBad must beTrue
    }

    "check for epi" in {
      val sut1 = fun(s3, s4)("inclusion", x => x)
      Setf.isEpimorphism(sut1) === false
      val sut2 = fun(s4, s3)("covering",
        {
          case "goodbye" => "hello"
          case x => x
        })
      Setf.isEpimorphism(sut2) === true
      Setf.isEpimorphism(Setf.id(s4)) === true
    }

    "check for mono" in {
      val sut1 = fun(s3, s4)("inclusion", x => x)
      Setf.isMonomorphism(sut1) === true
      val sut2 = fun(s4, s3)("covering",
        {
          case "goodbye" => "hello"
          case x => x
        })
      Setf.isMonomorphism(sut2) === false
      Setf.isMonomorphism(Setf.id(s4)) === true
    }

    "build product 3x3" in {
      Setf.product(Set(1, 2, 3), Set(2, 3, 4)) match
        case Good((p1, p2)) =>
          p1.d1 === Set(1, 2, 3)
          p2.d1 === Set(2, 3, 4)
          p1.d0 === p2.d0
          val sut = p1.d0
          sut.size === 9
          for i <- 1 to 3; j <- 2 to 4 do sut((i, j)) === true
        case none => failure(s"Where's my product? $none")

      ok
    }

    "have initial" in {
      Setf.initial match
        case Good(s0) => s0 must be empty
        case none => failure(s"Oops, no initial: $none")

      ok
    }

    "have terminal" in {
      Setf.terminal match
        case Good(s1) =>
          s1.size === 1
          s1.contains(Sets.Empty) === true
        case none => failure(s"Oops, no terminal: $none")
      
      ok
    }
    
    "build product 0x3" in {
      val sut = Setf.initial flatMap (empty => Setf.product(empty, Set(2, 3, 4)))
      sut match
        case Good((p1, p2)) =>
          p1.d1 === Sets.Empty
          p2.d1 === Set(2, 3, 4)
          p1.d0 === p2.d0
          p1.d0.isEmpty === true
        case none => failure(s"Where's my product? $none")

      ok
    }

    "build pullback 3x3" in {
      val a: set = Set(1, 2, 3)
      val b: set = Set(2, 3, 4)
      val c: set = Set(0, 1)
      val f = fun(a,c)("f", _.toInt % 2)
      val g = fun(b,c)("g", x => (x.toInt + 1) % 2)

      Setf.pullback(f, g) match
        case Good((p1, p2)) =>
          p1.d1 === a
          p2.d1 === b
          p1.d0 === p2.d0
          val sut = p1.d0
          sut.size === 5
          for {i <- 1 to 3; j <- 2 to 4} sut((i, j)) === ((i+j) %2 == 1)
        case none => failure(s"Where's my pullback? $none")

      ok
    }

    "build pushout 3+3" in {
      val a: set = Set(1, 2, 3)
      val b: set = Set(2, 3, 4)
      val c: set = Set(0, 1)
      val f = fun(c,a)("f", _.toInt + 1)
      val g = fun(c,b)("g", _.toInt + 2)

      val actual = Setf.pushout(f, g)
      actual match
        case Good((p1, p2)) =>
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
        case none => failure(s"Where's my pushout? $none")

      ok
    }
    
    "have union" in {
      val a: set = Set(1, 2, 3)
      val b: set = Set(3, 4)
      val actual = Setf.union(a, b)
      actual match
        case Good((ix, iy)) =>
          ix.d0 === a
          iy.d0 === b
          val expected = Set(("x", 1), ("x", 2), ("x", 3), ("y", 3), ("y", 4))
          ix.d1 === expected
          iy.d1 === expected
          ix(2) === ("x", 2)
          ix(3) === ("x", 3)
          iy(3) === ("y", 3)
          iy(4) === ("y", 4)
        case none => failure(s"Oops, failed to build a union of $a and $b: $none")

      ok
    }
    
    "have an inverse" in {
      val f = fun(s2,s2)("f", n => (n.toInt + 1) % 7)
      val g = fun(s2,s2)("f", n => (n.toInt + 6) % 7)
      val actual = Setf.inverse(f)
      actual === Good(g)
    }

    "not have an inverse" in {
      val f = fun(s2,s4)("f", n => if n.toInt < 3 then "hello" else "goodbye")
      val actual = Setf.inverse(f)
      actual.isBad must beTrue
    }

    "Have factorset by a diagonal" >> {
      val s = setOf[Int](1 to 10)
      val br: BinaryRelation[Int, Int] = (a: Int, b: Int) => a == b
      val actual: SetMorphism[Int, Set[Int]] = factorset(s, br)
      val factor = setOf[Set[Int]](for (i <- s) yield Set(i))
      actual === SetMorphism.build(s, factor, (i:Int) => Set(i)).iHope
    }

    "have factorset mod 2" >> {
      val s0 = setOf[Int](1 to 10)
      val br: BinaryRelation[Int, Int] = (a: Int, b: Int) => a % 2 == b % 2
      val actual = factorset(s0, br)
      val listofClasses = Array(Set(2, 4, 6, 8, 10), Set(1, 3, 5, 7, 9))
      val setOfClasses = Set(listofClasses(0), listofClasses(1))
      actual === SetMorphism.build(
        d0 = s0, d1 = setOfClasses, function = (i:Int) => listofClasses(i % 2)
      ).iHope
    }

    "for factorset" >> {
      val inclusive: Seq[Int] = 1 until 11
      val ten: Iterable[Int] = inclusive
      val set0: Set[Int] = setOf(ten)
      val set1 = set0.map(i => i:Any)
      def isOdd(x: Any) = x.toString.charAt(0) % 2 == 0
      val br: BinaryRelation[Any, Any] = (a: Any, b: Any) => isOdd(a) == isOdd(b)
      val s = Array(Set(2, 4, 6, 8), Set(1, 3, 5, 7, 9, 10))
      val sut = factorset(set1, br)
      val factor = Set(s(1), s(0))
      set1 === sut.d0
      factor === sut.d1
      s(0) === sut(8)
      s(1) === sut(5)
    }
  }
