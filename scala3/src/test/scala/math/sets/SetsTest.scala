package math.sets

import math.cat.SetMorphism
import math.sets.Sets._
import org.specs2.execute.Failure
import scalakittens.{Empty, Good, Result}
import testing.TestBase

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.Duration

/**
 * Test suite for Sets object
 */
class SetsTest extends TestBase:

  "This is a specification for the set parser" >> {

    "Set Parser should parse the string" >> {
      Sets.parse("{a, bc, def, ghij}") === Good(Set("a", "bc", "def", "ghij"))
    }

    "groupBy should group" >> {
      val xs = Set(2, 3, -2, 5, 6, 7)
      val f = groupBy(xs, (n: Int) => n * n)
      f(1) === Set.empty
      f(2) === Set.empty
      f(3) === Set.empty
      f(4) === Set(2, -2)
    }

    "groupedBy should group" >> {
      val xs = Set(2, 3, -2, 5, 6, 7)
      val m = groupedBy(xs, (n: Int) => n * n)
      m(1) === Set.empty
      m(2) === Set.empty
      m(3) === Set.empty
      m(4) === Set(2, -2)
    }

    "Iterable should produce a set" >> {
      val source = List("one", "two", "three", "")
      val actual = setOf(source, 4, source.contains(_: String))
      val expected = Set("one", "two", "three", "")
      actual("four") === false
      actual("two") === true
      actual === expected
      actual("two") === true
    }

    "Lazy iterator should not be called when building a set" >> {
      val source = List("one", "two", "three", "")
      var iteratorCalled = false

      val iterable = new Iterable[String] {
        override def iterator: Iterator[String] = {
          iteratorCalled = true
          source.iterator
        }
      }

      val actual = setOf(iterable, 4, (x: String) => source contains x)
      iteratorCalled must beFalse
      val expected = Set("one", "two", "three", "")
      actual("two") === true
      actual === expected
      actual("two") === true
      actual === expected
      actual("two") === true
    }

    "building from a list should work" >> {
      val source = List("one", "two", "three", "")
      val expected = Set("one", "two", "three", "")
      val actual: Set[String] = setOf[String](source,  expected)
      actual === expected
    }

    "setOf(Iterable) should calculate size if not provided" >> {
      val source = List("one", "two", "three", "")
      val actual = setOf(source, (s: String) => source.contains(s))
      val expected = Set("one", "two", "three", "")
      actual === expected
    }

    "setOf(Iterable) should be good" >> {
      val source = List("one", "two", "three", "")
      setOf[String](source) === Set("one", "two", "three", "")
    }

    "infinite set should be okay" >> {
      val iterable: Iterable[Int] = new Iterable[Int] {
        def iterator: Iterator[Int] = new Iterator[Int] {
          private var i = -1

          def next: Int = {
            i += 1
            i
          }

          def hasNext = true

          def remove: Failure = failure("N is immutable")
        }
      }

      val s = setOf(iterable, (n: Int) => true)
      // the following matcher does not work, because our set is infinite, and the test tries to build a vector      
      //      s must contain(42)
      s.contains(42) must beTrue
      var n = 0
      for (i <- s take 10) {
        n += 1
      }
      n === 10
    }

    "range(1,2,3) should be set(1)" >> {
      val r = range(1, 2, 3)
      r must haveSize(1)
    }
    
    "setOf(...) should only contain stuff" >> {
      setOf.elements(1,2,3).contains(1) === true
      setOf.elements(1,2,3).contains(2) === true
      setOf.elements(1,2,3).contains(3) === true
      setOf.elements(1,2,3).contains(4) === false
    }

    "range(1,3,2) should be set(1)" >> {
      val r = range(1, 3, 2)
      r must haveSize(1)
      r === setOf.elements(1)
    }

    "range(0,3,2) should be set(0, 2)" >> {
      val r = range(0, 3, 2)
      r must haveSize(2)
      r === setOf.elements(0, 2)
    }

    "union of a list of sets" >> {
      val sets = (1 to 5) map (n => setOf[Int]((10 * n) to (10 * n) + n))
      val expected = Set(10, 11, 20, 21, 22, 30, 31, 32, 33, 40, 41, 42, 43, 44, 50, 51, 52, 53, 54, 55)
      val actual = union(sets)
      val eq1 = actual == expected
      eq1 must beTrue
      val eq2 = actual == expected
      eq2 must beTrue
      actual === expected // fails for some reason
    }
    
    "union of a finite with an infinite set" >> {
      val src = N.untyped :: Set("a", "b").untyped :: Nil
      val sut = union(src)
      sut.size === InfiniteSize
      sut(BigInt(42)) === true
      sut(42) === false
      sut("b") === true
    }

    "take n of infinite should not hang" >> {
      val sut = N take 5
      sut === Set(0, 1, 2, 3, 4)
    }

    "union of a finite with an infinite should cover both" >> {
      val sut = union(Set("a", "b", 1.4), N) take 20
      sut must contain("a")
      sut must contain("b")
      sut must contain(8)
    }

    "union of an infinite with a finite should cover both" >> {
      val sut = union(N, Set("a", "b", 3.5))
      val sutShort = sut take 20
      sutShort must contain("a")
      sutShort must contain("b")
      sutShort must contain(8)
    }

    "union of an infinite with an infinite should include both" >> {
      val set1 = N filter (x => x % 5 == 0)
      val set2 = N filter (x => x % 5 == 2)
      val sut: Set[BigInt] = union(set1, set2)
      Set(BigInt(15), BigInt(22)).subsetOf(sut) must beTrue
      // can't use must contain(), since sets are infinite
      sut.contains(15) must beTrue
      sut.contains(22) must beTrue
    }

    "union of two finite sets should have the size equal to their sum of sizes" >> {
      union(Set(1, 2), Set(1, 2, 3)).size === 5
    }

    "union of an infinite with any other should have the size Integer.MAX_VALUE" >> {
      union(N, N) must haveSize(Integer.MAX_VALUE)
      union(Set(1, 2), N) must haveSize(Integer.MAX_VALUE)
      union(N, Set(1, 2)) must haveSize(Integer.MAX_VALUE)
    }

    "product with empty should produce empty" >> {
      product2(Set("a", "b", "c"), Set.empty).isEmpty must beTrue
      product2(Set.empty, Set("a", "b", "c")).isEmpty must beTrue
    }

    "product of 2 x 2 should produce a 6-element set" >> {
      val actual = product2(Set("a", "b"), Set("A", "B"))
      val expected = Set(("a", "A"), ("a", "B"), ("b", "A"), ("b", "B"))
      actual === expected
      actual.toString === "Set((a,A), (a,B), (b,A), (b,B))"
      expected === actual
    }

    "product of 3 x 2 should produce a 6-element set" >> {
      val actual = product2(Set("a", "b", "c"), Set("A", "B"))
      val expected = Set(("a", "A"), ("a", "B"), ("b", "A"), ("b", "B"), ("c", "A"), ("c", "B"))
      actual === expected
      expected === actual
    }

    "product of 2 x 3 should produce a 6-element set" >> {
      val actual = product2(Set("a", "b"), Set("A", "B", "C"))
      val expected = Set(("a", "A"), ("a", "B"), ("a", "C"), ("b", "A"), ("b", "B"), ("b", "C"))
      actual === expected
      expected === actual
    }

    "product should not prematurely calculate size" >> {
      var wasCalled = false
      val source = Set(1,2)
      val s = setOf(source, {wasCalled = true; 2}, (x: Int) => source contains x)

      wasCalled must beFalse
      val ss = product2(s, s)
      wasCalled must beFalse
      ss must haveSize(4)
      wasCalled must beTrue
    }

    "product of an infinite set with a finite should iterate over all pairs" >> {
      val sut = product2(N, Set('A', 'B'))
      val segment = sut take 30
      segment.contains((4,'B')) must beTrue
      segment.contains((2,'A')) must beTrue
    }

    "product of a finite set with an infinite should iterate over all pairs" >> {
      val sut = product2(Set("a", "b"), N)
      val segment = sut take 20
      segment.contains(("b",5)) must beTrue
      segment.contains(("a",3)) must beTrue
    }

    "product of two infinite sets should iterate over all pairs" >> {
      val sut = product2(N, N)
      val segment = sut take 40
      segment.contains((1,3)) must beTrue
      segment.contains((3,1)) must beTrue
      segment.contains((3,3)) must beTrue
    }

    "powerset of a 3-element set should give 8 elements" >> {
      val s = Set("a", "b", "c")
      val actual = pow(s)
      val expected = Set(Set.empty, Set("a"), Set("b"), Set("c"), Set("a", "b"), Set("a", "c"), Set("b", "c"), s)
      actual === expected
    }

    "{x,y,z}^{1,2} should give a 9-element set of maps" >> {
      val domain = Set("1", "2")
      val codomain = Set("x", "y", "z")
      val actual = exponent(domain, codomain)
      actual must haveSize(9)
      actual must contain(Map("1" -> "y", "2" -> "x"))
    }

    "parse({a,b,c} with spaces should be okay" >> {
      Sets.parse("  { a , b,  c} ") === Good(Set("a", "b", "c"))
    }

    "parse empty should produce empty set" >> {
      Sets.parse("{}").map(_.isEmpty) === Good(true)
      Sets.parse(" {  } ").map(_.isEmpty) === Good(true)
    }

    "Parse singleton should produce singleton" >> {
      Sets.parse("{0}") === Good(Set("0"))
      Sets.parse("{xyz}") === Good(Set("xyz"))
      Sets.parse("{ xyz }") === Good(Set("xyz"))
    }

    "Parse without closing curly should throw an exception" >> {
      try {
        val x = Sets.parse("{a, b, c")
        failure("Should have thrown an exception")
      } catch {
        case e: Exception => // as designed
      }
      ok
    }

    "Parse without opening curly should throw an exception" >> {
      try {
        val x = Sets.parse("a, b, c}")
        failure("Should have thrown an exception")
      } catch {
        case e: Exception => // as designed
      }
      ok
    }

    "Parse without nothing between commas should throw an exception" >> {
      try {
        val x = Sets.parse("{a, b,, c}")
        failure("Should have thrown an exception")
      } catch {
        case e: Exception => // as designed
      }
      ok
    }

    "String should work as expected" >> {
      val sut = Set("a", "b", "c")
      Sets.toString(sut) === "{a, b, c}"
    }

    "Filtering 1..5 by oddity should produce 1,3,5" >> {
      val sut = Set(1, 2, 3, 4, 5)
      val actual : Set[Int] = sut.filter(x => x % 2 == 1)
      actual === Set(1, 3, 5)
    }

    "Filtering product by diagonal should produce diagonal" >> {
      val sut = product2(Set(1, 2, 3), Set(1, 2, 3))
      val p1 = (1, 1)
      val p2 = (2, 2)
      val p3 = (3, 3)
      val expected = Set(p1, p2, p3)
      val actual = sut.filter(p => {val (x, y) = p; x  == y})
      actual === expected
    }

    "Set should be monadic" >> {
      val i: Set[Int] = Set(1, 2, 3)
      val s: Set[String] = for (n <- i) yield n.toString
      s === Set("1", "2", "3")
    }

    "NNO's 10th element should be 9" >> {
      N.contains(1001590) must beTrue
      val first10 = N.take(10)

      0 to 9 forall (i => first10.contains(i) must beTrue)
    }

    "Iterator of 1 is asingleton" >> {
      Sets.isSingleton(List("abc")) must beTrue
    }

    "Iterator of 0 is not a singleton" >> {
      Sets.isSingleton(List()) must beFalse
    }

    "Iterator of 2 is not a singleton" >> {
      Sets.isSingleton(List("a", "b")) must beFalse
    }

    "Factorset" >> {
      val s = setOf[Int](1 to 10)
      def isOdd(x: Int) = x % 2 == 0
      val br: BinaryRelation[Int, Int] = (a: Int, b: Int) => isOdd(a) == isOdd(b)
      val factoring = new FactorSet(s, br)

      factoring.content must haveSize(2)
      val twoSets = Array(Set(2, 4, 6, 8, 10), Set(1, 3, 5, 7, 9))
      val factor = Set(twoSets(1), twoSets(0))
      factor === factoring.content
      factoring.asFunction(6) === twoSets(0)
      factoring.asFunction(7) === twoSets(1)
    }

    "Set(iterable, size, filter) should not return false positives" >> {
      val s = Set(1, 2, 3)
      def predicate = (x:Any) => x == 2
      val sut = setOf(s, 1, predicate)
      sut contains 1 must beFalse
      val collected = sut.iterator.foldLeft(Sets.Empty) (_+_)
      collected === Set(2)
    }

    "Pullback, plain case" >> {
      val xs = Set(1,2,3,4,5)
      val ys = Set(2,4,6,8,10)
      val actual = pullback(xs, ys, (x:Int) => x/2, (y:Int) => y/5)
      val expected = Set((1,2),(1,4),(2,6),(2,8),(3,6),(3,8),(4,10),(5,10))
      actual === expected
    }

    "Finite Sets should not not contain NNO" >> {
      // may not even compile, good
      //      FiniteSets.contains(N) must beFalse
      ok
    }

    "Finite Sets should not contain itself" >> {
      // may not even compile, good (assertDoesNotCompile("..")
      //      FiniteSets.contains(FiniteSets) must beFalse
      ok
    }

    "Finite Sets should contain various finite sets" >> {
      FiniteSets.contains(Sets.Empty) must beTrue
      FiniteSets.contains(Set("infinity")) must beTrue
      FiniteSets.contains(Set(1,2,3,42)) must beTrue
      ok
    }
  }


