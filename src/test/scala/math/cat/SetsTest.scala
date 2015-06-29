package math.cat

import org.specs2.mutable._
import Sets._

/**
 * Test suite for Sets object
 */
class SetsTest extends Specification {

  "This is a specification for the set parser" >> {

    "Set Parser should parse the string" >> {
      Sets.parse("{a, bc, def, ghij}") === Set("a", "bc", "def", "ghij")
    }

    "groubBy should group" >> {
      val xs = Set(2, 3, -2, 5, 6, 7)
      val ys = Set(1, 2, 3, 4, 5, 6)
      val f = groupBy(xs, ys, (n: Int) => (n * n))
      f(1) === Set.empty
      f(2) === Set.empty
      f(3) === Set.empty
      f(4) === Set(2, -2)
    }

    "Iterable should produce a set" >> {
      val source = List("one", "two", "three", "")
      val actual = Sets.setOf[String](source, 4, source.contains(_: String))
      val expected = Set("one", "two", "three", "")
      actual === expected
    }

    "Lazy iterator should not be called when building a set" >> {
      val source = List("one", "two", "three", "")
      var iteratorCalled = false

      val iterable = new Iterable[String] {
        override def iterator = {
          iteratorCalled = true
          source.iterator
        }
      }

      val actual = Sets.setOf(iterable.iterator, 4, (x: String) => source contains x)
      iteratorCalled must beFalse
      val expected = Set("one", "two", "three", "")
      actual === expected
    }

    "Lazy iterator should not be called when building a set" >> {
      val source = List("one", "two", "three", "")
      val actual = setOf(source, (s: String) => source.contains(s))
      val expected = Set("one", "two", "three", "")
      actual.size === expected.size
      actual must contain("one")
      actual must contain("two")
      actual must contain("three")
      actual must contain("")
      actual === expected
    }

    "setOf(Iterable) should calculate size if not provided" >> {
      val source = List("one", "two", "three", "")
      val actual = setOf(source, (s: String) => source.contains(s))
      val expected = Set("one", "two", "three", "")
      actual.size === expected.size
      actual must contain("one")
      actual must contain("two")
      actual must contain("three")
      actual must contain("")
      actual === expected
    }

    "setOf(Iterable) should be good" >> {
      val source = List("one", "two", "three", "")
      setOf(source) === Set("one", "two", "three", "")
    }

    "infinite set should be okay" >> {
      val iterable = new Iterable[Int] {
        def iterator = new Iterator[Int] {
          var i = 0;

          def next = {
            i += 1;
            i - 1
          }

          def hasNext = true

          def remove = failure("N is immutable")
        }
      }

      val set = setOf(iterable, (n: Int) => true)
      set must contain(42)
      var n = 0
      for (i <- set take 10) {
        n += 1
      }
      n === 10
    }

    "range(1,2,3) should be set(1)" >> {
      val r = range(1, 2, 3)
      r must haveSize(1)
    }

    "range(1,3,2) should be set(1)" >> {
      val r = range(1, 3, 2)
      r must haveSize(1)
      r === setOf(List(1))
    }

    "range(0,3,2) should be set(0, 2)" >> {
      val r = range(0, 3, 2)
      r must haveSize(2)
      r === setOf(List(0, 2))
    }

    "union of a list of sets" >> {
      val sets = (1 to 5) map (n => setOf((10 * n) to (10 * n) + n))
      val expected = Set(10, 11, 20, 21, 22, 30, 31, 32, 33, 40, 41, 42, 43, 44, 50, 51, 52, 53, 54, 55)
      val actual = union(sets)
      actual === expected
    }

    "take n of infinite should not hang" >> {
      val sut = N take 5
      sut === Set(0, 1, 2, 3, 4)
    }

    "union of a finite with an infinite should cover both" >> {
      val sut = union(Set("a", "b", 1.4), N.asInstanceOf[Set[Any]]) take 20
      sut must contain("a")
      sut must contain("b")
      sut must contain(8)
    }

    "union of an infinite with a finite should cover both" >> {
      val sut = union(N.asInstanceOf[Set[Any]], Set("a", "b", 3.5)) take 20
      sut must contain("a")
      sut must contain("b")
      sut must contain(8)
    }

    "union of an infinite with an infinite should cover both" >> {
      val sut = union(N filter (x => x % 5 == 0), N filter (x => x % 5 == 2)) take 20
      sut must contain(15)
      sut must contain(22)
      sut must contain(0)
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
      product(Set("a", "b", "c"), Set.empty).isEmpty must beTrue
      product(Set.empty, Set("a", "b", "c")).isEmpty must beTrue
    }

    "product of 2 x 2 should produce a 6-element set" >> {
      val actual = product(Set("a", "b"), Set("A", "B"))
      val expected = Set(("a", "A"), ("a", "B"), ("b", "A"), ("b", "B"))
      actual === expected
      expected === actual
    }

    "product of 3 x 2 should produce a 6-element set" >> {
      val actual = product(Set("a", "b", "c"), Set("A", "B"))
      val expected = Set(("a", "A"), ("a", "B"), ("b", "A"), ("b", "B"), ("c", "A"), ("c", "B"))
      actual === expected
      expected === actual
    }

    "product of 2 x 3 should produce a 6-element set" >> {
      val actual = product(Set("a", "b"), Set("A", "B", "C"))
      val expected = Set(("a", "A"), ("a", "B"), ("a", "C"), ("b", "A"), ("b", "B"), ("b", "C"))
      actual === expected
      expected === actual
    }

    "product should not prematurely calculate size" >> {
      var wasCalled = false
      val source = Set(1,2)
      val s = Sets.setOf(source, {wasCalled = true; 2}, (x: Int) => (source contains x))

      wasCalled must beFalse
      val ss = product(s, s)
      wasCalled must beFalse
      ss must haveSize(4)
      wasCalled must beTrue
    }

    "product of an infinite set with a finite should iterate over all pairs" >> {
      val sut = product(N, Set('A, 'B))
      val segment = sut take 30
      System.out.flush
      segment must contain((4,'B))
      segment must contain((2,'A))
    }

    "product of a finite set with an infinite should iterate over all pairs" >> {
      val sut = product(Set("a", "b"), N)
      val segment = sut take 20
      segment must contain(("b",5))
      segment must contain(("a",3))
    }

    "product of two infinite sets should iterate over all pairs" >> {
      val sut = product(N, N)
      val segment = sut take 40
      segment must contain((1,3))
      segment must contain((3,1))
      segment must contain((3,3))
    }

    "powerset of a 3-element set should give 8 elements" >> {
      val set = Set("a", "b", "c")
      val actual = powerset(set)
      val expected = Set(Set.empty, Set("a"), Set("b"), Set("c"), Set("a", "b"), Set("a", "c"), Set("b", "c"), set)
      actual === expected
    }

    "split should split" >> {
      val sut = Set("a1", "b2", "c3")
      val (x, xs) = split(sut)
      x === "a1"
      val i = xs.iterator
      i.next === "b2"
      i.next === "c3"
      i.hasNext must beFalse
    }

    "{x,y,z}^{1,2} should give a 9-element set of maps" >> {
      val domain = Set("1", "2")
      val codomain = Set("x", "y", "z")
      val actual = exponent(domain, codomain)
      actual must haveSize(9)
      actual must contain(Map("1" -> "y", "2" -> "x"))
    }

    "parse({a,b,c} with spaces should be okay" >> {
      Sets.parse("  { a , b,  c} ") === Set("a", "b", "c")
    }

    "parse empty should produce empty set" >> {
      Sets.parse("{}").isEmpty must beTrue
      Sets.parse(" {  } ").isEmpty must beTrue
    }

    "Parse singleton should produce singleton" >> {
      Sets.parse("{0}") === Set("0")
      Sets.parse("{xyz}") === Set("xyz")
      Sets.parse("{ xyz }") ===Set("xyz")
    }

    "Parse without closing curly should throw an exception" >> {
      try {
        val x = Sets.parse("{a, b, c")
        failure("Should have thrown an exception")
      } catch {
        case e: Exception => println(e) // as designed
      }
      true
    }

    "Parse without opening curly should throw an exception" >> {
      try {
        val x = Sets.parse("a, b, c}")
        failure("Should have thrown an exception")
      } catch {
        case e: Exception => println(e) // as designed
      }
      true
    }

    "Parse without nothing between commas should throw an exception" >> {
      try {
        val x = Sets.parse("{a, b,, c}")
        failure("Should have thrown an exception")
      } catch {
        case e: Exception => println(e) // as designed
      }
      true
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
      val sut = product(Set(1, 2, 3), Set(1, 2, 3))
      val p1 = (1, 1)
      val p2 = (2, 2)
      val p3 = (3, 3)
      val expected = Set(p1, p2, p3)
      val actual = sut.filter(p => {val (x, y) = p; x  == y})
      !(actual must contain((1, 2)))
      actual must haveSize(3)
      actual must contain(p1)
      actual must contain(p2)
      actual must contain(p3)
      actual === expected
    }
/*
    def doNotTestDownshift() {
      val source: Set[Any] = Set("abc", 123, 4.56, Set("qqq"), "zzz")
      val target: Set[String] = downshift(source)
      target === Set("abc", "zzz")
    }

    "Set should be monadic" >> {
      val i: Set[Int] = Set(1, 2, 3)
      val s: Set[String] = for (n <- i) yield(n.toString)
      assertEquals(Set("1", "2", "3"), s)
    }

    "NNO's 10 element should be 9" >> {
      Sets.N must contain(1001590, "omg, what's wrong with the phone number?")
      val first10 = Sets.N.take(10)
      for (i <- 0 to 9) first10 must contain(i, "what's wrong with " + i + "? ")
    }

    "Iterator of 1 is asingleton" >> {
      Sets.isSingleton(List("abc").iterator))
    }

    "Iterator of 0 is not a singleton" >> {
      !Sets.isSingleton(List().iterator))
    }

    "Iterator of 2 is not a singleton" >> {
      !Sets.isSingleton(List("a", "b").iterator))
    }

    "Factorset" >> {
      val set:Set[Any] = setOf(1 to 10)
      def isOdd(x: Any) = x.toString.charAt(0) % 2 == 0
      val br: BinaryRelationship[Any, Any] = ((a: Any, b: Any) => isOdd(a) == isOdd(b))
      val factoring = new FactorSet(set, br)
      factoring.factorset must haveSize(2, "actual " + factoring.factorset.size)
      val s = Array(Set(2, 4, 6, 8), Set(1, 3, 5, 7, 9, 10))
      val factor = Set(s(1), s(0))
      factor == factoring.factorset, "got " + factoring.factorset)
      assertEquals(s(0), factoring.asFunction(6))
      assertEquals(s(1), factoring.asFunction(7))
      }

    "Factorset by a diagonal" >> {
      val set = setOf(1 to 10)
      val br: BinaryRelationship[Int, Int] = ((a: Int, b: Int) => a == b)
      val actual = factorset(set, br)
      val factor = setOf(for (i <- set) yield Set(i))
      assertEquals(SetMorphism(set, factor, (i:Int) => Set(i)), actual)
    }

    "Factorset mod 2" >> {
      val set = setOf(1 to 10)
      val br: BinaryRelationship[Int, Int] = ((a: Int, b: Int) => a % 2 == b % 2)
      val actual = factorset(set, br)
      val s = Array(Set(2, 4, 6, 8, 10), Set(1, 3, 5, 7, 9))
      val factor = Set(s(1), s(0))
      assertEquals(SetMorphism(set, factor, (i:Int) => s(i % 2)), actual)
    }

    "Set(iterable, size, filter) should not return false positives" >> {
      val s = Set(1, 2, 3)
      def predicate = (x:Any) => x == 2
      val sut = setOf(s, 1, predicate)
      !sut.contains(1))
      val collected = (Set.empty[Any] /: sut.iterator) (_+_)
      assertEquals(Set(2), collected)
    }

    "Pullback, plain case" >> {
      val xs = Set(1,2,3,4,5)
      val ys = Set(2,4,6,8,10)
      val actual = pullback(xs, ys, (x:Int) => x/2, (y:Int) => y/5)
      val expected = Set((1,2),(1,4),(2,6),(2,8),(3,6),(3,8),(4,10),(5,10))
      actual === expected
    }

    "Finite Sets should not not contain NNO" >> {
      !(FINITE_SETS must contain(N))
    }

    "Finite Sets should not contain itself" >> {
      !(FINITE_SETS must contain(FINITE_SETS))
    }

    "Finite Sets should contain various finite sets" >> {
      FINITE_SETS must contain(Set[String]())
      FINITE_SETS must contain(Set("infinity"))
      FINITE_SETS must contain(Set(1,2,3,42))
    }
  */
  }
}
