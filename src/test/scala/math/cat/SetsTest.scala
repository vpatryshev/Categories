package math.cat

import org.specs2.mutable._
import Sets._
import org.specs2.execute.Failure

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
      val f = groupBy(xs, ys, (n: Int) => n * n)
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
        override def iterator: Iterator[String] = {
          iteratorCalled = true
          source.iterator
        }
      }

      val actual = Sets.setOf(iterable.iterator, 4, (x: String) => source contains x)
      iteratorCalled must beFalse
      val expected = Set("one", "two", "three", "")
      actual === expected
    }

    "building from a list should work" >> {
      val source = List("one", "two", "three", "")
      val actual: Set[String] = setOf[String](source, (s: String) => source.contains(s))
      val expected = Set("one", "two", "three", "")
      actual.size === expected.size
      actual.contains("one") must beTrue
      actual.contains("two") must beTrue
      actual.contains("three") must beTrue
      actual.contains("") must beTrue
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
      val iterable: Iterable[Int] = new Iterable[Int] {
        def iterator: Iterator[Int] = new Iterator[Int] {
          var i = -1

          def next: Int = {
            i += 1
            i
          }

          def hasNext = true

          def remove: Failure = failure("N is immutable")
        }
      }

      val set = setOf(iterable, (n: Int) => true)
      set.contains(42) must beTrue
// the following matcher does not work, because our set is infinite, and it tries to build a vector      
//      set must contain(42)
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
      val eq1 = actual == expected
      eq1 must beTrue
      val eq2 = actual == expected
      eq2 must beTrue
      actual === expected // fails for some reason
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
      val sut = union(N, Set("a", "b", 3.5)) take 20
      sut must contain("a")
      sut must contain("b")
      sut must contain(8)
    }

    "union of an infinite with an infinite should cover both" >> {
      val set1 = N filter (x => x % 5 == 0)
      val set2 = N filter (x => x % 5 == 2)
      val sut: Set[BigInt] = union(set1, set2)
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
      val s = Sets.setOf(source, {wasCalled = true; 2}, (x: Int) => source contains x)

      wasCalled must beFalse
      val ss = product2(s, s)
      wasCalled must beFalse
      ss must haveSize(4)
      wasCalled must beTrue
    }

    "product of an infinite set with a finite should iterate over all pairs" >> {
      val sut = product2(N, Set('A, 'B))
      val segment = sut take 30
      segment.contains((4,'B)) must beTrue
      segment.contains((2,'A)) must beTrue
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
      val sut = product2(Set(1, 2, 3), Set(1, 2, 3))
      val p1 = (1, 1)
      val p2 = (2, 2)
      val p3 = (3, 3)
      val expected = Set(p1, p2, p3)
      val actual = sut.filter(p => {val (x, y) = p; x  == y})
      actual must not contain((1, 2))
      actual must haveSize(3)
      actual must contain(p1)
      actual must contain(p2)
      actual must contain(p3)
      actual === expected
    }

    "Set should be monadic" >> {
      val i: Set[Int] = Set(1, 2, 3)
      val s: Set[String] = for (n <- i) yield n.toString
      s === Set("1", "2", "3")
    }

    "NNO's 10 element should be 9" >> {
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
      val set = setOf(1 to 10)
      def isOdd(x: Int) = x % 2 == 0
      val br: BinaryRelationship[Int, Int] = (a: Int, b: Int) => isOdd(a) == isOdd(b)
      val factoring = new FactorSet(set, br)

      factoring.factorset must haveSize(2)
      val s = Array(Set(2, 4, 6, 8, 10), Set(1, 3, 5, 7, 9))
      val factor = Set(s(1), s(0))
      factor === factoring.factorset
      factoring.asFunction(6) === s(0)
      factoring.asFunction(7) === s(1)
    }

    "Factorset by a diagonal" >> {
      val set = setOf(1 to 10)
      val br: BinaryRelationship[Int, Int] = (a: Int, b: Int) => a == b
      val actual: SetMorphism[Int, Set[Int]] = factorset(set, br)
      val factor = setOf(for (i <- set) yield Set(i))
      actual === SetMorphism[Int, Set[Int]](set, factor, (i:Int) => Set(i))
    }

    "Factorset mod 2" >> {
      val set = setOf(1 to 10)
      val br: BinaryRelationship[Int, Int] = (a: Int, b: Int) => a % 2 == b % 2
      val actual = factorset(set, br)
      val s = Array(Set(2, 4, 6, 8, 10), Set(1, 3, 5, 7, 9))
      val factor = Set(s(1), s(0))
      actual === SetMorphism[Int, Set[Int]](d0 = set, d1 = factor, function = (i:Int) => s(i % 2))
    }

    "Set(iterable, size, filter) should not return false positives" >> {
      val s = Set(1, 2, 3)
      def predicate = (x:Any) => x == 2
      val sut = setOf(s, 1, predicate)
      sut contains 1 must beFalse
      val collected = (Set.empty[Any] /: sut.iterator) (_+_)
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
      FiniteSets.contains(N) must beFalse
    }

    "Finite Sets should not contain itself" >> {
      FiniteSets.contains(FiniteSets) must beFalse
    }

    "Finite Sets should contain various finite sets" >> {
      FiniteSets.contains(Set[String]()) must beTrue
      FiniteSets.contains(Set("infinity")) must beTrue
      FiniteSets.contains(Set(1,2,3,42)) must beTrue
    }
    /*
        def doNotTestDownshift() {
          val source: Set[Any] = Set("abc", 123, 4.56, Set("qqq"), "zzz")
          val target: Set[String] = downshift(source)
          target === Set("abc", "zzz")
        }

      */
  }
}
/*
def spendNotMoreThan[T](time: Duration, extraTimePercent:Int = 1) = new {
    def on(op: => Result[T]): Result[T] = {
      import LockSupport._
      var res:Result[T] = Empty
      val millis = time.toMillis
      val finalDeadline = System.currentTimeMillis + millis * (100 + extraTimePercent) / 100 + 1
      val done = new AtomicBoolean(false)
      val worker = new Thread {
        override def run {
          try {
            res = op
            done.set(true)
          } catch {case ie: InterruptedException => }
        }
      }
      worker.setPriority(1)
      worker.start
      worker.join(time.toMillis)
      if (worker.isAlive) {
        worker.interrupt
        parkUntil(finalDeadline)
      }
      if (worker.isAlive) worker.stop
      if (done.get) res else Result.error(s"Timeout after $time")
    }
  }
 */
