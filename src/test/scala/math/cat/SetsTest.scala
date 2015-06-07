package math.cat

import org.specs2.mutable._


/**
 * Test suite for Sets object
 */
class SetsTest extends Specification {
/*
  test("parsing plain set") {
    val actual = Sets.parse("{a, bc, def, ghij}")
    val expected = Set("a", "bc", "def", "ghij")
    assertEquals(expected, actual)
    assertEquals(expected, actual)
  }

  test("groupbBy should group by") {
    val xs = Set(2, 3, -2, 5, 6, 7)
    val ys = Set(1, 2, 3, 4, 5, 6)
    val f = groupBy(xs, ys, (n: Int) => (n * n))
    assertEquals(Set.empty, f(1))
    assertEquals(Set.empty, f(2))
    assertEquals(Set.empty, f(3))
    assertEquals(Set(2, -2), f(4))
  }

  test("Iterable should produce a set") {
    val source = List("one", "two", "three", "")
    val actual : Set[String] = Sets.setOf[String](source: Iterable[String], 4, source.contains(_))
    val expected = Set("one", "two", "three", "")
    assertEquals(expected, actual)
  }  

  test("Lazy iterator should not be called when building a set") {
    val source = List("one", "two", "three", "")
    var iteratorCalled = false

    val iterable = new Iterable[String] {
      override def iterator = {
        iteratorCalled = true;
        source.iterator
      }
    }

    val actual : Set[String] = setOf(iterable.iterator, 4, (x: String) => source contains x)
    assert(!iteratorCalled)
    val expected = Set("one", "two", "three", "")
    assertEquals(expected, actual)
  }  

  test("setOf(Iterable) should calculate size if not provided") {
    val source = List("one", "two", "three", "")
    val actual : Set[String] = setOf(source, (s: String) => source.contains(s))
    val expected = Set("one", "two", "three", "")
    assertEquals(expected.size, actual.size)
    assert(actual contains "one")
    assert(actual contains "two")
    assert(actual contains "three")
    assert(actual contains "")
    assertEquals(expected, actual)
  }

  test("setOf(Iterable) should be good") {
    val source = List("one", "two", "three", "")
    val actual : Set[String] = setOf(source)
    val expected = Set("one", "two", "three", "")
    assertEquals(expected, actual)
  }

  test("infinite set should be okay") {
    val iterable = new Iterable[Int] {
      def iterator = new Iterator[Int] {
        var i = 0;
        def next = { i += 1; i - 1 }
        def hasNext = true
        def remove = error("N is immutable")
      }
    }

    val set = setOf(iterable, (n:Int) => true)
    assert(set contains 42)
    var n = 0
    for (i <- set take 10) {
      n += 1
    }
    assert(n == 10)
  }

  test("range should produce a good set") {
    assertEquals(setOf(List(1)), range(1, 2, 3))
    assertEquals(setOf(List(1)), range(1, 3, 2))
    assertEquals(setOf(List(0, 2)), range(0, 3, 2))
  }

  test("setOf from a vector should produce the right stuff") {
    val source = Vector(10, 11, 20, 21, 22, 30, 31, 32, 33, 40, 41, 42, 43, 44, 50, 51, 52, 53, 54, 55)
    val actual = setOf(source)
    val expected = Set(10, 11, 20, 21, 22, 30, 31, 32, 33, 40, 41, 42, 43, 44, 50, 51, 52, 53, 54, 55)
    assertEquals(expected, actual)
  }

  test("union of non-intersecting sets should be okay") {
    val a = Set("a", "b", "c")
    val b = Set("d", "e")
    val actual : Set[String] = union(a, b)
    val expected = Set("a", "b", "c", "d", "e")
    assertEquals(expected, actual)
  }

  test("union of a list of sets") {
    val sets = (1 to 5) map (n => setOf((10 * n) to (10 * n) + n))
    val expected = Set(10, 11, 20, 21, 22, 30, 31, 32, 33, 40, 41, 42, 43, 44, 50, 51, 52, 53, 54, 55)
    val actual = union(sets)
    assertEquals(expected, actual)
  }

  test("take n of infinite should not hang") {
    val sut = N take 5
    assertEquals(Set(0,1,2,3,4), sut)
  }

  test("union of a finite with an infinite should cover both") {
    val sut = union(Set("a","b", 1.4), N.asInstanceOf[Set[Any]]) take 20
    assert(sut contains "a")
    assert(sut contains "b")
    assert(sut contains 8)
  }

  test("union of an infinite with a finite should cover both") {
    val sut = union(N.asInstanceOf[Set[Any]], Set("a","b", 3.5)) take 20
    assert(sut contains "a")
    assert(sut contains "b")
    assert(sut contains 8)
  }

  test("union of an infinite with an infinite should cover both") {
    val sut = union(N filter(x => x % 5 == 0), N filter(x => x % 5 == 2)) take 20
    assert(sut contains 15)
    assert(sut contains 22)
    assert(sut contains 0)
  }

  test("union of two finite sets should have the size equal to their sum of sizes") {
    assert(union(Set(1, 2), Set(1, 2, 3)).size == 5)
  }

  test("union of an infinite with any other should have the size Integer.MAX_VALUE") {
    assert(union(N, N).size == Integer.MAX_VALUE)
    assert(union(Set(1, 2), N).size == Integer.MAX_VALUE)
    assert(union(N, Set(1, 2)).size == Integer.MAX_VALUE)
  }

  test("product with empty should produce empty") {
    assert(product(Set("a", "b", "c"), Set.empty) isEmpty)
    assert(product(Set.empty, Set("a", "b", "c")) isEmpty)
  }

  test("product of 2 x 2 should produce a 6-element set") {
    val actual : Set[(String, String)] = product(Set("a", "b"), Set("A", "B"))
    val expected = Set(("a", "A"), ("a", "B"), ("b", "A"), ("b", "B"))
    assertEquals(expected, actual)
    assertEquals(actual, expected)
  }

  test("product of 3 x 2 should produce a 6-element set") {
    val actual : Set[(String, String)] = product(Set("a", "b", "c"), Set("A", "B"))
    val expected = Set(("a", "A"), ("a", "B"), ("b", "A"), ("b", "B"), ("c", "A"), ("c", "B"))
    assertEquals(expected, actual)
    assertEquals(actual, expected)
  }

  test("product of 2 x 3 should produce a 6-element set") {
    val actual : Set[(String, String)] = product(Set("a", "b"), Set("A", "B", "C"))
    val expected = Set(("a", "A"), ("a", "B"), ("a", "C"), ("b", "A"), ("b", "B"), ("b", "C"))
    assertEquals(expected, actual)
    assertEquals(actual, expected)
  }

  test("product should not prematurely calculate size") {
    var wasCalled = false
    val source = Set(1,2)
    val s = setOf(source, {wasCalled = true; 2}, (x: Int) => (source contains x))
    assert(!wasCalled)
    val ss = product(s, s)
    assert(!wasCalled)
    assertEquals(4, ss.size)
    assert(wasCalled)
  }

  test("product of an infinite set with a finite should iterate over all pairs") {
    val sut = product(N, Set('A, 'B))
    val segment = sut take 30
    System.out.flush
    assert(segment contains(4,'B), "got " + segment)
    assert(segment contains(2,'A))
  }

  test("product of a finite set with an infinite should iterate over all pairs") {
    val sut = product(Set("a", "b"), N)
    val segment = sut take 20
    assert(segment contains("b",5))
    assert(segment contains("a",3))
  }

  test("product of two infinite sets should iterate over all pairs") {
    val sut = product(N, N)
    val segment = sut take 40
    assert(segment contains(1,3))
    assert(segment contains(3,1))
    assert(segment contains(3,3))
  }

  test("powerset of a 3-element set should give 8 elements") {
    val set = Set("a", "b", "c")
    val actual : Set[Set[String]] = powerset(set)
    val expected = Set(Set.empty, Set("a"), Set("b"), Set("c"), Set("a", "b"), Set("a", "c"), Set("b", "c"), set)
    assertEquals(expected, actual)
  }

  test("split should split") {
    val sut = Set("a1", "b2", "c3")
    val (x, xs) = split(sut)
    assert(x == "a1")
    val i = xs.iterator
    assert(i.next == "b2")
    assert(i.next == "c3")
    assert(!i.hasNext)
  }

  test("{x,y,z}^{1,2} should give a 9-element set of maps") {
    val domain = Set("1", "2")
    val codomain = Set("x", "y", "z")
    val actual : Set[Map[String, String]] = exponent(domain, codomain)
    assert(9 == actual.size, "Expected 9, got " + actual.size)
    assert(actual contains Map("1" -> "y", "2" -> "x"))
  }

  test("parse({a,b,c} with spaces should be okay") {
    assertEquals(Set("a", "b", "c"), Sets.parse("  { a , b,  c} "))
  }

  test("parse empty should produce empty set") {
    assert(Sets.parse("{}").isEmpty)
    assert(Sets.parse(" {  } ").isEmpty)
  }

  test("Parse singleton should produce singleton") {
    assertEquals(Set("0"), Sets.parse("{0}"))
    assertEquals(Set("xyz"), Sets.parse("{xyz}"))
    assertEquals(Set("xyz"), Sets.parse("{ xyz }"))
  }

  test("Parse without closing curly should throw an exception") {
    try {
      val x : Set[String] = Sets.parse("{a, b, c")
      failure("Should have thrown an exception")
    } catch {
      case e: Exception => println(e) // as designed
    }
  }

  test("Parse without opening curly should throw an exception") {
    try {
      val x : Set[String] = Sets.parse("a, b, c}")
      failure("Should have thrown an exception")
    } catch {
      case e: Exception => println(e) // as designed
    }
  }

  test("Parse without nothing between commas should throw an exception") {
    try {
      val x : Set[String] = Sets.parse("{a, b,, c}")
      failure("Should have thrown an exception")
    } catch {
      case e: Exception => println(e) // as designed
    }
  }

  test("oString should work as expected")
  {
    val sut = Set("a", "b", "c")
    assertEquals("{a, b, c}", Sets.toString(sut))
  }

  test("Filtering 1..5 by oddity should produce 1,3,5")
  {
    val sut = Set(1, 2, 3, 4, 5)
    val actual : Set[Int] = sut.filter(x => x % 2 == 1)
    assertEquals(Set(1, 3, 5), actual)
  }

  test("Filtering product by diagonal should produce diagonal")
  {
    val source = product(Set(1, 2, 3), Set(1, 2, 3))
    val sut : Set[(Int, Int)] = product(Set(1, 2, 3), Set(1, 2, 3))
    assertEquals(source, sut)
    val p1 = (1, 1)
    val p2 = (2, 2)
    val p3 = (3, 3)
    val expected = Set(p1, p2, p3)
    val actual : Set[(Int, Int)] = sut.filter(p => {val (x, y) = p; x  == y})
    assert(!(actual contains (1, 2)))
    assert(actual.size == 3, "actually, size is " + actual.size)
    assert(actual contains p1)
    assert(actual contains p2)
    assert(actual contains p3)
    assertEquals(expected, actual)
  }

  def doNotTestDownshift() {
    val source: Set[Any] = Set("abc", 123, 4.56, Set("qqq"), "zzz")
    val target: Set[String] = downshift(source)
    assertEquals(Set("abc", "zzz"), target)
  }

  test("Set should be monadic") {
    val i: Set[Int] = Set(1, 2, 3)
    val s: Set[String] = for (n <- i) yield(n.toString)
    assertEquals(Set("1", "2", "3"), s)
  }

  test("NNO's 10 element should be 9") {
    assert(Sets.N contains 1001590, "omg, what's wrong with the phone number?")
    val first10 = Sets.N.take(10)
    for (i <- 0 to 9) assert(first10 contains i, "what's wrong with " + i + "? ")
  }

  test("Iterator of 1 is asingleton") {
    assert(Sets.isSingleton(List("abc").iterator))
  }

  test("Iterator of 0 is not a singleton") {
    assert(!Sets.isSingleton(List().iterator))
  }

  test("Iterator of 2 is not a singleton") {
    assert(!Sets.isSingleton(List("a", "b").iterator))
  }

  test("Factorset") {
    val set:Set[Any] = setOf(1 to 10)
    def isOdd(x: Any) = x.toString.charAt(0) % 2 == 0
    val br: BinaryRelationship[Any, Any] = ((a: Any, b: Any) => isOdd(a) == isOdd(b))
    val factoring = new FactorSet(set, br)
    assert(factoring.factorset.size == 2, "actual " + factoring.factorset.size)
    val s = Array(Set(2, 4, 6, 8), Set(1, 3, 5, 7, 9, 10))
    val factor = Set(s(1), s(0))
    assert(factor == factoring.factorset, "got " + factoring.factorset)
    assertEquals(s(0), factoring.asFunction(6))
    assertEquals(s(1), factoring.asFunction(7))
    }

  test("Factorset by a diagonal") {
    val set = setOf(1 to 10)
    val br: BinaryRelationship[Int, Int] = ((a: Int, b: Int) => a == b)
    val actual = factorset(set, br)
    val factor = setOf(for (i <- set) yield Set(i))
    assertEquals(SetMorphism(set, factor, (i:Int) => Set(i)), actual)
  }

  test("Factorset mod 2") {
    val set = setOf(1 to 10)
    val br: BinaryRelationship[Int, Int] = ((a: Int, b: Int) => a % 2 == b % 2)
    val actual = factorset(set, br)
    val s = Array(Set(2, 4, 6, 8, 10), Set(1, 3, 5, 7, 9))
    val factor = Set(s(1), s(0))
    assertEquals(SetMorphism(set, factor, (i:Int) => s(i % 2)), actual)
  }

  test("Set(iterable, size, filter) should not return false positives") {
    val s = Set(1, 2, 3)
    def predicate = (x:Any) => x == 2
    val sut = setOf(s, 1, predicate)
    assert(!sut.contains(1))
    val collected = (Set.empty[Any] /: sut.iterator) (_+_)
    assertEquals(Set(2), collected)
  }

  test("Pullback, plain case") {
    val xs = Set(1,2,3,4,5)
    val ys = Set(2,4,6,8,10)
    val actual = pullback(xs, ys, (x:Int) => x/2, (y:Int) => y/5)
    val expected = Set((1,2),(1,4),(2,6),(2,8),(3,6),(3,8),(4,10),(5,10))
    assertEquals(expected, actual)
  }

  test("Finite Sets should not not contain NNO") {
    assert(!(FINITE_SETS contains N))
  }

  test("Finite Sets should not contain itself") {
    assert(!(FINITE_SETS contains FINITE_SETS))
  }

  test("Finite Sets should contain various finite sets") {
    assert(FINITE_SETS contains Set[String]())
    assert(FINITE_SETS contains Set("infinity"))
    assert(FINITE_SETS contains Set(1,2,3,42))
  }
*/
}
