package math.sets

import math.sets.Sets.*
import org.specs2.execute.Failure
import scalakittens.Good
import testing.TestBase
import scala.language.implicitConversions

import scala.collection.immutable.Set as ===

/**
 * Test suite for Sets object
 */
class SetsTest extends TestBase:

  "Set Parser" should:
    "parse the string" in :
      Sets.parse("{a, bc, def, more}") must be_==(Good)(Set("a", "bc", "def", "more"))

    "parse({a,b,c} with spaces" in :
      Sets.parse("  { a , b,  c} ") must be_==(Good)(Set("a", "b", "c"))

    "parse empty to an empty set" in :
      Sets.parse("{}").map(_.isEmpty) must be_==(Good)(true)
      Sets.parse(" {  } ").map(_.isEmpty) must be_==(Good)(true)

    "parse singleton producing singleton" in :
      Sets.parse("{0}") must be_==(Good)(Set("0"))
      Sets.parse("{xyz}") must be_==(Good)(Set("xyz"))
      Sets.parse("{ xyz }") must be_==(Good)(Set("xyz"))

    "parse without closing curly should throw an exception" in :
      try
        val x = Sets.parse("{a, b, c")
        failure("Should have thrown an exception")
      catch
        case e: Exception => // as designed

      ok

  "parse without opening curly should throw an exception" in :
    try
      val x = Sets.parse("a, b, c}")
      failure("Should have thrown an exception")
    catch
      case e: Exception => // as designed

    ok

  "parse without nothing between commas should throw an exception" in :
    try
      val x = Sets.parse("{a, b,, c}")
      failure("Should have thrown an exception")
    catch
      case e: Exception => // as designed

    ok

  "sets" should:

    "group via groupBy" in :
      val xs = Set(2, 3, -2, 5, 6, 7)
      val f = groupBy(xs, (n: Int) => n * n)
      f(1) must be_==(Set.empty)
      f(2) must be_==(Set.empty)
      f(3) must be_==(Set.empty)
      f(4) must be_==(Set(2, -2))

    "group via groupedBy" in :
      val xs = Set(2, 3, -2, 5, 6, 7)
      val m = groupedBy(xs, (n: Int) => n * n)
      m(1) must be_==(Set.empty)
      m(2) must be_==(Set.empty)
      m(3) must be_==(Set.empty)
      m(4) must be_==(Set(2, -2))

  "Iterable" should:

    "produce a set" in :
      val source = List("one", "two", "three", "")
      val actual = setOf(source, 4, source.contains(_: String))
      val expected = Set("one", "two", "three", "")
      actual("four") must beFalse
      actual("two") must beTrue
      actual must be_==(expected)
      actual("two") must beTrue

    "not call lazy iterator when building a set" in :
      val source = List("one", "two", "three", "")
      var iteratorCalled = false

      val iterable = new Iterable[String]:
        override def iterator: Iterator[String] =
          iteratorCalled = true
          source.iterator

      val actual = setOf(iterable, 4, (x: String) => source contains x)
      iteratorCalled must beFalse
      val expected = Set("one", "two", "three", "")
      actual("two") must beTrue

    "build from a list" in :
      val source = List("one", "two", "three", "")
      val expected = Set("one", "two", "three", "")
      val actual: Set[String] = setOf[String](source,  expected)
      actual must be_==(expected)

    "accept infinite sets" in :
      val iterable: Iterable[Int] = new Iterable[Int]:
        def iterator: Iterator[Int] = new Iterator[Int]:
          private var i = -1

          def next(): Int =
            i += 1
            i

          def hasNext = true

          def remove: Failure = failure("N is immutable")

      val s = setOf(iterable, Int.MaxValue)
      val isInfinite = s.isInfinite
      isInfinite must beTrue
      val s1 = s.take(5)
      s1.isInfinite must beFalse
      // the following matcher does not work, because our set is infinite, and the test tries to build a vector
      //      s must contain(42)
      s.contains(42) must beTrue
      var n = 0
      for (i <- s take 10)
        n += 1

      n must be_==(10)

    "range(1,2,3) should be set(1)" in :
      val r = range(1, 2, 3)
      r must haveSize(1)

    "only contain stuff" in :
      setOf.elements(1,2,3).contains(1) must beTrue
      setOf.elements(1,2,3).contains(2) must beTrue
      setOf.elements(1,2,3).contains(3) must beTrue
      setOf.elements(1,2,3).contains(4) must beFalse

  "range" should:
    "produce set(1) for range(1,3,2)" in :
      val r = range(1, 3, 2)
      r must haveSize(1)
      r must be_==(setOf).elements(1)

    "produce set(0, 2) for range(0,3,2)  " in :
      val r = range(0, 3, 2)
      r must haveSize(2)
      r must be_==(setOf).elements(0, 2)

  "union" should:
    "work for a list of sets" in :
      val sets = (1 to 5) map (n => setOf[Int]((10 * n) to (10 * n) + n))
      val expected = Set(10, 11, 20, 21, 22, 30, 31, 32, 33, 40, 41, 42, 43, 44, 50, 51, 52, 53, 54, 55)
      val actual = union(sets)
      actual must be_==(expected)

    "work for a finite with an infinite set" in :
      val src = N.untyped :: Set("a", "b").untyped :: Nil
      val sut = union(src)
      sut.size must be_==(InfiniteSize)
      sut(BigInt(42)) must beTrue
      sut(42) must beFalse
      sut("b") must beTrue

    "take n of infinite should not hang" in :
      val sut = N take 5
      sut must be_==(Set(0, 1, 2, 3, 4))

    "should include both finite and infinite" in :
      val sut = union(Set("a", "b", 1.4), N) take 20
      sut must contain("a")
      sut must contain("b")
      sut must contain(8)

    "should include bot for an infinite and a finite" in :
      val sut = union(N, Set("a", "b", 3.5))
      val sutShort = sut take 20
      sutShort must contain("a")
      sutShort must contain("b")
      sutShort must contain(8)

    "should include bot infinites" in :
      val set1 = N filter (x => x % 5 == 0)
      val set2 = N filter (x => x % 5 == 2)
      val sut: Set[BigInt] = union(set1, set2)
      Set(BigInt(15), BigInt(22)).subsetOf(sut) must beTrue
      // can't use must contain(), since sets are infinite
      sut.contains(15) must beTrue
      sut.contains(22) must beTrue

    "shoulod have the size equal to the sum of sizes" in :
      union(Set(1, 2), Set(1, 2, 3)).size must be_==(5)

    "should have the size Int.MaxValue when an infinte is involved" in :
      union(N, N) must haveSize(Int.MaxValue)
      union(Set(1, 2), N) must haveSize(Int.MaxValue)
      union(N, Set(1, 2)) must haveSize(Int.MaxValue)

  "product" should:

    "be empty if one component is empty" in :
      product2(Set("a", "b", "c"), Set.empty).isEmpty must beTrue
      product2(Set.empty, Set("a", "b", "c")).isEmpty must beTrue

    "have 4 elements for a 2 x 2" in :
      val actual = product2(Set("a", "b"), Set("A", "B"))
      val expected = Set(("a", "A"), ("a", "B"), ("b", "A"), ("b", "B"))
      actual must be_==(expected)
      actual.toString === "Set((a,A), (a,B), (b,A), (b,B))"
      expected must be_==(actual)

    "have 6 elements for a 3 x 2" in :
      val actual = product2(Set("a", "b", "c"), Set("A", "B"))
      val expected = Set(("a", "A"), ("a", "B"), ("b", "A"), ("b", "B"), ("c", "A"), ("c", "B"))
      actual must be_==(expected)
      expected must be_==(actual)

    "have 6 elements for a 2 x 3" in :
      val actual = product2(Set("a", "b"), Set("A", "B", "C"))
      val expected = Set(("a", "A"), ("a", "B"), ("a", "C"), ("b", "A"), ("b", "B"), ("b", "C"))
      actual must be_==(expected)
      expected must be_==(actual)

    "not prematurely calculate size" in :
      var wasCalled = false
      val source = Set(1,2)
      val s = setOf(source, {wasCalled = true; 2}, (x: Int) => source contains x)

      wasCalled must beFalse
      val ss = product2(s, s)
      wasCalled must beFalse
      ss must haveSize(4)
      wasCalled must beTrue

    "iterate over all pairs for an infinite set and a finite one" in :
      val sut = product2(N, Set('A', 'B'))
      val segment = sut take 30
      segment.contains((4,'B')) must beTrue
      segment.contains((2,'A')) must beTrue

    "iterate over all pairs for a finite set with an infinite" in :
      val sut = product2(Set("a", "b"), N)
      val segment = sut take 20
      segment.contains(("b",5)) must beTrue
      segment.contains(("a",3)) must beTrue

    "iterate over all pairs of two infinite sets" in :
      val sut = product2(N, N)
      val segment = sut take 40
      segment must contain((1,3))
      segment must contain((3,1))
      segment must contain((3,3))

    "be empty for an empty list of components" in :
      val source = List()
      val expected = Set(List())
      val actual = product(source)
      actual must be_==(expected)

    "give a list of singletons for a singleton list (of set)" in :
      val mySet = Set(2220, 2221, 2222, 2223)
      val source = List(mySet)
      val expected = mySet map (x =>List(x))
      val actual = product(source)
      actual must be_==(expected)

    "be a cartesian product of two sets" in :
      val source = List(Set("a", "b", "c"), Set("1", "2"))
      val expected = Set(
        List("a", "1"), List("a", "2"),
        List("b", "1"), List("b", "2"),
        List("c", "1"), List("c", "2")
      )
      val actual = product(source)
      actual must be_==(expected)

    "be a cartesian product product of several sets" in :
      val source = List(Set("a", "b", "c"), Set("1", "2"), Set("Ebony", "Ivory"))
      val expected = Set(
        List("a", "1", "Ebony"), List("a", "1", "Ivory"), List("a", "2", "Ebony"), List("a", "2", "Ivory"),
        List("b", "1", "Ebony"), List("b", "1", "Ivory"), List("b", "2", "Ebony"), List("b", "2", "Ivory"),
        List("c", "1", "Ebony"), List("c", "1", "Ivory"), List("c", "2", "Ebony"), List("c", "2", "Ivory")
      )
      val actual = product(source)
      actual must be_==(expected)

    "be empty if one component is empty" in :
      val source = List(Set("a", "b", "c"), Set.empty, Set("Ebony", "Ivory"))
      val actual = product(source)
      actual must be_==(Set.empty)

  "Powerset" should:
    "get 8 elements for a 3-element set" in :
      val s = Set("a", "b", "c")
      val actual = pow(s)
      val expected = Set(Set.empty, Set("a"), Set("b"), Set("c"), Set("a", "b"), Set("a", "c"), Set("b", "c"), s)
      actual must be_==(expected)

  "exponent" should:
    "give 9 maps from {x,y,z}^{1,2}" in :
      val domain = Set("1", "2")
      val codomain = Set("x", "y", "z")
      val actual = exponent(domain, codomain)
      actual must haveSize(9)
      actual must contain(Map("1" -> "y", "2" -> "x"))

  "Other tests" should:
    "toString should work as expected" in :
      val sut = Set("a", "b", "c")
      Sets.toString(sut) === "{a, b, c}"

    "Filtering 1..5 by oddity should produce 1,3,5" in :
      val sut = Set(1, 2, 3, 4, 5)
      val actual : Set[Int] = sut.filter(x => x % 2 == 1)
      actual must be_==(Set(1, 3, 5))

    "Filtering product by diagonal should produce diagonal" in :
      val sut = product2(Set(1, 2, 3), Set(1, 2, 3))
      val p1 = (1, 1)
      val p2 = (2, 2)
      val p3 = (3, 3)
      val actual = sut.filter(p => {val (x, y) = p; x  == y})
      actual must be_==(Set(p1, p2, p3))

    "Set should be monadic" in :
      val i: Set[Int] = Set(1, 2, 3)
      val s: Set[String] = for (n <- i) yield n.toString
      s must be_==(Set("1", "2", "3"))

    "NNO's 10th element should be 9" in :
      N.contains(7688721) must beTrue
      val first10 = N.take(10)

      0 to 9 forall (i => first10 must contain(i))

      ok

    "Iterator of 1 is a singleton" in :
      Sets.isSingleton(List("abc")) must beTrue

    "Iterator of 0 is not a singleton" in :
      Sets.isSingleton(List()) must beFalse

    "Iterator of 2 is not a singleton" in :
      Sets.isSingleton(List("a", "b")) must beFalse

    "Factorset" in :
      val s = setOf[Int](1 to 10)
      def isOdd(x: Int) = x % 2 == 0
      val br: BinaryRelation[Int, Int] = (a: Int, b: Int) => isOdd(a) == isOdd(b)
      val factoring = new FactorSet(s, br)

      factoring.content must haveSize(2)
      val twoSets = Array(Set(2, 4, 6, 8, 10), Set(1, 3, 5, 7, 9))
      val factor = Set(twoSets(1), twoSets(0))
      factor must be_==(factoring).content
      factoring.asFunction(6) must be_==(twoSets)(0)
      factoring.asFunction(7) must be_==(twoSets)(1)

    "Pullback, plain case" in :
      val xs = Set(1,2,3,4,5)
      val ys = Set(2,4,6,8,10)
      val actual = pullback(xs, ys, (x:Int) => x/2, (y:Int) => y/5)
      val expected = Set((1,2),(1,4),(2,6),(2,8),(3,6),(3,8),(4,10),(5,10))
      actual must be_==(expected)

    "Finite Sets should not not contain NNO" in :
      // may not even compile, good
      //      FiniteSets.contains(N) must beFalse
      ok

    "Finite Sets should not contain itself" in :
      // Will not even compile, which is good! good (assertDoesNotCompile("..")
      // FiniteSets.contains(FiniteSets) must beFalse
      ok

    "Finite Sets should contain various finite sets" in :
      FiniteSets.contains(Sets.`∅`) must beTrue
      FiniteSets.contains(Set("infinity")) must beTrue
      FiniteSets.contains(Set(1,2,3,42)) must beTrue

    "setForIterable" in :
      val s00 = List(1,2,3,4,5,6,7,8,9,10)
      val s10 = List(1,2,3,4,5,6,7,8,9,10)
      val s20 = List(1,2,3,4,5,6,7,8,9,10,11)
      val s0 = SetForIterable[Any](s00, 5, _ => true)
      val s1 = SetForIterable[Any](s10, 5, _ => true)
      val s2 = SetForIterable[Any](s20, 5, _ => true)
      val s3 = SetForIterable[Any](s20, 11, _ => true)
      val s4 = SetForIterable[Any](s20, 3, (x:Any) => x.toString.toInt % 2 == 0)
      s0 must be_==(s1)
      s1 must be_==(s0)
      s2 must be_==(s1)
      s3 !== s0
      s0 !== s3
      s0 contains 5 must beTrue
      s4 contains 4 must beTrue
      s4 contains 5 must beFalse
      s4 contains 6 must beTrue
//      s0 contains 12 must beFalse
//      s0 contains "1" must beFalse
//      s0 contains 6 must beFalse
//      s4 contains 8 must beFalse

  "setOf" should:

    "should be good" in :
      val source = List("one", "two", "three", "")
      setOf[String](source) must be_==(Set("one", "two", "three", ""))

    "calculate size if not provided" in :
      val source = List("one", "two", "three", "")
      val actual = setOf(source, (s: String) => source.contains(s))
      val expected = Set("one", "two", "three", "")
      actual must be_==(expected)

    "behave in general" in :
      val s00 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val s10 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val s20 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
      val s0 = setOf[Any](s00, _ => true)
      val s1 = setOf[Any](s10, 5, _ => true)
      val s2 = setOf[Any](s20, 5, _ => true)
      val s3 = setOf[Any](s20, 11, _ => true)
      val s4 = setOf[Any](s20, 3, (x: Any) => x.toString.toInt % 2 == 0)
      s0 !== s1
      s1 !== s0
      s2 must be_==(s1)
      s3 !== s0
      s0 !== s3
      s0 must contain(1)
      s0 must contain(6)
      s4 must contain(4)
      s4 contains 5 must beFalse
      s4 must contain(6)
//      s0 contains 12 must beFalse
//      s0 contains "1" must beFalse
//      s4 contains 8 must beFalse

  "not return false positives" in :
    val s = Set(1, 2, 3)

    def predicate = (x: Any) => x == 2

    val sut = setOf(s, 1, predicate)
    sut contains 1 must beFalse
    val collected = sut.iterator.foldLeft(Sets.`∅`)(_ + _)
    collected must be_==(Set(2))
