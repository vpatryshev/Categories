package math.cat




import scala.collection.Set
import scala.collection.Set._
import Sets._
import TypelessSetMorphism._
import org.scalatest.FunSuite

/**
 * Test suite for Sets object
 */

class TypelessSetMorphismSuite extends FunSuite {

  test("building TypelessSetMorphism") {
    val sut = TypelessSetMorphism("test", Set(1, 2, "a"), Set("x1", "x2", "xa", 77), (x: Any) => "x" + x);
    assert(sut(1) == "x1")
    assert(sut("a") == "xa")
    try {
      sut(3)
      fail("3 is not in domain")
    } catch {
      case e: Exception => // praise the Lord!
    }
  }

  test("TypelessSetMorphism then another") {
    val x = Set(1, 2, "a")
    val y = Set("x1", "x2", "xa", 77)
    val z = Set(2, 28, x)
    val f = new TypelessSetMorphism("f", x, y, (x: Any) => "x" + x);
    val g = new TypelessSetMorphism("g", y, z, (y: Any) => y.toString.length)
    val sut = f then g
    assert(sut(1) == 2)
    assert(sut("a") == 2)
    try {
      sut(z)
      fail("3 is not in domain")
    } catch {
      case e: Exception => // praise the Lord
    }
  }

  test("TypelessSetMorphism before another") {
    val x = Set(1, 2, "a")
    val y = Set("x1", "x2", "xa", 77)
    val z = Set(2, 28, x)
    val f = new TypelessSetMorphism("f", x, y, (x: Any) => "x" + x);
    val g = new TypelessSetMorphism("g", y, z, (y: Any) => y.toString.length)
    val sut = g before f
    assert(sut.d0 == x)
    assert(sut.d1 == z)
    assert(f(1) == "x1")
    assert(sut(1) == 2)
    assert(sut("a") == 2)
    try {
      sut(z)
      fail("3 is not in domain")
    } catch {
      case e: Exception => // praise the Lord
    }
  }

  test("building a constant") {
    val s0 = Set(1, 2, "a")
    val s1 = Set("x1", "x2", "xa", 77)
    val sut = TypelessSetMorphism.constant(s0, s1, 77);
    assert(sut.d0 == s0)
    assert(sut.d1 == s1)
    for (x <- s0) assert(sut(x) == 77)
    try {
      sut(3)
      fail("3 is not in domain")
    } catch {
      case e: Exception => // praise the Lord!
    }
  }

  test("building a nonexistent constant") {
    try {
      val sut = TypelessSetMorphism.constant(Set(1, 2, "a"), Set("x1", "x2", "xa", 77), "xx");
      fail("xx is not in codomain")
    } catch {
      case e: Exception => // praise the Lord!
    }
  }

  test("building an inclusion") {
    val s0 = Set(1, 2, "a")
    val s1 = Set(0, 1, 2, "b", s0, "a")
    val sut = inclusion(s0, s1);
    assert(sut.d0 == s0)
    assert(sut.d1 == s1)
    for (x <- s0) assert(sut(x) == x)
    try {
      sut("b")
    } catch  {
      case e: Exception => // Hallelujah!
    }
  }

  test("building a predicate-based inclusion") {
    val s = Set(1, 2, 77, 90, 42, "1xya2")
    def predicate = (x:Any) => x.toString.apply(0) == '1'
    val sut = inclusion(s, predicate);
    assert(sut.d1 == s)
    assert(sut.d0 == (s filter predicate))
    for (x <- List(1, "1xya2")) assert(sut(x) == x)
    try {
      sut(2)
    } catch  {
      case e: Exception => // Hallelujah!
    }
  }

  test("building a unit") {
    val s = Set(1, 2, "a")
    val sut = unit(s);
    assert(sut.d0 == s)
    assert(sut.d1 == s)
    for (x <- s) assert(sut(x) == x)
    try {
      sut("b")
    } catch  {
      case e: Exception => // Hallelujah!
    }
  }

  test("for factorset") {
    val set:Set[Any] = setOf(1 to 10)
    def isOdd(x: Any) = x.toString.charAt(0) % 2 == 0
    val br: BinaryRelationship[Any, Any] = ((a: Any, b: Any) => isOdd(a) == isOdd(b))
    val factoring = new FactorSet(set, br)
    val s = Array(Set(2, 4, 6, 8), Set(1, 3, 5, 7, 9, 10))
    val sut = forFactorset(factoring)
    val factor = Set(s(1), s(0))
    assert(set == sut.d0, "got " + sut.d0)
    assert(factor == sut.d1, "got " + sut.d1)
    assert(s(0) == sut(8))
    assert(s(1) == sut(5))
  }
}