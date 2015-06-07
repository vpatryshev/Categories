package math.cat

import org.specs2.mutable._

/**
 * Test suite for Functions object
 * @author vpatryshev
 */
class FunctionsTest extends Specification {
/*
  test("your test sucks") {
    var n = 0
    def f(x: Int) = { n += 1; x+1}
    expect(0) { n }
  }

  test("lazy pair should not call its function until asked") {
    var n = 0
    def f(x: Int) = {n += 1; x+1; throw new RuntimeException("If the pair were lazy, this exception would not happen")}
    val p = LazyPair(123, f)
  }

  test("lazy pair should call its function once") {
    var n = 0
    def f(x: Int) = { n += 1; x+1}
    val p = LazyPair(123, f)
    expect(0) { n }
    expect(123) { p._1 }
    expect(0) { n }
    expect(124) { p._2 }
    expect(1) { n }
    expect(124) { p._2 }
    expect(1) { n }
  }

  test("lazy pair should call its function just once") {
    var n = 0
    def f(x: Int) = { n += 1; x+1}
    val p = LazyPair(123, f)
    expect(0) { n }
    expect(124) { p._2 }
    expect(1) { n }
    expect(124) { p._2 }
    expect(1) { n }
  }

  test("injection applied to a set should produce a set of the same size") {
    val set = Set("a", "b", "cdef")
    val f = injection{(s: String) => s + "!"}
    assert(Set("a!", "b!", "cdef!") == f.applyTo(set))
  }

  test("injection after injection is still an injection") {
    val f = injection{(s: String) => s + "!"}
    val g = injection{(s: String) => s + "?!"}
    val fg: Injection[String, String] = f andThen g
  }

  test("inclusion should be practically usable") {
    val f = inclusion[Integer, Number]
    val n:Number = 1
    assert(n == f(1))
  }

  test("Schwartzian transform as defined in Wikipedia") {
    val f = schwartzianTransform {(s: String) => s.toUpperCase}
    assert(Set(("aX", "AX"), ("mmm", "MMM")) == f.applyTo(Set("aX", "mmm")))
  }

  */
}
