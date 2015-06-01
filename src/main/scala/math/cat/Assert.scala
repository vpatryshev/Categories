package math.cat

/**
 * @author vpatryshev
 *
 * A couple of better assertions
 */

trait Assert {
  def assertEquals(a: Any, b: Any) {
    assert(a == b, "expected " + a + ", actual " + b)
  }

  def assertEquals(a: Any, b: Any, message: String) {
    assert(a == b, message + ": expected " + a + ", actual " + b)
  }
}