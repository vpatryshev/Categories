package math.cat

import org.specs2.mutable._
import Pair._

/**
 * Tests for math.cat.Pair class
 */
class PairTest extends Specification {
  "Pair" >> {
    
    "withLeft" >> {
      def sut[Y] = Pair.withLeft[String, Y]("hello")
      sut(42) should_== ("hello", 42)
      sut("world") should_== ("hello", "world")
    }

    "withRight" >> {
      def sut[X] = Pair.withRight[X, String]("hello")
      sut(42) should_== (42, "hello")
      sut("Kitty") should_== ("Kitty", "hello")
    }
    
  }
  
}
