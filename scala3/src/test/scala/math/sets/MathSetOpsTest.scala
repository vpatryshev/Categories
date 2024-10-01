package math.sets

import math.cat.SetMorphism
import math.sets.MathSetOps.cantorIterator
import org.specs2.execute.Failure
import scalakittens.{Empty, Result}
import testing.TestBase

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.Duration

/**
 * Test suite for the Math Sets object
 */
class MathSetOpsTest extends TestBase:

  "SetOps" >> {
    "Cantor iterator should behave - 1" >> {
      val sut = cantorIterator(1::2::Nil, 'X'::Nil)
      sut.hasNext must beTrue
      sut.next() === (1, 'X')
      sut.hasNext must beTrue
      sut.next() === (2, 'X')
      sut.hasNext must beFalse
      ok
    }
    "Cantor iterator should behave - 2" >> {
      val sut = cantorIterator(1::Nil, 'a'::'b'::Nil)
      sut.hasNext must beTrue
      sut.next() === (1, 'a')
      sut.hasNext must beTrue
      sut.next() === (1, 'b')
      sut.hasNext must beFalse
      ok
    }
    "Cantor iterator should behave - 3" >> {
      val sut = cantorIterator(1::2::Nil, 'a'::'b'::Nil)
      sut.hasNext must beTrue
      sut.next() === (1, 'a')
      sut.hasNext must beTrue
      sut.next() === (1, 'b')
      sut.hasNext must beTrue
      sut.next() === (2, 'a')
      sut.hasNext must beTrue
      sut.next() === (2, 'b')
      sut.hasNext must beFalse
      ok
    }
  }
