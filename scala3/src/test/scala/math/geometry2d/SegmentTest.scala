package math.geometry2d

import math.geometry2d.Segment._
import org.specs2.mutable._
import testing.TestBase

import scala.language.implicitConversions

/**
  * Prototype for all tests
  */
class SegmentTest extends TestBase:
  
  "intersection" should {
    "work" in {
      for
        x10 <- 0 to 10
        x11 <- 0 to 10
        x20 <- 0 to 10
        x21 <- 0 to 10
      do
        val i0 = Pt(x10, 0)
        val i1 = Pt(x11, 1)
        val seg1 = Segment(i0, i1)
        val j0 = Pt(x20, 0)
        val j1 = Pt(x21, 1)
        val seg2 = Segment(j0, j1)
        val expected = (x20 - x10) * (x21 - x11) <= 0
        val actual = seg1 interesectsWith seg2
        if (expected) {
          seg1.intersectsLine(seg2) === true
          seg2.intersectsLine(seg1) === true
        }
        (x10, x11, x20, x21, actual) === (x10, x11, x20, x21, expected)

      ok
    }
  }
