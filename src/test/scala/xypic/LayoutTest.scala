package xypic

import math.cat.Categories.KnownFiniteCategories
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification
import scalakittens._

/**
  * Tests for Layout class
  */
class LayoutTest extends Specification {
  "Layout" >> {

    "graded" >> {
      val expected = Map(
        "_1_"->List(Set("0")),
        "Discrete_2.1"->List(Set("a")),
        "Discrete_2.2"->List(Set("b")),
        "Z2"->List(Set("1")),
        "_2_"->List(Set("0"), Set("1")),
        "Z3"->List(Set("0")),
        "ParallelPair"->List(Set("0"), Set("1")),
        "Pullback"->List(Set("a","b"), Set("c")),
        "Pushout"->List(Set("a"), Set("b","c")),
        "SplitMono"->List(Set("a"), Set("b")),
        "_3_"->List(Set("0"), Set("1"), Set("2")),
        "Square"->List(Set("a"), Set("b","c"), Set("d")),
        "M"->List(Set("b","d"), Set("a","e"), Set("c")),
        "W"->List(Set("a","c","e"), Set("b","d")),
        "_4_"->List(Set("0"), Set("1"), Set("2"), Set("3")),
        "HalfSimplicial"->List(Set("0"), Set("1"), Set("2")),
        "_5_"->List(Set("0"), Set("1"), Set("2"), Set("3"), Set("4"))
      )

      val actual: Map[String, List[Set[String]]] = (for {
        c <- KnownFiniteCategories
        ls = Layout(c).layouts
        l <- ls
      } yield {
        val name = if (ls.size == 1) c.name else l.cat.name
        name -> l.graded.map(_.map(_.toString))
      }) toMap
      
      actual === expected
      ok
    }

  }

}
