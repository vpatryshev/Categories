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
      val expectedLayersOfClusters = Map(
        "_1_"->List(Set(Set("0"))),
        "Discrete_2.1"->List(Set(Set("a"))),
        "Discrete_2.2"->List(Set(Set("b"))),
        "Z2"->List(Set(Set("1"))),
        "_2_"->List(Set(Set("0")), Set(Set("1"))),
        "Z3"->List(Set(Set("0"))),
        "ParallelPair"->List(Set(Set("0")), Set(Set("1"))),
        "Pullback"->List(Set(Set("a"),Set("b")), Set(Set("c"))),
        "Pushout"->List(Set(Set("a")), Set(Set("b"),Set("c"))),
        "Pushout4"->List(Set(Set("a")), Set(Set("b"),Set("c"),Set("d"),Set("e"))),
        "SplitMono"->List(Set(Set("a")), Set(Set("b"))),
        "_3_"->List(Set(Set("0")), Set(Set("1")), Set(Set("2"))),
        "Square"->List(Set(Set("a")), Set(Set("b"),Set("c")), Set(Set("d"))),
        "M"->List(Set(Set("b"),Set("d")), Set(Set("a"),Set("e"),Set("c"))),
        "W"->List(Set(Set("a"),Set("c"),Set("e")), Set(Set("b"),Set("d"))),
        "_4_"->List(Set(Set("0")), Set(Set("1")), Set(Set("2")), Set(Set("3"))),
        "HalfSimplicial"->List(Set(Set("0")), Set(Set("1")), Set(Set("2"))),
        "_5_"->List(Set(Set("0")), Set(Set("1")), Set(Set("2")), Set(Set("3")), Set(Set("4"))),
        "AAAAAA" -> List(Set(Set("1", "2", "3", "4", "5", "6")))
      )
      
      def U[T](ss: Set[Set[T]]): Set[T] = (Set.empty[T] /: ss)(_ union _)
      
      val expectedLayers = expectedLayersOfClusters mapValues (_ map U)

      val actualLayers: Map[String, List[Set[String]]] = (for {
        c <- KnownFiniteCategories
        ls = Layout(c, 300, 300).gradedObjects
        l <- ls
      } yield {
        val name = if (ls.size == 1) c.name else l.category.name
        name -> l.layers.map(_.map(_.toString))
      }) toMap

      for {
        name <- expectedLayers.keySet
      } actualLayers(name) === expectedLayers(name)
      
      actualLayers === expectedLayers
    }

  }

}
