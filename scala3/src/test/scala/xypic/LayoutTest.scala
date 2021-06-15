package xypic

import math.cat.Categories._
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification
import scalakittens._
import math.cat.Category
import scala.language.postfixOps

/**
  * Tests for Layout class
  */
class LayoutTest extends Specification {
  "Layout" >> {

    "graded" >> {
      val expectedLayersOfClusters = Map(
        "_1_"->List(List(Set("0"))),
        "Discrete_2.1"->List(List(Set("a"))),
        "Discrete_2.2"->List(List(Set("b"))),
        "Z2"->List(List(Set("1"))),
        "_2_"->List(List(Set("0")), List(Set("1"))),
        "Z3"->List(List(Set("0"))),
        "Z4"->List(List(Set("0"))),
        "ParallelPair"->List(List(Set("0")), List(Set("1"))),
        "Pullback"->List(List(Set("a"),Set("b")), List(Set("c"))),
        "Pushout"->List(List(Set("a")), List(Set("b"),Set("c"))),
        "Pushout4"->List(List(Set("a")), List(Set("b"),Set("c"),Set("d"),Set("e"))),
        "SplitMono"->List(List(Set("a")), List(Set("b"))),
        "_3_"->List(List(Set("0")), List(Set("1")), List(Set("2"))),
        "Square"->List(List(Set("a")), List(Set("b"),Set("c")), List(Set("d"))),
        "M"->List(List(Set("b"),Set("d")), List(Set("a"),Set("c"),Set("e"))),
        "W"->List(List(Set("a"),Set("c"),Set("e")), List(Set("b"),Set("d"))),
        "_4_"->List(List(Set("0")), List(Set("1")), List(Set("2")), List(Set("3"))),
        "Simplicial3"->List(List(Set("0")), List(Set("1")), List(Set("2"))),
        "_5_"->List(List(Set("0")), List(Set("1")), List(Set("2")), List(Set("3")), List(Set("4"))),
        "AAAAAA" -> List(List(Set("1", "2", "3", "4", "5", "6")))
      )
      
      def U[T](ss: Set[Set[T]]): Set[T] = ss.foldLeft(Set.empty[T])(_ union _)
      
      val expectedLayers = expectedLayersOfClusters.view.mapValues  (_.map(ls => U(ls.toSet))).toMap

      def gradedObjectsOf(c: Category): Set[GradedObjects] =
        Layout(c, 300, 300).gradedObjects
      
      val actualLayersOfClusters: Map[String, List[List[Set[String]]]] = {
        val lol: List[(String, List[List[Set[String]]])] = (for {
          c: Category <- KnownFiniteCategories
          ls = gradedObjectsOf(c)
          l: GradedObjects <- ls
          name: String = if (ls.size == 1) c.name else l.category.name
        } yield {
          name -> l.nameObjectsInLayers
        })
        lol.toMap
      }

      expectedLayersOfClusters.keySet === actualLayersOfClusters.keySet

      for {
        name <- actualLayersOfClusters.keySet
      } actualLayersOfClusters(name) === expectedLayersOfClusters(name)

      actualLayersOfClusters === expectedLayersOfClusters

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
