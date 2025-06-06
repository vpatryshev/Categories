package xypic

import math.cat.Categories._
import math.cat.Category
import org.specs2.execute.Result as MatchResult
import org.specs2.mutable.Specification
import scalakittens._

import scala.language.postfixOps

/**
  * Tests for Layout class
  */
class LayoutTest extends Specification:
  "Layout" >> {

    "special cases of graded" >> {
      ok
    }

    "graded" >> {
      val expectedLayersOfClusters =
        Map(
          "Discrete_2.1"->List(List(Set("a"))),
          "Discrete_2.2"->List(List(Set("b")))) ++ 
        Map(
        "𝟙"->List(List(Set("0"))),
        "Z2"->List(List(Set("1"))),
        "𝟚"->List(List(Set("0")), List(Set("1"))),
        "𝟛"->List(List(Set("0")), List(Set("1")), List(Set("2"))),
        "𝟜"->List(List(Set("0")), List(Set("1")), List(Set("2")), List(Set("3"))),
        "𝟝"->List(List(Set("0")), List(Set("1")), List(Set("2")), List(Set("3")), List(Set("4"))),
        "Z3"->List(List(Set("0"))),
        "Z4"->List(List(Set("0"))),
        "ParallelPair"->List(List(Set("0")), List(Set("1"))),
        "Pullback"->List(List(Set("a"),Set("b")), List(Set("c"))),
        "Pushout"->List(List(Set("a")), List(Set("b"),Set("c"))),
        "Pushout4"->List(List(Set("a")), List(Set("b"),Set("c"),Set("d"),Set("e"))),
        "SplitMono"->List(List(Set("a")), List(Set("b"))),
        "Square"->List(List(Set("a")), List(Set("b"),Set("c")), List(Set("d"))),
        "M"->List(List(Set("b"),Set("d")), List(Set("a"),Set("c"),Set("e"))),
        "W"->List(List(Set("a"),Set("c"),Set("e")), List(Set("b"),Set("d"))),
        "Simplicial3"->List(List(Set("0")), List(Set("1")), List(Set("2"))),
        "AAAAAA" -> List(List(Set("1", "2", "3", "4", "5", "6")))
      ).view.filterKeys(KnownFiniteCategories.map(_.name).contains).toMap
      
      def U[T](ss: Set[Set[T]]): Set[T] = ss.foldLeft(Set.empty[T])(_ union _)
      def united[T](lss: List[List[Set[T]]]): List[Set[T]] = lss.map(ls => U(ls.toSet))

      val premap: Map[String, List[Set[String]]] =
        expectedLayersOfClusters.map {
          case (k, v) => k -> united(v)
        } toMap

      val expectedLayers = premap

      def gradedObjectsOf(c: Category): Set[GradedObjects] =
        Layout(c, 300, 300).gradedObjects
      
      val actualLayersOfClusters: Map[String, List[List[Set[String]]]] =
        val lol: List[(String, List[List[Set[String]]])] = (for
          c: Category <- KnownFiniteCategories
          ls = gradedObjectsOf(c)
          l: GradedObjects <- ls
          name: String = if ls.size == 1 then c.name else l.category.name
        yield
          name -> l.nameObjectsInLayers
        )
        lol.toMap

      val missing = (expectedLayersOfClusters.keySet diff actualLayersOfClusters.keySet).toList
      missing must be_==(Nil)

      val extra = (actualLayersOfClusters.keySet diff expectedLayersOfClusters.keySet).toList
      extra must be_==(Nil)

      actualLayersOfClusters.keySet must be_==(expectedLayersOfClusters.keySet)

      for
        name <- actualLayersOfClusters.keySet
      do 
        actualLayersOfClusters(name) must be_==(expectedLayersOfClusters(name))

      actualLayersOfClusters must be_==(expectedLayersOfClusters)

      val actualLayers: Map[String, List[Set[String]]] = (for
        c <- KnownFiniteCategories
        ls = Layout(c, 300, 300).gradedObjects
        l <- ls
      yield
        val name = if ls.size == 1 then c.name else l.category.name
        name -> l.layers.map(_.map(_.toString))
      ) toMap

      for
        name <- expectedLayers.keySet
      do actualLayers(name) must be_==(expectedLayers(name))
      
      actualLayers must be_==(expectedLayers)
    }

  }
