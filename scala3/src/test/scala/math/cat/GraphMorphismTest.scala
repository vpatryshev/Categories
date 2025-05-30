package math.cat

import math.Test
import math.sets.PoSet
import org.specs2.mutable._

import scala.language.postfixOps

/**
  * Tests for GraphMorphism class
  */
class GraphMorphismTest extends Test:
  "GraphMorphism" >> {

    "Constructor" >> {
      val objects = Set(1, 2, 3)
      val arrows1 = Set("1a", "1b", "2to1", "3to2", "1to3")
      val map = Map("1a" -> (1, 1), "1b" -> (1, 1), "2to1" -> (2, 1), "3to2" -> (3, 2), "1to3" -> (1, 3))
      val graph1 = Graph.fromArrowMap("g", objects, map).iHope
      val arrows2 = Set(11, 111, 21, 32, 13)
      val graph2 =
        Graph.build("g2", objects, arrows2,
          (x: Int) => x / 10 % 10, (x: Int) => x % 10).iHope
      val nm = SetMorphism.id(objects).asInstanceOf[graph1.Node => graph2.Node]
      val am =
        SetMorphism.build(arrows1, arrows2, Map("1a" -> 11, "1b" -> 111, "2to1" -> 21, "3to2" -> 32, "1to3" -> 13)).iHope.asInstanceOf[graph1.Arrow => graph2.Arrow]

      val sut = GraphMorphism("test", graph1, graph2)(nm, am)
      sut.nodesMapping(3) must be_==(3)
      sut.arrowsMapping("1b") must be_==(111)
      sut.d0 must be_==(graph1)
      sut.d1 must be_==(graph2)
    }

    "id" >> {
      val objects = Set(1, 2, 3)
      val arrows1 = Set("1a", "1b", "2to1", "3to2", "1to3")
      val map = Map("1a" -> (1, 1), "1b" -> (1, 1), "2to1" -> (2, 1), "3to2" -> (3, 2), "1to3" -> (1, 3))
      val graph = Graph.fromArrowMap("g", objects, map).iHope
      val sut: GraphMorphism = GraphMorphism.id(graph)
      sut.d0 must be_==(graph)
      sut.d1 must be_==(graph)
      sut.arrowsMapping("1a") === "1a"
      val sameThing = sut == GraphMorphism.id(graph)
      sameThing must beTrue
    }

    "compose" >> {
      def same[X, Y] = (i: X) => i.asInstanceOf[Y]
      val g1: Graph = Graph.ofPoset("sut", PoSet.range(0, 6, 1))
      
      val g6: Graph = Graph.build(
        "g",
        Set(1,2,3,4,5,6),
        Set(1,2,3,4,5,6),
        same,
        (i:Int) => i%6 + 1).iHope
      
      val g3 = Graph.build(
        "g3",
        Set(1,2,3),
        Set(1,2,3),
        same,
        (i:Int) => i%3+1
      ).iHope

      g3.isFinite must beTrue

      val add1 = (i: Int) => 1+i
      val mod3 = (i: Int) => 1+(i+2)%3
      val add1mod3 = (i: Int) => 1+i%3
      
      val sut1 = GraphMorphism("linear to loop", g1, g6)(
        add1.asInstanceOf[g1.Node => g6.Node],
        (p:g1.Arrow) => p.asInstanceOf[(Int, Int)]._1 + 1)
      
      val sut2: GraphMorphism = GraphMorphism("6 to 3", g6, g3)(
        mod3.asInstanceOf[g6.Node => g3.Node],
        mod3.asInstanceOf[g6.Arrow => g3.Arrow])
      
      val expected = GraphMorphism("linear to 3", g1, g3)(
        add1mod3.asInstanceOf[g1.Node => g3.Node],
        (p:g1.Arrow) => add1mod3(p.asInstanceOf[(Int, Int)]._1))

      val actual: GraphMorphism = sut1 andThen sut2 get
      
      actual.d0 must be_==(expected.d0)
      actual.d1 must be_==(expected.d1)
      val nodeDiff: List[expected.d0.Node] = expected.d0.nodes.filterNot(expected.sameNodesMapping(actual)).toList
      
      val nodeDiffVals = nodeDiff map
        ((x:expected.d0.Node) => (x, actual.nodesMapping(x), expected.nodesMapping(x)))

      nodeDiffVals must be_==(Nil)
      expected.sameNodes(actual) must beTrue

      val arrowDiff: List[expected.d0.Arrow] = expected.d0.arrows.filterNot(expected.sameArrowsMapping(actual)).toList

      val arrowDiffVals = arrowDiff map
        ((x:expected.d0.Arrow) => (x, actual.arrowsMapping(x), expected.arrowsMapping(x)))

      arrowDiffVals must be_==(Nil)

      expected.sameArrows(actual) must beTrue
      val sameThing = expected == actual
      sameThing must beTrue
    }

  }

