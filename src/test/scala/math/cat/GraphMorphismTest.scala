package math.cat

import math.Test
import math.sets.PoSet
import org.specs2.mutable._
import math.Base._

/**
  * Tests for GraphMorphism class
  */
class GraphMorphismTest extends Test {
  "GraphMorphism" >> {

    "Constructor" >> {
      val objects = Set(1, 2, 3)
      val arrows1 = Set("1a", "1b", "2to1", "3to2", "1to3")
      val map = Map("1a" -> (1, 1), "1b" -> (1, 1), "2to1" -> (2, 1), "3to2" -> (3, 2), "1to3" -> (1, 3))
      val graph1 = Graph.fromArrowMap(objects, map).iHope
      val arrows2 = Set(11, 111, 21, 32, 13)
      val graph2 =
        Graph.build(objects, arrows2,
          (x: Int) => x / 10 % 10, (x: Int) => x % 10).iHope
      val nm = SetMorphism.id(objects)
      val am =
        SetMorphism(arrows1, arrows2, Map("1a" -> 11, "1b" -> 111, "2to1" -> 21, "3to2" -> 32, "1to3" -> 13))
      val sut = GraphMorphism("test", graph1, graph2)(
        nm.asInstanceOf[graph1.Node => graph2.Node],
        am.asInstanceOf[graph1.Arrow => graph2.Arrow])
      sut.nodesMapping(sut.d0.node(3)) === 3
      sut.arrowsMapping(sut.d0.arrow("1b")) === 111
      sut.d0 === graph1
      sut.d1 === graph2
    }

    "id" >> {
      val objects = Set(1, 2, 3)
      val arrows1 = Set("1a", "1b", "2to1", "3to2", "1to3")
      val map = Map("1a" -> (1, 1), "1b" -> (1, 1), "2to1" -> (2, 1), "3to2" -> (3, 2), "1to3" -> (1, 3))
      val graph = Graph.fromArrowMap(objects, map).iHope
      val sut: GraphMorphism = GraphMorphism.id(graph)
      sut.d0 === graph
      sut.d1 === graph
      sut.arrowsMapping("1a".asInstanceOf[sut.d0.Arrow]) === "1a"
      val sameThing = sut == GraphMorphism.id(graph)
      sameThing === true
    }

    "compose" >> {
      def same[X, Y] = (i: X) => i.asInstanceOf[Y]
      val g1: Graph = Graph.ofPoset(PoSet.range(0, 6, 1))
      
      val g6: Graph = Graph.build(
        Set(1,2,3,4,5,6),
        Set(1,2,3,4,5,6),
        same,
        (i:Int) => i%6 + 1).iHope
      
      val g3 = Graph.build(
        Set(1,2,3),
        Set(1,2,3),
        same,
        (i:Int) => i%3+1
      ).iHope

      g3.isFinite === true

      val add1 = (i: Int) => 1+i
      val mod3 = (i: Int) => 1+(i+2)%3
      val add1mod3 = (i: Int) => 1+i%3
      
      val sut1 = GraphMorphism("linear to loop", g1, g6)(
        add1.asInstanceOf[g1.Node => g6.Node],
        (p:g1.Arrow) => g6.arrow(p.asInstanceOf[(Int, Int)]._1 + 1))
      
      val sut2: GraphMorphism = GraphMorphism("6 to 3", g6, g3)(
        mod3.asInstanceOf[g6.Node => g3.Node],
        mod3.asInstanceOf[g6.Arrow => g3.Arrow])
      
      val expected = GraphMorphism("linear to 3", g1, g3)(
        add1mod3.asInstanceOf[g1.Node => g3.Node],
        (p:g1.Arrow) => g3.arrow(add1mod3(p.asInstanceOf[(Int, Int)]._1)))

      val actual = sut1 compose sut2
      actual.d0 === expected.d0
      actual.d1 === expected.d1
      val nodeDiff: List[expected.d0.Node] = expected.d0.nodes.filterNot(expected.sameNodesMapping(actual)).toList
      
      val nodeDiffVals = nodeDiff map
        ((x:expected.d0.Node) => (x, actual.nodesMapping(actual.d0.node(x)), expected.nodesMapping(expected.d0.node(x))))

      nodeDiffVals === Nil
      expected.sameNodes(actual) === true

      val arrowDiff: List[expected.d0.Arrow] = expected.d0.arrows.filterNot(expected.sameArrowsMapping(actual)).toList

      val arrowDiffVals = arrowDiff map
        ((x:expected.d0.Arrow) => (x, actual.arrowsMapping(actual.d0.arrow(x)), expected.arrowsMapping(expected.d0.arrow(x))))

      arrowDiffVals === Nil

      expected.sameArrows(actual) === true
      val sameThing = expected == actual
      sameThing === true
    }

  }

}
