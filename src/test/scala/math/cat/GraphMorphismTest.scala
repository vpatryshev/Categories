package math.cat

import math.sets.PoSet
import org.specs2.mutable._
import scalaz.Alpha.X

/**
  * Tests for GraphMorphism class
  */
class GraphMorphismTest extends Specification {
  "GraphMorphism" >> {

    "Constructor" >> {
      val objects = Set(1, 2, 3)
      val arrows1 = Set("1a", "1b", "2to1", "3to2", "1to3")
      val map = Map("1a" -> (1, 1), "1b" -> (1, 1), "2to1" -> (2, 1), "3to2" -> (3, 2), "1to3" -> (1, 3))
      val graph1 = Graph.fromArrowMap(objects, map).getOrElse(throw new InstantiationException("oops"))
      val arrows2 = Set(11, 111, 21, 32, 13)
      val graph2 =
        Graph.build(objects, arrows2,
          (x: Int) => x / 10 % 10, (x: Int) => x % 10).getOrElse(throw new InstantiationException("oops"))
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
      val graph = Graph.fromArrowMap(objects, map).getOrElse(throw new InstantiationException("oops"))
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
        (i:Int) => i%6 + 1).getOrElse(throw new InstantiationException("oops"))
      
      val g3 = Graph.build(
        Set(1,2,3),
        Set(1,2,3),
        same,
        (i:Int) => i%3+1
      ).getOrElse(throw new InstantiationException("oops"))

      g3.isFinite === true
      
      val mod3 = (i: Int) => 1+(i-1)%3
      
      val sut1 = GraphMorphism("linear to loop", g1, g6)(
        same, (p:g1.Arrow) => g6.arrow(p.asInstanceOf[(Int, Int)]._1 + 1))
      
      val sut2: GraphMorphism = GraphMorphism("6 to 3", g6, g3)(
        mod3.asInstanceOf[g6.Node => g3.Node], mod3.asInstanceOf[g6.Arrow => g3.Arrow])
      
      val expected = GraphMorphism("linear to 3", g1, g3)(
        mod3.asInstanceOf[g1.Node => g3.Node], (p:g1.Arrow) => g3.arrow(mod3(p.asInstanceOf[(Int, Int)]._1 + 1)))

      val actual = sut1 compose sut2
      val sameThing = actual == expected
      sameThing === true
    }

  }

}
