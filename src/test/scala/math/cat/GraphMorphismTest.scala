package math.cat

import math.sets.PoSet
import org.specs2.mutable._

/**
  * Tests for GraphMorphism class
  */
class GraphMorphismTest extends Specification {
  "GraphMorphism" >> {

    "Constructor" >> {
      val objects = Set(1, 2, 3)
      val arrows1 = Set("1a", "1b", "2to1", "3to2", "1to3")
      val map = Map("1a" -> (1, 1), "1b" -> (1, 1), "2to1" -> (2, 1), "3to2" -> (3, 2), "1to3" -> (1, 3))
      val graph1 = Graph(objects, map)
      val arrows2 = Set(11, 111, 21, 32, 13)
      val graph2 = Graph(objects, arrows2, (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      val nm = SetMorphism.id(objects)
      val am = SetMorphism(arrows1, arrows2, Map("1a" -> 11, "1b" -> 111, "2to1" -> 21, "3to2" -> 32, "1to3" -> 13))
      val sut = new GraphMorphism("test", graph1, graph2, nm, am)
      sut.nodesMapping(3) === 3
      sut.arrowsMapping("1b") === 111
      sut.d0 === graph1
      sut.d1 === graph2
    }

    "id" >> {
      val objects = Set(1, 2, 3)
      val arrows1 = Set("1a", "1b", "2to1", "3to2", "1to3")
      val map = Map("1a" -> (1, 1), "1b" -> (1, 1), "2to1" -> (2, 1), "3to2" -> (3, 2), "1to3" -> (1, 3))
      val graph = Graph(objects, map)
      type GIS = Graph[Int, String]
      val sut: GraphMorphism[Int, String, Int, String] = GraphMorphism.id(graph)
      sut.d0 === graph
      sut.d1 === graph
      sut.arrowsMapping("1a") === "1a"
      val sameThing = sut == GraphMorphism.id(graph)
      sameThing === true
    }

    "compose" >> {
      val same = (i: Int) => i
      val g1: Graph[Int, (Int, Int)] = Graph(PoSet.range(0, 6, 1))
      val g6: Graph[Int, Int] = Graph[Int, Int](nodes = Set(1,2,3,4,5,6), arrows = Set(1,2,3,4,5,6), same, (i:Int) => i%6 + 1)
      val g3 = Graph[Int, Int](nodes = Set(1,2,3), arrows = Set(1,2,3), same, (i:Int) => i%3+1)
      
      def mod3(i: Int) = 1+(i-1)%3
      
      val sut1 =
        new GraphMorphism[Int, (Int, Int), Int, Int]("linear to loop", g1, g6, same, (p:(Int, Int)) => p._1)
      
      val sut2: GraphMorphism[Int, Int, Int, Int] = new GraphMorphism("6 to 3", g6, g3, mod3, mod3)
      
      val expected = new GraphMorphism("linear to 3", g1, g3, mod3, (p:(Int, Int)) => mod3(p._1))

      val sameThing = (sut1 compose sut2) == expected
      sameThing === true
    }

  }

}
