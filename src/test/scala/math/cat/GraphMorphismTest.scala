package math.cat

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
    val graph2 = Graph(objects, arrows2, (x:Int) => x / 10 % 10, (x:Int) => x % 10)
    val nm = SetMorphism.unit(objects)
    val am = SetMorphism(arrows1, arrows2, Map("1a" -> 11, "1b" -> 111, "2to1" -> 21, "3to2" -> 32, "1to3" -> 13))
    val sut = new GraphMorphism("test", graph1, graph2, nm, am)
    sut.nodesMorphism(3) === 3
    sut.arrowsMorphism("1b") === 111
    sut.d0 === graph1
    sut.d1 === graph2
  }

  "Unit" >> {
    val objects = Set(1, 2, 3)
    val arrows1 = Set("1a", "1b", "2to1", "3to2", "1to3")
    val map = Map("1a" -> (1, 1), "1b" -> (1, 1), "2to1" -> (2, 1), "3to2" -> (3, 2), "1to3" -> (1, 3))
    val graph = Graph(objects, map)
    type GIS = Graph[Int, String]
    val sut: GraphMorphism[Int, String, GIS, Int, String, GIS] = GraphMorphism.unit(graph)
    sut.d0 === graph
    sut.d1 === graph
    sut.arrowsMorphism("1a") == "1a"
  }

  }

}
