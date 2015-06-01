package math.cat



import org.scalatest.junit.JUnit3Suite
import org.scalatest.prop.Checkers
import org.scalacheck.Prop.forAll
//
import runtime.RichInt

/**
 * Test suite for GraphMorphism class
 * @author vpatryshev
 */
class GraphMorphismSuite extends JUnit3Suite {

  def testConstructor {
    val objects = Set(1, 2, 3)
    val arrows1 = Set("1a", "1b", "2to1", "3to2", "1to3")
    val map = Map("1a" -> (1, 1), "1b" -> (1, 1), "2to1" -> (2, 1), "3to2" -> (3, 2), "1to3" -> (1, 3))
    val graph1 = Graph(objects, map)
    val arrows2 = Set(11, 111, 21, 32, 13)
    val graph2 = Graph(objects, arrows2, (x:Int) => x / 10 % 10, (x:Int) => x % 10)
    val nm = SetMorphism.unit(objects)
    val am = SetMorphism(arrows1, arrows2, Map("1a" -> 11, "1b" -> 111, "2to1" -> 21, "3to2" -> 32, "1to3" -> 13))
    val sut = new GraphMorphism("test", graph1, graph2, nm, am)
    assert(3 == sut.nodesMorphism(3))
    assert(111 == sut.arrowsMorphism("1b"))
    assert(graph1 == sut.d0)
    assert(graph2 == sut.d1)
  }

  def testUnit {
    val objects = Set(1, 2, 3)
    val arrows1 = Set("1a", "1b", "2to1", "3to2", "1to3")
    val map = Map("1a" -> (1, 1), "1b" -> (1, 1), "2to1" -> (2, 1), "3to2" -> (3, 2), "1to3" -> (1, 3))
    val graph = Graph(objects, map)
    type GIS = Graph[Int, String]
    val sut: GraphMorphism[Int, String, GIS, Int, String, GIS] = GraphMorphism.unit(graph)
    assert(objects == sut.d0)
    assert(objects == sut.d1)
    assert("1a" == sut.arrowsMorphism("1a"))
  }
}
