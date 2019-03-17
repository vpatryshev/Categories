package math.cat

import math.Test
import math.sets.{PoSet, Sets}
import org.specs2.mutable._
import scalakittens.{Good, Result}

class GraphTest extends Test {
  import Graph._

  type SUT = Graph
  
  "Graph" >> {

    "checks its arrows" >> {
      expect(sut => {
        import sut._
        sut.arrow(111) === 111
        sut.arrow(112) should throwA[MatchError]
      })(
        Graph.build(Set(1, 2, 3), Set(11, 111, 21, 32, 13), (x: Int) => x / 10 % 10, (x: Int) => x % 10))
    }

    "are parallel" >> {
      expect(sut => {
        import sut._
        sut.areParallel(13, 113) === true
        sut.areParallel(21, 32) === false
      })(
        Graph.build(
          Set(1, 2, 3),
          Set(11, 111, 21, 32, 13, 113), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      )
    }

    "same domain" >> {
      expect(sut => {
        import sut._
        sut.sameDomain(11, 113) === true
        sut.sameDomain(13, 113) === true
        sut.sameDomain(21, 32) === false
      })(Graph.build(
        Set(1, 2, 3),
        Set(11, 111, 21, 32, 13, 113), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      )
    }

    "same codomain" >> {
      expect(sut => {
        import sut._
        sut.sameCodomain(13, 113) === true
        sut.sameCodomain(21, 111) === true
        sut.sameCodomain(21, 32) === false
      })(
        Graph.build(
          Set(1, 2, 3),
          Set(11, 111, 21, 32, 13, 113), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      )
    }

    "contains" >> {
      expect(sut => {
        import sut._
      (sut contains 2) === true
      (sut contains 7) === false
      })(
        Graph.build(
          Set(1, 2, 3),
          Set(11, 111, 21, 32, 13, 113), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      )
    }

    "equal" >> {
      val sut1 = Graph.build(Set(1, 2, 3), Set(11, 111, 21, 32, 13, 113), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      val sut2 = Graph.build(Set(1, 2, 3), Set(11, 111, 21, 32, 13, 113), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      val sut3 = Graph.build(Set(1, 2, 3), Set(11, 111, 21, 32, 13), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      val sut4 = Graph.build(Set(1, 2, 3, 4), Set(11, 111, 21, 32, 13), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      (sut1 equals sut1) === true
      (sut1 equals sut2) === true
      (sut2 equals sut1) === true
      (sut2 equals sut3) === false
      (sut3 equals sut4) === false
    }

    "follows" >> {
      expect(sut => {
        import sut._
      sut.follows(113, 111) === true
      sut.follows(111, 113) === false
      })(
        Graph.build(
          Set(1, 2, 3),
          Set(11, 111, 21, 32, 13, 113), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      )
    }

    "parse" >> {
      val nodes = Set("0", "1", "2")
      val arrows = Map(
        "0.id" -> ("0", "0"),
        "0.1" -> ("0", "1"),
        "0.2" -> ("0", "2"),
        "1.id" -> ("1", "1"),
        "a" -> ("1", "2"),
        "b" -> ("1", "2"),
        "2.1" -> ("2", "1"),
        "2.id" -> ("2", "2"),
        "2.a" -> ("2", "2"),
        "2.b" -> ("2", "2"),
        "2.swap" -> ("2", "2"))
      val testGraph = Graph.fromArrowMap(nodes, arrows)
      testGraph === testGraph.flatMap(g => Graph.read(g.toString))
    }

    "Singleton" >> {
      val singleton = graph"({.}, {})"
      singleton.nodes === Set(".")
      singleton.arrows.isEmpty must beTrue
    }

    "Constructor_plain_withmap" >> {
      val objects = Set(1, 2, 3)
      val map = Map("1a" -> (1, 1), "1b" -> (1, 1), "2to1" -> (2, 1), "3to2" -> (3, 2), "1to3" -> (1, 3))
      val sutOpt = Graph.fromArrowMap(objects, map)

      check(sutOpt, (sut: Graph) => {
        import sut._
        sut.nodes === Set(3, 1, 2)
        sut.d0("2to1") === 2
        sut.d1("2to1") === 1
        sut.d0("1to3") === 1
        sut.d1("1to3") === 3
        sut.d0("3to2") === 3
        sut.d1("3to2") === 2
      }); ok
    }

    "Constructor_plain_withFunctions" >> {
      expect(sut => {
        import sut._
      sut.d0(111) === 1
      sut.d0(13) === 1
      sut.d1(13) === 3
      sut.d1(32) === 2
      })(
        Graph.build(
          Set(1, 2, 3),
          Set(11, 111, 21, 32, 13), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      )
    }

    "Constructor_negativeBadD0" >> {
      val sutOpt = Graph.build(
        Set(1, 3),
        Set(11, 111, 21, 32, 13), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      sutOpt.isBad === true
    }

    "Constructor_negativeBadD1" >> {
        val sutOpt = Graph.build(
          Set(1, 2),
          Set(11, 111, 21, 13), (x: Int) => x / 10 % 10, (x: Int) => x % 10)

      sutOpt.isBad === true
    }

    "Equals_positive" >> {
      val map = Map(11 -> (1, 1), 111 -> (1, 1), 21 -> (2, 1), 32 -> (3, 2), 13 -> (1, 3))
      val sut1 = Graph.fromArrowMap(Set(1, 2, 3), map)
      val sut2 = Graph.build(Set(1, 2, 3), Set(11, 111, 21, 32, 13), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      sut1 === sut2
    }

    "Equals_negative" >> {
      val sut1 = Graph.build(Set(1, 2, 3), Set(11, 21, 32, 13), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      val sut2 = Graph.build(Set(1, 2, 3), Set(11, 111, 21, 32, 13), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      (sut1 == sut2) must beFalse
    }

    "UnaryOp" >> {
      val sutOpt = Graph.build(
        Set(1, 2, 3),
        Set(11, 21, 32, 13),
        (x: Int) => x / 10 % 10,
        (x: Int) => x % 10)
      
      expect(sut => {
        import sut._
        sut.d0(32) === 3
        val opsut = ~sut
        val expected = Graph.build(
          Set(1, 2, 3),
          Set(11, 21, 32, 13),
          (x: Int) => x % 10,
          (x: Int) => x / 10 % 10).getOrElse(throw new InstantiationException("oops"))
        opsut === expected
        sut === ~opsut
      })(
        Graph.build(
          Set(1, 2, 3),
          Set(11, 21, 32, 13),
          (x: Int) => x / 10 % 10,
          (x: Int) => x % 10)      )
    }

    "Discrete" >> {
      val sut = Graph.discrete(Set(1, 2, 3))
      val expected = Graph.build(Set(1, 2, 3), Set[Int](), (x: Int) => x, (x: Int) => x)
      Good(sut) === expected
      sut === ~sut
    }

    "FromPoset" >> {
      val nodes = Set("a", "b", "c")
      val sut = Graph.ofPoset(PoSet(nodes, (a: String, b: String) => a <= b))
      import sut._
      val arrows = Sets.idMap(Set(("a", "a"), ("a", "b"), ("a", "c"), ("b", "b"), ("b", "c"), ("c", "c")))
      val expected = Graph.fromArrowMap(nodes, arrows).getOrElse(throw new IllegalArgumentException)
      sut.nodes === expected.nodes
      sut.arrows === expected.arrows
      sut === expected
    }

    "Parser_empty1" >> {
      val sut = graph"({}, {})"
      sut === Graph.discrete(Set[String]())
    }

    "Parser_1" >> {
      var sut = graph"({0}, {})"
      sut === Graph.discrete(Set("0"))
    }

    "Parser_discrete1" >> {
      val sut = graph"({1, 2, 3}, {})"
      sut === Graph.discrete(Set("1", "2", "3"))
    }

    "Parser" >> {
      val objects = Set("1", "2", "3")
      val map = Map("1a" -> ("1", "1"), "1b" -> ("1", "1"), "2to1" -> ("2", "1"), "3to2" -> ("3", "2"), "1to3" ->
        ("1", "3"))
      val expected = Graph.fromArrowMap(objects, map).getOrElse(throw new IllegalArgumentException)
      val sut = graph"({1, 2, 3}, {1a: 1 -> 1, 1b: 1 -> 1, 2to1: 2 -> 1, 3to2: 3 -> 2, 1to3: 1 -> 3})"
      import sut._

      sut.nodes === expected.nodes
      sut.arrows === expected.arrows
      sut === expected
    }

    "hom" >> {
      val sut = graph"({1, 2, 3}, {1a: 1 -> 1, 1b: 1 -> 1, 2to1: 2 -> 1, 3to2: 3 -> 2, 1to3: 1 -> 3})"
      import sut._
      val hom = sut.arrowsBetween("1", "1")
      hom === Sets.parse("{1a, 1b}")
      sut.arrowsBetween("3", "2") === Sets.parse("{3to2}")
    }

    "~" >> {
      val sut = graph"({1, 2, 3}, {1a: 1 -> 1, 1b: 1 -> 1, 2to1: 2 -> 1, 3to2: 3 -> 2, 1to3: 1 -> 3})"
      ~sut === graph"({1, 2, 3}, {1a: 1 -> 1, 1b: 1 -> 1, 2to1: 1 -> 2, 3to2: 2 -> 3, 1to3: 3 -> 1})"
    }

  }
}
