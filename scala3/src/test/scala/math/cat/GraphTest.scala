package math.cat

import math.Test
import math.sets.{PoSet, Sets}
import scalakittens.Good

import scala.language.postfixOps
import math.Base.*
import org.specs2.execute.Result as MatchResult

class GraphTest extends Test:
  import Graph._
  
  type SUT = Graph
  
  "Graph" should :

    "checks its arrows" in :
      expect(sut =>
        sut.asArrow(111) must be_==(111)
        sut.asArrow(112) should throwA[IllegalArgumentException]
      )(
        Graph.build("sut1", Set(1, 2, 3), Set(11, 111, 21, 32, 13), (x: Int) => x / 10 % 10, (x: Int) => x % 10))

    "are parallel" in :
      expect(sut => {
        import sut._
        
        sut.areParallel(13, 113) must beTrue
        sut.areParallel(21, 32) must beFalse
      })(
        Graph.build(
          "g",
          Set(1, 2, 3),
          Set(11, 111, 21, 32, 13, 113), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      )

    "same domain" in :
      expect(sut =>
        import sut._
        sut.sameDomain(11, 113) must beTrue
        sut.sameDomain(13, 113) must beTrue
        sut.sameDomain(21, 32) must beFalse
      )(Graph.build(
        "g",
        Set(1, 2, 3),
        Set(11, 111, 21, 32, 13, 113), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      )

    "same codomain" in :
      expect(sut =>
        import sut._
        sut.sameCodomain(13, 113) must beTrue
        sut.sameCodomain(21, 111) must beTrue
        sut.sameCodomain(21, 32) must beFalse
      )(
        Graph.build(
          "g",
          Set(1, 2, 3),
          Set(11, 111, 21, 32, 13, 113), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      )

    "contains" in :
      expect(sut => {
      (sut contains 2) must beTrue
      (sut contains 7) must beFalse
      })(
        Graph.build(
          "g",
          Set(1, 2, 3),
          Set(11, 111, 21, 32, 13, 113), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      )

    "equal" in :
      val sut1 = Graph.build(
        "sut1",
        Set(1, 2, 3),
        Set(11, 111, 21, 32, 13, 113), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      val sut2 = Graph.build(
        "sut2",
        Set(1, 2, 3),
        Set(11, 111, 21, 32, 13, 113), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      val sut3 = Graph.build(
        "sut3",
        Set(1, 2, 3),
        Set(11, 111, 21, 32, 13), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      val sut4 = Graph.build(
        "sut4",
        Set(1, 2, 3, 4),
        Set(11, 111, 21, 32, 13), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      (sut1 == sut1) must beTrue
      (sut1 == sut2) must beTrue
      (sut2 == sut1) must beTrue
      (sut2 == sut3) must beFalse
      (sut3 == sut4) must beFalse

    "follows" in :
      expect(sut => {
        import sut._
        sut.follows(113, 111) must beTrue
        sut.follows(111, 113) must beFalse
      })(
        Graph.build(
          "sut",
          Set(1, 2, 3),
          Set(11, 111, 21, 32, 13, 113), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      )

    "parse" in :
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
      val testGraph = Graph.fromArrowMap("sut", nodes, arrows)
      testGraph must be_==(testGraph.flatMap(g => Graph.read(g.toString)))

    "Singleton" in :
      val singleton = graph"({.}, {})"
      singleton.nodes must be_==(Set("."))
      singleton.arrows.isEmpty must beTrue

    "Constructor_plain_withmap" in :
      val objects = Set(1, 2, 3)
      val map = Map("1a" -> (1, 1), "1b" -> (1, 1), "2to1" -> (2, 1), "3to2" -> (3, 2), "1to3" -> (1, 3))
      val sutOpt = Graph.fromArrowMap("sut", objects, map)

      checkOption(sutOpt, (sut: Graph) =>
        import sut._
        sut.nodes must be_==(Set(3, 1, 2))
        sut.d0("2to1") must be_==(2)
        sut.d1("2to1") must be_==(1)
        sut.d0("1to3") must be_==(1)
        sut.d1("1to3") must be_==(3)
        sut.d0("3to2") must be_==(3)
        sut.d1("3to2") must be_==(2)
      ); ok


    "Constructor_plain_withFunctions" in :
      expect(sut =>
        import sut._
        sut.d0(111) must be_==(1)
        sut.d0(13) must be_==(1)
        sut.d1(13) must be_==(3)
        sut.d1(32) must be_==(2)
      )(
        Graph.build(
          "sut",
          Set(1, 2, 3),
          Set(11, 111, 21, 32, 13), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      )

    "Constructor_negativeBadD0" in :
      val sutOpt = Graph.build(
        "sut",
        Set(1, 3),
        Set(11, 111, 21, 32, 13), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      sutOpt.isBad must beTrue

    "Constructor_negativeBadD1" in :
      val sutOpt = Graph.build(
        "sut",
        Set(1, 2),
        Set(11, 111, 21, 13), (x: Int) => x / 10 % 10, (x: Int) => x % 10)

      sutOpt.isBad must beTrue

    "Equals_positive" in :
      val map = Map(11 -> (1, 1), 111 -> (1, 1), 21 -> (2, 1), 32 -> (3, 2), 13 -> (1, 3))
      val sut1 = Graph.fromArrowMap("sut", Set(1, 2, 3), map)
      val sut2 = Graph.build(
        "sut",
        Set(1, 2, 3),
        Set(11, 111, 21, 32, 13), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      sut1 must be_==(sut2)

    "Equals_negative" in :
      val sut1 = Graph.build(
        "sut1",
        Set(1, 2, 3),
        Set(11, 21, 32, 13), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      val sut2 = Graph.build(
        "sut2",
        Set(1, 2, 3),
        Set(11, 111, 21, 32, 13), (x: Int) => x / 10 % 10, (x: Int) => x % 10)
      (sut1 == sut2) must beFalse


    "UnaryOp" in :
      val sutOpt = Graph.build(
        "sut",
        Set(1, 2, 3),
        Set(11, 21, 32, 13),
        (x: Int) => x / 10 % 10,
        (x: Int) => x % 10)
      
      expect(sut => {
        import sut._
        sut.d0(32) must be_==(3)
        val opsut = ~sut
        val expected = Graph.build(
          "sut",
          Set(1, 2, 3),
          Set(11, 21, 32, 13),
          (x: Int) => x % 10,
          (x: Int) => x / 10 % 10) iHope
        
        opsut must be_==(expected)
        sut === ~opsut
      })(
        Graph.build(
          "sut",
          Set(1, 2, 3),
          Set(11, 21, 32, 13),
          (x: Int) => x / 10 % 10,
          (x: Int) => x % 10)
      )

    "subgraph" in :
      expect(sut =>
        import sut._
        val sub1 = sut.subgraph("0", Set.empty) iHope
        
        sub1.nodes.isEmpty must beTrue
        sub1.arrows.isEmpty must beTrue
        
        val sub2 = sut.subgraph("self", sut.nodes)
        sub2 must be_==(Good(sut))
        val sub3 = sut.subgraph("1,3", Set(sut.asNode(1), sut.asNode(3)))
        val expected = Graph.build(
          "sut",
          Set(1, 3),
          Set(11, 13),
          (x: Int) => x / 10 % 10,
          (x: Int) => x % 10)
        
        sub3 must be_==(expected)
      )(
        Graph.build(
          "sut",
          Set(1, 2, 3),
          Set(11, 21, 32, 13),
          (x: Int) => x / 10 % 10,
          (x: Int) => x % 10))

    "Discrete" in :
      val sut = Graph.discrete(Set(1, 2, 3))
      val expected = Graph.build("sut", Set(1, 2, 3), Set[Int](), (x: Int) => x, (x: Int) => x)
      Good(sut) must be_==(expected)
      sut must be_==(~sut)

    "FromPoset" in :
      val nodes = Set("a", "b", "c")
      val sut = Graph.ofPoset("sut", PoSet(nodes, (a: String, b: String) => a <= b))
      val arrows = idMap(Set(("a", "a"), ("a", "b"), ("a", "c"), ("b", "b"), ("b", "c"), ("c", "c")))
      val expected = Graph.fromArrowMap("sut", nodes, arrows) iHope
      
      sut.nodes must be_==(expected.nodes)
      sut.arrows must be_==(expected.arrows)
      sut must be_==(expected)

    "Parser_empty1" in :
      val sut = graph"({}, {})"
      sut must be_==(Graph.discrete(Set[String]()))

    "Parser_1" in :
      var sut = graph"({0}, {})"
      sut must be_==(Graph.discrete(Set("0")))

    "Parser_discrete1" in :
      val sut = graph"({1, 2, 3}, {})"
      sut must be_==(Graph.discrete(Set("1", "2", "3")))

    "Parser" in :
      val objects = Set("1", "2", "3")
      val map = Map("1a" -> ("1", "1"), "1b" -> ("1", "1"), "2to1" -> ("2", "1"), "3to2" -> ("3", "2"), "1to3" ->
        ("1", "3"))
      val expected = Graph.fromArrowMap("sut", objects, map).iHope
      val sut = graph"({1, 2, 3}, {1a: 1 -> 1, 1b: 1 -> 1, 2to1: 2 -> 1, 3to2: 3 -> 2, 1to3: 1 -> 3})"

      sut.nodes must be_==(expected.nodes)
      sut.arrows must be_==(expected.arrows)
      sut must be_==(expected)

    "hom" in :
      val sut = graph"({1, 2, 3}, {1a: 1 -> 1, 1b: 1 -> 1, 2to1: 2 -> 1, 3to2: 3 -> 2, 1to3: 1 -> 3})"
      import sut._
      val hom = sut.arrowsBetween("1", "1")
      Sets.parse("{1a, 1b}") must be_==(Good(hom))
      Sets.parse("{3to2}") must be_==(Good(sut.arrowsBetween("3", "2")))

    "~" in :
      val sut = graph"({1, 2, 3}, {1a: 1 -> 1, 1b: 1 -> 1, 2to1: 2 -> 1, 3to2: 3 -> 2, 1to3: 1 -> 3})"
      ~sut must be_== (graph"({1, 2, 3}, {1a: 1 -> 1, 1b: 1 -> 1, 2to1: 1 -> 2, 3to2: 2 -> 3, 1to3: 3 -> 1})")

    "be finite" in :

      val g3 = Graph.build(
        "g3",
        Set(1,2,3),
        Set(1,2,3),
        identity[Int],
        (i:Int) => i%3+1
      ).iHope
      
      g3.isFinite must beTrue
