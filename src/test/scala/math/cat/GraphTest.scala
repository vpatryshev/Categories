package math.cat

import org.specs2.mutable._

class GraphTest extends Specification {

  "Graph" >> {
//    "is immutable" >> {
//      val sut = Graph(Set(1, 2, 3), Set(11, 111, 21, 32, 13), (x:Int) => x / 10 % 10, (x:Int) => x % 10)
//
//      sut-1 should throwA[UnsupportedOperationException]
//      sut+1 should throwA[UnsupportedOperationException]
//    }
     "parse" >> {
       val nodes = Set("0", "1", "2")
       val arrows = Map(
            "0.id"  -> ("0", "0"),
            "0.1"   -> ("0", "1"),
            "0.2"   -> ("0", "2"),
            "1.id"  -> ("1", "1"),
            "a"     -> ("1", "2"),
            "b"     -> ("1", "2"),     
            "2.1"   -> ("2", "1"),
            "2.id"  -> ("2", "2"),
            "2.a"   -> ("2", "2"),
            "2.b"   -> ("2", "2"),
            "2.swap"-> ("2", "2"))
       val testGraph = Graph(nodes, arrows)
      testGraph ===  Graph(testGraph.toString())
     }

    "Singleton" >> {
      val singleton = Graph("({.}, {})")
      singleton.nodes === Set(".")
      singleton.arrows.isEmpty must beTrue
    }

    "Constructor_plain_withmap" >> {
    val objects = Set(1, 2, 3)
    val map = Map("1a" -> (1, 1), "1b" -> (1, 1), "2to1" -> (2, 1), "3to2" -> (3, 2), "1to3" -> (1, 3))
    val sut = Graph(objects, map)

    sut.nodes === Set(3, 1, 2)
    sut.d0("2to1") === 2
    sut.d1("2to1") === 1
    sut.d0("1to3") === 1
    sut.d1("1to3") === 3
    sut.d0("3to2") === 3
    sut.d1("3to2") === 2
  }

   "Constructor_plain_withFunctions" >> {
     val sut = Graph(Set(1, 2, 3), Set(11, 111, 21, 32, 13), (x:Int) => x / 10 % 10, (x:Int) => x % 10)
     sut.d0(111) === 1
     sut.d0(13) === 1
     sut.d1(13) === 3
     sut.d1(32) === 2
  }

  "CopyConstructor" >> {
    val source = Graph(Set(1, 2, 3), Set(11, 111, 21, 32, 13), (x:Int) => x / 10 % 10, (x:Int) => x % 10)
    val sut = new Graph(source)
    source === sut
  }

  "Constructor_negativeBadD0" >> {
    try {
      val sut = Graph(Set(1, 3), Set(11, 111, 21, 32, 13), (x:Int) => x / 10 % 10, (x:Int) => x % 10)
      failure("Validator should have thrown an exception")
    } catch {
      case e: IllegalArgumentException => // as expected
    }
    true
  }

  "Constructor_negativeBadD1" >> {
    try {
      val sut = Graph[Int, Int](Set(1, 2), Set(11, 111, 21, 13), (x:Int) => x / 10 % 10, (x:Int) => x % 10)
      failure("Validator should have thrown an exception")
    } catch {
      case e: IllegalArgumentException => // as expected
    }
    true
  }

  "Equals_positive" >> {
    val map = Map(11 -> (1, 1), 111 -> (1, 1), 21 -> (2, 1), 32 -> (3, 2), 13 -> (1, 3))
    val sut1 = Graph(Set(1, 2, 3), map)
    val sut2 = Graph(Set(1, 2, 3), Set(11, 111, 21, 32, 13), (x:Int) => x / 10 % 10, (x:Int) => x % 10)
    sut1 === sut2
  }

  "Equals_negative" >> {
    val sut1 = Graph(Set(1, 2, 3), Set(11, 21, 32, 13), (x:Int) => x / 10 % 10, (x:Int) => x % 10)
    val sut2 = Graph(Set(1, 2, 3), Set(11, 111, 21, 32, 13), (x:Int) => x / 10 % 10, (x:Int) => x % 10)
    (sut1 == sut2) must beFalse
  }

   "UnaryOp" >> {
     val sut = Graph(Set(1, 2, 3), Set(11, 21, 32, 13), (x:Int) => x / 10 % 10, (x:Int) => x % 10)
     sut.d0(32) === 3
     val opsut = ~sut
     val expected = Graph(Set(1, 2, 3), Set(11, 21, 32, 13), (x:Int) => x % 10, (x:Int) => x / 10 % 10)

     opsut === expected
     sut === ~opsut
  }

  "Discrete" >> {
    val sut = Graph(Set(1, 2, 3))
    val expected = Graph(Set(1, 2, 3), Set[Int](), (x:Int) => x % 10, (x:Int) => x / 10 % 10)
    sut === expected
    sut === ~sut
  }

  "FromPoset" >> {
    val nodes = Set("a", "b", "c")
    val sut = Graph(PoSet(nodes, (a: String, b: String) => a <= b))
    val arrows = Sets.idMap(Set(("a", "a"), ("a", "b"), ("a", "c"), ("b", "b"), ("b", "c"), ("c", "c")))
    val expected = Graph(nodes, arrows)
    sut.nodes === expected.nodes
    sut.arrows ==== expected.arrows
    sut === expected
  }

  "Parser_empty1" >> {
    val sut = Graph("({}, {})")
    sut === Graph(Set[String]())
  }

  "Parser_1" >> {
    var sut = Graph("({0}, {})")
    sut === Graph(Set("0"))
  }

  "Parser_discrete1" >> {
    val sut = Graph("({1, 2, 3}, {})")
    sut === Graph(Set("1", "2", "3"))
  }

  "Parser" >> {
    val objects = Set("1", "2", "3")
    val map = Map("1a" -> ("1", "1"), "1b" -> ("1", "1"), "2to1" -> ("2", "1"), "3to2" -> ("3", "2"), "1to3" -> ("1", "3"))
    val expected = Graph(objects, map)
    val sut = Graph("({1, 2, 3}, {1a: 1 -> 1, 1b: 1 -> 1, 2to1: 2 -> 1, 3to2: 3 -> 2, 1to3: 1 -> 3})")

    sut.nodes ==== expected.nodes
    sut.arrows === expected.arrows
    sut === expected
  }

  "Hom" >> {
    val sut = Graph("({1, 2, 3}, {1a: 1 -> 1, 1b: 1 -> 1, 2to1: 2 -> 1, 3to2: 3 -> 2, 1to3: 1 -> 3})")
    val hom = sut.hom("1", "1")
    hom === Sets.parse("{1a, 1b}")
    sut.hom("3", "2") === Sets.parse("{3to2}")
  }

  }
}