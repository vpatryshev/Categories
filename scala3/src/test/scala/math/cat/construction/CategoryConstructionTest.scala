package math.cat.construction

import math.Base.*
import math.Test
import math.cat.Categories.*
import math.cat.{Categories, Category, Graph}
import math.cat.construction.*
import org.specs2.execute.Result as MatchResult
import scalakittens.Params.*
import scalakittens.{Good, Params, Result}

import scala.annotation.tailrec
import scala.language.{implicitConversions, postfixOps}

/**
  * Tests for Category class construction
  */
class CategoryConstructionTest extends Test with CategoryFactory:
  
  type SUT = Category
  
  val EmptyComposition: Map[(String, String), String] = Map()
  val EmptyMap: Map[String, String] = Map()

  private val defineComposition = Category.arrowBuilder
  
  "Category" should :

    "have segments" in :
      for i <- 0 until 10 do
        Category.fromSegment(i).arrows.size must be_==(i * (i + 1) / 2)

      ok

    "parsing example1" in :
      val d0d1 = Map(
        "0.1" -> ("0", "1"),
        "0.2" -> ("0", "2"),
        "a" -> ("1", "2"),
        "b" -> ("1", "2"),
        "2.1" -> ("2", "1"),
        "2.a" -> ("2", "2"),
        "2.b" -> ("2", "2"),
        "2.swap" -> ("2", "2")
      )

      lazy val sutOpt = Category("example1",
        Set("0", "1", "2"), // objects
        d0d1.view.mapValues(_._1).toMap, // d0
        d0d1.view.mapValues(_._2).toMap, // d1
        Map(
          ("0.1", "a") -> "0.2",
          ("0.1", "b") -> "0.2",
          ("2.1", "a") -> "2.a",
          ("2.1", "b") -> "2.b",
          ("a", "2.a") -> "a",
          ("a", "2.b") -> "b",
          ("a", "2.swap") -> "b",
          ("b", "2.a") -> "a",
          ("b", "2.b") -> "b",
          ("b", "2.swap") -> "a",
          ("2.a", "2.a") -> "2.a",
          ("2.a", "2.b") -> "2.b",
          ("2.b", "2.a") -> "2.a",
          ("2.b", "2.b") -> "2.b",
          ("2.swap", "2.a") -> "2.a",
          ("2.swap", "2.b") -> "2.b",
          ("2.swap", "2.swap") -> "2") // composition map
      )

      sutOpt match
        case Good(cat) =>
          val string = cat.toString
          Category.read(string) must be_==(sutOpt)
        case oops => failure(oops.toString)

      ok

    "regression from 6/9/15" in :
      val expected = Category("regression from 6/9/15",
        objects = Set("0", "1", "2"),
        domain = Map("0" -> "0", "1" -> "1", "2" -> "2", "a" -> "0", "b" -> "1"),
        codomain = Map("0" -> "0", "1" -> "1", "2" -> "2", "a" -> "2", "b" -> "2"),
        composition = EmptyComposition,
        defineComposition
      ).iHope

      val sample1 = category"sample1:({0,1,2}, {a: 0 -> 2, b: 1 -> 2}, {a ∘ 0 = a})"
      sample1 must be_==(expected)

      val sample2 = category"sample2:({0,1,2}, {a: 0 -> 2, b: 1 -> 2})"
      sample2 must be_==(expected)

    "constructor_Simplicial3" in :
      Simplicial3.objects must haveSize(3)

    "constructor 𝟙 bare" in :
      val sutOpt = Category(
        "constructor𝟙bare",
        objects = Set("1"),
        domain = EmptyMap,
        codomain = EmptyMap,
        composition = EmptyComposition,
        defineComposition
      )
      checkOption(sutOpt, _.arrows must haveSize(1))
      ok

    "constructor 𝟙 full" in :
      expect(_.arrows must haveSize(1))(
        Category("constructor 𝟙 full", Set("1"),
          Map("1" -> "1"), // d0
          Map("1" -> "1"), // d1
          Map(("1", "1") -> "1"),
          defineComposition
        )
      )

    "parse_1" in :
      val sut = category"({0}, {}, {})"
      sut.objects must be_==(Set("0"))

    "parse 𝟙 1" in :
      val sut = category"({1, 0}, {}, {})"
      sut.objects must be_==(Set("0", "1"))

    "parse_2" in :
      val sut = category"({1, 0}, {a: 0 -> 1}, {})"
      sut.objects must be_==(Set("0", "1"))

    "parse_Z3" in :
      Z3.arrows must be_==(Set("0", "1", "2"))

    "parse_nonsense" in :
      try
        category"(bs)"
        failure("should not have worked")
      catch
        case x: Exception => ok

      ok

    "parse_negative" in :
      val actual = Category("Bad Example", Set("0", "1", "2"),
        Map(
          "0_1" -> "0", "0_2" -> "0", "a" -> "1",
          "b" -> "1", "2_1" -> "2", "2_a" -> "2",
          "2_b" -> "2", "swap" -> "2"), // d0
        Map("0_1" -> "1", "0_2" -> "2", "a" -> "1", "b" -> "1",
          "2_1" -> "1", "2_a" -> "2", "2_b" -> "2", "swap" -> "2"), // d1
        // breaking laws
        Map(
          ("0_1", "a") -> "0_2", ("0_1", "b") -> "0_2",
          ("2_1", "a") -> "2_a", ("2_1", "b") -> "2_b",
          ("a", "swap") -> "b", ("b", "swap") -> "a",
          ("swap", "swap") -> "2"),
        defineComposition
      )
      val expectedErrors =
//        "composition must be defined for swap and 2_b in Bad Example"::
//        "composition must be defined for b and a in Bad Example"::
//        "composition must be defined for a and a in Bad Example"::
//        "composition must be defined for 2_a and swap in Bad Example"::
//        "Wrongly defined composition of 0_1 and swap in Bad Example"::
//        "Wrongly defined composition of b and 2 in Bad Example"::
//        "composition must be defined for b and b in Bad Example"::
//        "composition must be defined for 2_b and swap in Bad Example"::
//        "composition must be defined for swap and 2_a in Bad Example"::
//        "composition must be defined for a and b in Bad Example"::
//        "composition must be defined for 2_a and 2_a in Bad Example"::
//        "composition must be defined for 2_a and 2_b in Bad Example"::
//        "composition must be defined for 2_b and 2_b in Bad Example"::
//        "Wrongly defined composition of b and swap in Bad Example"::
//        "Wrongly defined composition of 2_1 and swap in Bad Example"::
//        "composition must be defined for 2_b and 2_a in Bad Example"::
//        "Wrongly defined composition of a and 2 in Bad Example"::
        "Wrongly defined composition of a and swap in Bad Example"::Nil
// TODO: fix this test case

      expectError(expectedErrors.mkString("; "), actual)
      actual.isBad

    def checkParsing(catOpt: Result[Category]): MatchResult =
      expect(sut =>
        val string = sut.toString
        val parsed = Category.read(string)
        parsed must be_==(catOpt)
      )(catOpt)

    "parse_positive_0" in :
      val sutOpt = Category("sample0", Set("1"),
        EmptyMap, // d0
        EmptyMap, // d1
        EmptyComposition,
        defineComposition
      )
      checkParsing(sutOpt)

    "parse_positive_3" in :
      val parsed = category"({1, 2}, {1: 1->1, 2: 2->2, 2_1: 2->1}, {2_1 ∘ 2 = 2_1})"
      parsed.objects.size must be_==(2)

    "parse_positive_4" in :
      val parsed = category"""(
        {1, 2},
        {1: 1->1, 2: 2->2, 2_1: 2->1, 2_a: 2->2}, 
        {2_1 ∘ 2 = 2_1, 2_a ∘ 2_a = 2_a, 2 ∘ 2_a = 2_a, 2_1 ∘ 2_a = 2_1,
         2_a ∘ 2 = 2_a, 2 ∘ 2 = 2, 1 ∘ 1 = 1, 1 ∘ 2_1 = 2_1}
      )"""
      parsed.objects.size must be_==(2)

    "parse_positive_5" in :
      val sutOpt = Category("sample5", Set("1", "2"),
        Map("2_1" -> "2"), // d0
        Map("2_1" -> "1"), // d1
        EmptyComposition,
        defineComposition
      )

      checkParsing(sutOpt)

    "parse_positive_6" in :
      checkParsing(Category("sample6", Set("1", "2"),
        Map("2_1" -> "2", "2_a" -> "2"), // d0
        Map("2_1" -> "1", "2_a" -> "2"), // d1
        Map(("2_a", "2_a") -> "2_a"),
        defineComposition
      ))

    "parse_positive_7" in :
      val sutOpt = Category("sample7", Set("0", "1", "2"),
        Map("0_1" -> "0", "0_2" -> "0", "2_1" -> "2", "2_a" -> "2"), // d0
        Map("0_1" -> "1", "0_2" -> "2", "2_1" -> "1", "2_a" -> "2"), // d1
        Map(("0_1", "a") -> "0_2",
            ("2_1", "a") -> "2_a",
            ("2_a", "2_a") -> "2_a"
        ),
        defineComposition
      )
      checkParsing(sutOpt)

    "parse_positive_8" in :
      val sutOpt = Category("sample8", Set("0", "1", "2"),
        Map("0_1" -> "0", "0_2" -> "0", "2_1" -> "2", "2_a" -> "2"), // d0
        Map("0_1" -> "1", "0_2" -> "2", "2_1" -> "1", "2_a" -> "2"), // d1
        Map(("0_1", "a") -> "0_2",
            ("2_1", "a") -> "2_a",
            ("2_a", "2_a") -> "2_a"
        ),
        defineComposition
      )
      checkParsing(sutOpt)
      ok

    "parse_positive" in :
      checkParsing(Good(Simplicial3))
      ok

    // following are tests for accompanying object

    "0" in :
      val expected = "𝟘: ({}, {}, {})"
      val actual = `𝟘`.toString
      actual must be_==(expected)
      `𝟘`.objects.size must be_==(0)
      `𝟘`.arrows.size must be_==(0)

    "1" in :
     `𝟙`.objects must be_==(Set("0"))
     `𝟙`.arrows must be_==(Set("0.0"))
     `𝟙`.objects.size must be_==(1)
     `𝟙`.arrows.size must be_==(1)

    "2" in :
      val sut = `𝟚`
      sut.objects must be_==(Set("0", "1"))
      val expected = Set("0.0", "0.1", "1.1")
      val arrows = sut.arrows
      arrows must be_==(expected)
      sut.arrowsBetween("0", "1").size must be_==(1)

    "3" in :
     `𝟛`.objects must be_==(Set("0", "1", "2"))
      val expected = Set("0.0", "1.1", "2.2", "0.1", "0.2", "1.2")
      expected === `𝟛`.arrows

    "Z2" in :
      Z2.arrows must be_==(Set("1", "a"))
      Z2.m("a", "a") must beSome("1")

    "SplitMono" in :
      SplitMono.objects must be_==(Set("a", "b"))
      SplitMono.arrows must be_==(Set("a", "b", "ab", "ba", "bb"))

    "M" in :
      M.objects.size must be_==(5)

    "Segment" in :
      def sut: Cat = fromSegment(3)
      sut === `𝟛`

  "Square" should :
    "pass a regression test of 3/31/19" in :
      Square.d0("cd") must be_==("c")
      Square.d1("cd") must be_==("d")

  @tailrec private[cat] final def transitiveClosure(
    data: PartialData, previouslyMissing: Int = Int.MaxValue): PartialData =
    
    try
      val stringified = data.toString
    catch
      case iae: IllegalArgumentException =>
        throw new IllegalArgumentException("Very bad data", iae)

    val missing = try
      data.missingCompositions
    catch
      case x: Exception =>
        throw new IllegalArgumentException(s"Failed on $data", x)

    if missing.isEmpty then data else
      val newData: PartialData = appendArrows(data, missing)
      transitiveClosure(newData, missing.size)


  private def appendArrows(data: PartialData, missing: Iterable[(data.Arrow, data.Arrow)]) =
    val nonexistentCompositions: Set[(data.Arrow, data.Arrow)] = data.nonexistentCompositions.toSet
    val newArrows: Map[data.Arrow, (data.Obj, data.Obj)] = nonexistentCompositions.flatMap{
      case (f, g) =>
        data.newComposition(f, g).map { h => (h, (data.d0(f), data.d1(g))) }
    }.toMap

    require(!newArrows.isEmpty,
      s"${data.name}: ${missing.size} arrows still missing: $missing")

    val newGraph: Graph = data.addArrows(newArrows) iHope
    
    val newData = new PartialData(newGraph):
      override def newComposition(f: Arrow, g: Arrow): Option[Arrow] =
        data.newComposition(f, g).map(itsanArrow)

      override val compositionSource: CompositionTable = data.composition.asInstanceOf[CompositionTable]
    (newData.validateGraph returning newData) orCommentTheError s"Failed on $newData" iHope

  "Parser, regression test of 6/18/21" should :
    "Parse AAA" in :
      val source = "AAA: ({1,2,3}, {12: 1 -> 2, 23: 2 -> 3, 31: 3 -> 1})"
      val graph = Graph.read(source)
      graph.isGood must beTrue
      val parser = new CategoryParser

      val data1 = CategoryData.partial[String, String](graph.iHope)(Map.empty, arrowBuilder)
      val s1 = data1.toString
      val missing1 = data1.missingCompositions
      val data2: PartialData = appendArrows(data1, missing1)

      val nodeStrings = asString(data2.nodes)
      val arrowsSorted: Seq[data2.Arrow] = listSorted(data2.arrows)
      def stringify(a: data2.Arrow) = s"$a: ${data2.d0(a)}->${data2.d1(a)}"
      val arrowStrings =
        arrowsSorted map ((a: data2.Arrow) => stringify(a)) mkString ", "

      val closure = transitiveClosure(data1)

      val raw1 = closure.factory.map { validData => validData.newCategory }

      raw1.isGood must beTrue

      val raw2 = data2.build
      raw2.isGood must beTrue

      val category = parser.buildCategory(graph, Map.empty)
      category.isGood must beTrue

      val parsed = parser.parseAll(parser.category, source)
      parsed match
        case parser.Success(res, _) => if !res.errorDetails.isEmpty then
          val p = Categories.read(source).iHope
          res.errorDetails must beNone

        case e: parser.NoSuccess => failure(s"Failed to parse: $e")

      ok

    "Parse AAAAAA" in :
      val source =
        "AAAAAA: ({1,2,3,4,5,6}, {12: 1 -> 2, 23: 2 -> 3, 34: 3 -> 4, 45: 4 -> 5, 56: 5 -> 6, 61: 6 -> 1})"
      val graph = Graph.read(source)
      graph.isGood must beTrue
      val parser = new CategoryParser

      val data = CategoryData.partial[String, String](graph.iHope)(Map.empty, arrowBuilder)

      val missingCompositions: List[(Any, Any)] = data.missingCompositions.toList

      val missing = try
        data.missingCompositions
      catch case x: Exception =>
        throw new IllegalArgumentException(s"Failed on $data", x)

      val expected =
        """TC: ({1, 2, 3, 4, 5, 6}, {12: 1->2, 1: 1->1, 23: 2->3, 2: 2->2, 34: 3->4, 3: 3->3, 45: 4->5, 4: 4->4, 56: 5->6, 5: 5->5, 61: 6->1, 6: 6->6})
          |TC: missing: HashSet((56,61), (45,56), (34,45), (23,34), (61,12), (12,23))
          |TC: new ones: HashMap(23∘12 -> (1,3), 34∘23 -> (2,4), 45∘34 -> (3,5), 61∘56 -> (5,1), 56∘45 -> (4,6), 12∘61 -> (6,2))
          |TC: ({1, 2, 3, 4, 5, 6}, {12: 1->2, 12∘61: 6->2, 1: 1->1, 23: 2->3, 23∘12: 1->3, 2: 2->2, 34: 3->4, 34∘23: 2->4, 3: 3->3, 45: 4->5, 45∘34: 3->5, 4: 4->4, 56: 5->6, 56∘45: 4->6, 5: 5->5, 61: 6->1, 61∘56: 5->1, 6: 6->6})
          |TC: missing: HashSet((61∘56,12), (34,56∘45), (56∘45,12∘61), (61∘56,23∘12), (45∘34,56), (23,45∘34), (23∘12,45∘34), (12,34∘23), (34∘23,56∘45), (34∘23,45), (23∘12,34), (12∘61,23), (56∘45,61), (45∘34,61∘56), (45,61∘56), (12∘61,34∘23), (61,23∘12), (56,12∘61))
          |TC: new ones: HashMap(61∘56∘45∘34 -> (3,1), 12∘61∘56 -> (5,2), 61∘56∘45 -> (4,1), 12∘61∘56∘45 -> (4,2), 56∘45∘34 -> (3,6), 56∘45∘34∘23 -> (2,6), 34∘23∘12 -> (1,4), 45∘34∘23 -> (2,5), 23∘12∘61 -> (6,3), 45∘34∘23∘12 -> (1,5), 34∘23∘12∘61 -> (6,4), 23∘12∘61∘56 -> (5,3))
          |TC: ({1, 2, 3, 4, 5, 6}, {12: 1->2, 12∘61: 6->2, 12∘61∘56: 5->2, 12∘61∘56∘45: 4->2, 1: 1->1, 23: 2->3, 23∘12: 1->3, 23∘12∘61: 6->3, 23∘12∘61∘56: 5->3, 2: 2->2, 34: 3->4, 34∘23: 2->4, 34∘23∘12: 1->4, 34∘23∘12∘61: 6->4, 3: 3->3, 45: 4->5, 45∘34: 3->5, 45∘34∘23: 2->5, 45∘34∘23∘12: 1->5, 4: 4->4, 56: 5->6, 56∘45: 4->6, 56∘45∘34: 3->6, 56∘45∘34∘23: 2->6, 5: 5->5, 61: 6->1, 61∘56: 5->1, 61∘56∘45: 4->1, 61∘56∘45∘34: 3->1, 6: 6->6})
          |TC: missing: HashSet((61∘56∘45,23∘12), (45∘34∘23∘12,56), (23,61∘56∘45∘34), (56∘45∘34,12∘61), (12∘61∘56∘45,23), (34∘23,61∘56∘45), (56∘45,23∘12∘61), (23∘12∘61,45∘34), (34∘23∘12∘61,45), (34∘23∘12,56∘45), (12∘61∘56,34∘23), (56,34∘23∘12∘61), (61∘56∘45∘34,12), (23∘12,56∘45∘34), (61,45∘34∘23∘12), (45∘34∘23,61∘56), (56∘45∘34∘23,61), (61∘56,34∘23∘12), (23∘12∘61∘56,34), (45,23∘12∘61∘56), (34,12∘61∘56∘45), (12,56∘45∘34∘23), (12∘61,45∘34∘23), (45∘34,12∘61∘56))
          |TC: new ones: HashMap(23∘12∘61∘56∘45 -> (4,3), 12∘61∘56∘45∘34 -> (3,2), 61∘56∘45∘34∘23 -> (2,1), 56∘45∘34∘23∘12 -> (1,6), 34∘23∘12∘61∘56 -> (5,4), 45∘34∘23∘12∘61 -> (6,5))
          |TC: ({1, 2, 3, 4, 5, 6}, {12: 1->2, 12∘61: 6->2, 12∘61∘56: 5->2, 12∘61∘56∘45: 4->2, 12∘61∘56∘45∘34: 3->2, 1: 1->1, 23: 2->3, 23∘12: 1->3, 23∘12∘61: 6->3, 23∘12∘61∘56: 5->3, 23∘12∘61∘56∘45: 4->3, 2: 2->2, 34: 3->4, 34∘23: 2->4, 34∘23∘12: 1->4, 34∘23∘12∘61: 6->4, 34∘23∘12∘61∘56: 5->4, 3: 3->3, 45: 4->5, 45∘34: 3->5, 45∘34∘23: 2->5, 45∘34∘23∘12: 1->5, 45∘34∘23∘12∘61: 6->5, 4: 4->4, 56: 5->6, 56∘45: 4->6, 56∘45∘34: 3->6, 56∘45∘34∘23: 2->6, 56∘45∘34∘23∘12: 1->6, 5: 5->5, 61: 6->1, 61∘56: 5->1, 61∘56∘45: 4->1, 61∘56∘45∘34: 3->1, 61∘56∘45∘34∘23: 2->1, 6: 6->6})
          |TC: missing: HashSet()
          |""".stripMargin.split("\n")

      resetLog()
      val closure = Result.forValue(CategoryData.transitiveClosure(data))

      val actual = Params.getLog.split("\n").filterNot(_.isEmpty)
      actual.length === expected.size

      val badAt = (0 until actual.length.min(expected.size)) find (i => actual(i) != expected(i))
      badAt match {
        case Some(i) =>
          failure(s"$i:\n${actual(i)}\n${expected(i)}")
        case None => ok
      }
      actual must be_==(expected)

      val raw1 = closure.flatMap(_.factory).map { validData => validData.newCategory }

      raw1.isGood must beTrue

      val raw2 = Result.forValue {
        CategoryData.transitiveClosure(data).factory.map { validData => validData.newCategory }
      }.flatten
      
      raw2.isGood must beTrue
      
      val raw3 = data.build
      raw3.isGood must beTrue

      val category = parser.buildCategory(graph, Map.empty)
      category.isGood must beTrue

      val parsed = parser.parseAll(parser.category, source)
      parsed match
        case parser.Success(res, _) => if !res.errorDetails.isEmpty then
          val p = Categories.read(source).iHope
          res.errorDetails must beNone

        case e: parser.NoSuccess => failure(s"Failed to parse: $e")

      ok
