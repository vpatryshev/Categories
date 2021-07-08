package math.cat.construction

import math.Base._
import math.cat.Categories.Cat
import math.cat.Graph.GraphParser
import math.cat.{Category, Graph}
import math.sets.PoSet
import scalakittens.Result._
import scalakittens.{Good, Result}

import java.io.Reader
import scala.language.{implicitConversions, postfixOps}

/**
  * This trait contains a bunch of factory methods
  * for building all kinds of categories:
  * from strings, from partial orders, segments of numbers,
  * discrete categories. Used in `Categories` object,
  * which is a container for good examples of categories.
  * 
  */
private[cat] trait CategoryFactory:
  /**
    * Builds a category out of a segment of integers between 0 and n (not included).
    *
    * @param n number of elements
    * @return a new category
    */
  def segment(n: Int): Cat =
    val numbers = fromPoset(s"_${n}_", PoSet.range(0, n, 1))
    convert2Cat(numbers) { case (a, b) => s"$a.$b" } iHope

  private def convert2Cat[O, A](source: Category)(
    arrow2string: (source.Arrow => String) = (x: source.Arrow) => x.toString
  ): Result[Cat] =
    val stringToObject: Map[String, source.Obj] = source.objects map (o => o.toString -> o) toMap
    val string2Arrow = source.arrows map (a => arrow2string(a) -> a) toMap
    val objects = stringToObject.keySet
    val arrows = string2Arrow.keySet
    val d0 = (f: String) => source.d0(string2Arrow(f)).toString
    val d1 = (f: String) => source.d1(string2Arrow(f)).toString
    val ids = (o: String) => arrow2string(source.id(stringToObject(o)))

    val composition = (f: String, g: String) => source.m(string2Arrow(f), string2Arrow(g)) map arrow2string

    for
      _ <- OKif(source.isFinite, "Need a finite category")
      _ <- OKif(objects.size == source.objects.size, "some objects have the same string repr")
      _ <- OKif(arrows.size == source.arrows.size, "some arrows have the same string repr")
      g <- Graph.build(source.name, objects, arrows, d0, d1)
      typedIds <- ids.typed[g.Node => g.Arrow]
      typedComp <- composition.typed[(g.Arrow, g.Arrow) => Option[g.Arrow]]
      data = CategoryData(g)(typedIds, typedComp)
      category <- data.build
      c <- category.typed[Cat]
    yield c
  
  end convert2Cat

  /**
    * Builds a category out of a poset. Arrows are pairs (x,y) where x <= y.
    *
    * @tparam T poset element type
    * @param theName the name we give to the category we build
    * @param poset   original poset
    * @return category based on he poset
    */
  def fromPoset[T](theName: String = "", poset: PoSet[T]): Category =
    new Category(theName):
      override val graph: Graph = Graph.ofPoset(theName, poset)
      type Node = T
      type Arrow = (T, T)

      override def id(o: Obj): Arrow = arrow((o, o))

      override def m(f: Arrow, g: Arrow): Option[Arrow] = (f, g) match
        case (f: (T, T), g: (T, T)) =>
          Option(f._1, g._2).filter(_ => f._2 == g._1) map arrow
    
  end fromPoset

  def asCat(source: Category): Cat = convert2Cat(source)(_.toString) iHope

  /**
    * Builds a category given a limited (but sufficient) amount of data.
    * Objects have the same name as their identity arrows.
    *
    * @tparam T object and arrow type
    * @param objects            set of objects (same as identity arrows)
    * @param domain             maps arrows to domains
    * @param codomain           maps arrows to codomain
    * @param composition        source table of arrows composition (may be incomplete)
    * @param compositionFactory creates a new arrow for a composition of two arrows
    * @return a newly-built category
    */
  def apply[T](
    name: String,
    objects: Set[T],
    domain: Map[T, T],
    codomain: Map[T, T],
    composition: Map[(T, T), T],
    compositionFactory: ((T, T)) => Option[T] = CategoryData.nothing
  ): Result[Category] =
    for
      g <- Graph.build(name, objects, domain.keySet, domain, codomain)
      c <- CategoryData.partial[T](g)(composition, compositionFactory).build
    yield c

  /**
    * Builds a discrete category on a given set of objects.
    *
    * @tparam T object type
    * @param objects set of this category's objects
    * @return the category
    */
  def discrete[T](objects: Set[T]): Category =
    CategoryData.partial[T](Graph.discrete[T](objects, s"Discrete_${objects.size}")
    )().build iHope

  def composablePairs(graph: Graph): Iterable[(graph.Arrow, graph.Arrow)] =
    for (f <- graph.arrows; g <- graph.arrows if graph.follows(g, f)) yield (f, g)

  /**
    * Factory method. Parses a string and builds a category from it.
    *
    * @param input input to parse
    * @return the category
    */
  def read(input: Reader): Result[Cat] =
    (new CategoryParser).readCategory(input)

  /**
    * Factory method. Parses a string and builds a category from it.
    *
    * @param input the string to parse
    * @return the category
    */
  def read(input: CharSequence): Result[Cat] =
    val r = (new CategoryParser).readCategory(input)
    r.map { _.withSource(input.toString) }

  class CategoryParser extends GraphParser:

    def readCategory(input: CharSequence): Result[Cat] =
      explain(parseAll(category, input))

    def category: Parser[Result[Cat]] =
      (name ?) ~ "(" ~ graphData ~ (("," ~ multTable) ?) ~ ")" ^^ {
        case nameOpt ~ "(" ~ gOpt ~ mOpt ~ ")" =>
          val name = nameOpt getOrElse (Good("c"))
          val graphOpt = (gOpt andAlso name) map {
            case (g, n) => g build n
          }

          mOpt match
            case None =>
              buildCategory(graphOpt, Map.empty)
            case Some("," ~ Good(m)) =>
              buildCategory(graphOpt, m)
            case Some("," ~ multTableErrors) =>
              multTableErrors orCommentTheError "Failed to parse composition table" returning segment(0)
            case Some(garbage) => Result.error(s"bad data: $garbage")

        case nonsense => Result.error(s"malformed <<$nonsense>>")
      }

    private[cat] def buildCategory(
      gOpt: Result[Graph],
      multTable: Map[(String, String), String]): Result[Cat] =
      for
        g: Graph <- gOpt
        data = CategoryData.partial[String](g)(multTable, arrowBuilder)
        raw <- data.build
        cat <- convert2Cat(raw)()
      yield cat

    def multTable: Parser[Result[Map[(String, String), String]]] =
      "{" ~ repsep(multiplication, ",") ~ "}" ^^ {
        case "{" ~ m ~ "}" => Result.traverse(m).map(_.toMap)
        case nonsense => Result.error(s"malformed <<$nonsense>>")
      }

    def multiplication: Parser[Result[((String, String), String)]] =
      word ~ ("o" | "∘") ~ word ~ "=" ~ word ^^ {
        case g ~ o ~ f ~ "=" ~ h => Good(((f, g), h))
        case someShit => Result.error(s"Failed to parse $someShit")
      }

    def readCategory(input: Reader): Result[Cat] = explain(parseAll(category, input))
    
  end CategoryParser

  private[cat] val arrowBuilder = (p: (String, String)) => Option(s"${p._2}∘${p._1}")
