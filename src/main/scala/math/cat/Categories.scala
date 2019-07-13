package math.cat

import java.io.Reader
import java.util.Objects

import math.cat.Categories._
import math.sets.PoSet
import math.sets.Sets._
import scalakittens.Result._
import scalakittens.{Good, Result}

import scala.collection.{GenTraversableOnce, TraversableOnce, mutable}

private[cat] trait CategoryFactory {
  /**
    * Builds a category out of a segment of integers between 0 and n (not included).
    *
    * @param n number of elements
    * @return a new category
    */
  def segment(n: Int): Cat = {
    val numbers = fromPoset(s"_${n}_", PoSet.range(0, n, 1))
    val maybeSegment = convert2Cat(numbers)(
      _.toString,
      { case (a, b) ⇒ s"$a.$b" })
    maybeSegment.fold(identity, err ⇒ throw new InstantiationException(err.toString))
  }

  private def convert2Cat[O, A](source: Category)(
    object2string: source.Obj ⇒ String = (_: source.Obj).toString,
    arrow2string: source.Arrow ⇒ String = (_: source.Arrow).toString): Result[Cat] = {
    val stringToObject = source.objects map (o ⇒ object2string(o) → o) toMap
    val string2Arrow = source.arrows map (a ⇒ arrow2string(a) → a) toMap
    val objects = stringToObject.keySet
    val arrows = string2Arrow.keySet
    val d0 = (f: String) ⇒ object2string(source.d0(string2Arrow(f)))
    val d1 = (f: String) ⇒ object2string(source.d1(string2Arrow(f)))
    val ids = (o: String) ⇒ arrow2string(source.id(stringToObject(o)))
    val composition = (f: String, g: String) ⇒ source.m(string2Arrow(f), string2Arrow(g)) map arrow2string

    for {
      _ ← OKif(source.isFinite, "Need a finite category")
      _ ← OKif(objects.size == source.objects.size, "some objects have the same string repr")
      _ ← OKif(arrows.size == source.arrows.size, "some arrows have the same string repr")
      g ← Graph.build(source.name, objects, arrows, d0, d1)
      data: CategoryData = CategoryData(g)(
        ids.asInstanceOf[g.Node ⇒ g.Arrow],  // TODO: find a way to avoid casting
        composition.asInstanceOf[(g.Arrow, g.Arrow) ⇒ Option[g.Arrow]])

        c ← data.build
    } yield c.asInstanceOf[Cat]
  }

  /**
    * Builds a category out of a poset. Arrows are pairs (x,y) where x <= y.
    *
    * @tparam T poset element type
    * @param theName the name we give to the category we build
    * @param poset original poset
    * @return category based on he poset
    */
  def fromPoset[T](theName: String = "", poset: PoSet[T]): Category = {
    new Category {
      val graph = Graph.ofPoset(theName, poset)
      type Node = T
      type Arrow = (T, T)

      override def id(o: Obj): Arrow = arrow((o, o))

      override def m(f: Arrow, g: Arrow): Option[Arrow] = (f, g) match {
        case (f: (T, T), g: (T, T)) ⇒
          Option(f._1, g._2).filter(_ ⇒ f._2 == g._1) map arrow
      }
    }
  }

  def asCat(source: Category): Cat = convert2Cat(source)(_.toString, _.toString).getOrElse(
    throw new InstantiationException("Failed to convert to Cat")
  )

  /**
    * Builds a category given a limited (but sufficient) amount of data.
    * Objects have the same name as their identity arrows.
    *
    * @tparam T object and arrow type
    * @param objects           set of objects (same as identity arrows)
    * @param domain            maps arrows to domains
    * @param codomain          maps arrows to codomain
    * @param compositionSource source table of arrows composition (may be incomplete)
    * @return a newly-built category
    */
  def apply[T](
    name: String,
    objects: Set[T],
    domain: Map[T, T],
    codomain: Map[T, T],
    comp: Map[(T, T), T]): Result[Category] = {
    for {
      g ← Graph.build(name, objects, domain.keySet, domain, codomain)
      val partial = CategoryData.partial(g)(comp.asInstanceOf[Map[(g.Arrow, g.Arrow), g.Arrow]]) // same type actually
      c ← partial.build
    } yield c
  }

  /**
    * Builds a discrete category on a given set of objects.
    *
    * @tparam T object type
    * @param objects set of this category's objects
    * @return the category
    */
  def discrete[T](objects: Set[T]): Category = {
    new PartialData(Graph.discrete[T](objects, s"Discrete_${objects.size}")).build iHope
  }

  /**
    * Builds a category given a limited (but sufficient) amount of data.
    * Objects have the same name as their identities.
    *
    * @tparam T arrow and node type
    * @param name              category name
    * @param graph             the underlying graph
    * @param compositionSource source table of arrows composition (may be incomplete)
    * @return a newly-built category
    */

  private[cat] def addUnitsToGraph(graph: Graph): Graph = {

    val nodesOpt: Option[Set[Any]] = if (graph.isFinite) Some(graph.nodes.toSet) else None

    def isIdentity(f: Any): Boolean = nodesOpt map (_ contains f) getOrElse (graph contains f)

    new Graph {
      override val name = graph.name
      
      def nodes: Nodes = graph.nodes.asInstanceOf[Nodes]

      lazy val arrows: Arrows = (graph.nodes ++ graph.arrows).asInstanceOf[Arrows]

      def d0(f: Arrow): Node =
        if (isIdentity(f)) node(f) else node(graph.d0(graph.arrow(f)))

      def d1(f: Arrow): Node =
        if (isIdentity(f)) node(f) else node(graph.d1(graph.arrow(f)))
    }
  }

  def composablePairs(graph: Graph): Iterable[(graph.Arrow, graph.Arrow)] = {
    for (f ← graph.arrows; g ← graph.arrows if graph.follows(g, f)) yield (f, g)
  }

  /**
    * Factory method. Parses a string and builds a category from it.
    *
    * @param input input to parse
    * @return the category
    */
  def read(input: Reader): Result[Cat] = (new Parser).readCategory(input)

  /**
    * Factory method. Parses a string and builds a category from it.
    *
    * @param input the string to parse
    * @return the category
    */
  def read(input: CharSequence): Result[Cat] = (new Parser).readCategory(input)

  class Parser extends Graph.Parser {

    def readCategory(input: CharSequence): Result[Cat] = {
      val parseResult = parseAll(category, input)
      explain(parseResult)
    }

    def category: Parser[Result[Cat]] =
      (name ?) ~ "(" ~ graphData ~ (("," ~ multTable) ?) ~ ")" ^^ {
        case nameOpt ~ "(" ~ gOpt ~ mOpt ~ ")" ⇒ {
          val name = nameOpt.getOrElse("c")
          val graphOpt = gOpt.map(_.build(name))

          mOpt match {
            case None ⇒
              buildCategory(graphOpt, Map.empty)
            case Some("," ~ m) ⇒
              buildCategory(graphOpt, m)
            case Some(garbage) ⇒ Result.error(s"bad data: $garbage")
          }
        }
      }

    private def buildCategory(
      gOpt: Result[Graph],
      multTable: Map[(String, String), String]): Result[Cat] = {
      for {
        g: Graph ← gOpt
        raw ← new PartialData(g) {
//          override val graph = g
          override val compositionSource = multTable.asInstanceOf[Map[(graph.Arrow, graph.Arrow), graph.Arrow]]
        }.build
        cat ← convert2Cat(raw)()
      } yield cat
    }

    def multTable: Parser[Map[(String, String), String]] = "{" ~ repsep(multiplication, ",") ~ "}" ^^ { case "{" ~ m
      ~ "}" ⇒ Map() ++ m
    }

    def multiplication: Parser[((String, String), String)] = {
      word ~ ("o"|"∘") ~ word ~ "=" ~ word ^^ { case g ~ o ~ f ~ "=" ~ h ⇒ ((f, g), h)
      }
    }

    def readCategory(input: Reader): Result[Cat] = {
      val parseResult = parseAll(category, input)
      explain(parseResult)
    }
  }

}


/**
  * A bunch of specific categories in this object
  */
object Categories extends CategoryFactory {

  type Cat = Category {
    type Node = String
    type Arrow = String
  }

  /**
    * Empty category
    */
  lazy val _0_ : Cat = segment(0)

  /**
    * Singleton category
    */
  lazy val _1_ : Cat = segment(1)

  /**
    * Discrete 2-object category
    */
  lazy val _1plus1_ : Cat = asCat(discrete(Set("a", "b")))

  /**
    * Category <b>2</b>: 2 objects linearly ordered
    */
  lazy val _2_ : Cat = segment(2)

  /**
    * Category <b>3</b>: 3 objects linearly ordered
    */
  lazy val _3_ : Cat = segment(3)

  /**
    * Category <b>4</b>: 4 objects linearly ordered
    */
  lazy val _4_ : Cat = segment(4)

  /**
    * Category <b>5</b>: 5 objects linearly ordered
    */
  lazy val _5_ : Cat = segment(5)

  /**
    * Category with 2 objects and 2 parallel arrows from one to another
    */
  lazy val ParallelPair = category"ParallelPair:({0, 1}, {a:0→1, b:0→1})"

  /**
    * Category <b>Z2</2> - a two-element monoid
    */
  lazy val Z2 = category"Z2: ({1}, {1: 1 → 1, a: 1 → 1}, {1 ∘ 1 = 1, 1 ∘ a = a, a ∘ 1 = a, a ∘ a = 1})"

  lazy val Z3 = category"Z3: ({0}, {0: 0 → 0, 1: 0 → 0, 2: 0 → 0}, {1 ∘ 1 = 2, 1 ∘ 2 = 0, 2 ∘ 1 = 0, 2 ∘ 2 = 1})"

  /**
    * "Split Monomorphism" category (see http://en.wikipedia.org/wiki/Morphism)
    * Two objects, and a split monomorphism from a to b
    */
  lazy val SplitMono =
    category"SplitMono: ({a,b}, {ab: a → b, ba: b → a, bb: b → b}, {ab ∘ ba = bb, bb ∘ bb = bb})"

  /**
    * Commutative square category
    */
  lazy val Square = category"Square:({a,b,c,d}, {ab: a → b, ac: a → c, bd: b → d, cd: c → d, ad: a → d})"

  /**
    * Pullback category: a → c ← b
    */
  lazy val Pullback = category"Pullback:({a,b,c}, {ac: a → c, bc: b → c})"

  /**
    * Pushout category: b ← a → c
    */
  lazy val Pushout = category"Pushout:({a,b,c}, {ab: a → b, ac: a → c})"

  /**
    * Pushout3 category: b ← a → c
    */
  lazy val Pushout4 = category"Pushout4:({a,b,c,d,e}, {ab: a → b, ac: a → c, ad: a → d, ae: a → e})"

  /**
    * Sample W-shaped category: a → b ← c → d ← e
    */
  lazy val W = category"W:({a,b,c,d,e}, {ab: a → b, cb: c → b, cd: c → d, ed: e → d})"

  /**
    * Sample M-shaped category: a ← b → c ← d → e
    */
  lazy val M = category"M:({a,b,c,d,e}, {ba: b → a, bc: b → c, dc: d → c, de: d → e})"


  /**
    * A segment of simplicial category.
    * Represents three sets (empty, singleton and two-point) and
    * all their possible functions.
    */
  lazy val HalfSimplicial: Cat = asCat(apply("HalfSimplicial",
    Set("0", "1", "2"),
    Map("0_1" → "0", "0_2" → "0", "2_1" → "2", "2_a" → "2", "2_b" → "2", "a" → "1", "b" → "1", "2_swap" →
      "2"), // d0
    Map("0_1" → "1", "0_2" → "2", "2_1" → "1", "2_a" → "2", "2_b" → "2", "a" → "2", "b" → "2", "2_swap" →
      "2"), // d1
    Map(("0_1", "a") → "0_2",
      ("0_1", "b") → "0_2",
      ("2_1", "a") → "2_a",
      ("2_1", "b") → "2_b",
      ("a", "2_swap") → "b",
      ("a", "2_a") → "a",
      ("b", "2_swap") → "a",
      ("b", "2_a") → "a",
      ("b", "2_b") → "b",
      ("2_swap", "2_swap") → "2",
      ("2_swap", "2_a") → "2_a",
      ("2_swap", "2_b") → "2_b",
      ("2_a", "2_a") → "2_a",
      ("2_b", "2_b") → "2_b",
      ("2_a", "2_swap") → "2_b",
      ("2_b", "2_swap") → "2_a"
    )
  ).
    getOrElse(throw new InstantiationException("Bad semisimplicial?")))
  
  lazy val AAAAAA = category"AAAAAA: ({1,2,3,4,5,6}, {12: 1 → 2, 23: 2 → 3, 34: 3 → 4, 45: 4 → 5, 56: 5 → 6, 61: 6 → 1})"

  lazy val NaturalNumbers: Category = fromPoset("ℕ", PoSet.ofNaturalNumbers)

  lazy val SomeKnownCategories = List(
    _0_, _1_, _3_, ParallelPair, SplitMono, W, M, Z3)

  lazy val KnownCategories: List[Category] = List(
    _0_, _1_, _2_, _3_, _4_, _5_, _1plus1_,
    ParallelPair, Pullback, Pushout, Pushout4, SplitMono, Square,
    M, W,
    Z2, Z3,
//    AAAAAA,
    HalfSimplicial, NaturalNumbers).sortBy(_.arrows.size)

  lazy val KnownFiniteCategories: List[Category] =
    KnownCategories filter (_.isFinite)

  implicit class CategoryString(val sc: StringContext) extends AnyVal {
    def category(args: Any*): Cat = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      var buf = new StringBuffer(strings.next)
      while (strings.hasNext) {
        buf append expressions.next
        buf append strings.next
      }
      read(buf) match {
        case Good(c) ⇒ c
        case bad ⇒ throw new InstantiationException(bad.errorDetails.mkString)
      }
    }
  }
}
