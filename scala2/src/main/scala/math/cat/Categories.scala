package math.cat

import scala.language.implicitConversions
import scala.language.postfixOps
import java.io.Reader

import math.Base._
import math.cat.Categories._
import math.sets.PoSet
import scalakittens.Result._
import scalakittens._

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
      { case (a, b) => s"$a.$b" })
    maybeSegment.fold(identity, err => throw new InstantiationException(err.toString))
  }

  private def convert2Cat[O, A](source: Category)(
    arrow2string: source.Arrow => String = _.toString): Result[Cat] = {
    val stringToObject: Map[String, source.Obj] = source.objects map (o => o.toString -> o) toMap
    val string2Arrow = source.arrows map (a => arrow2string(a) -> a) toMap
    val objects = stringToObject.keySet
    val arrows = string2Arrow.keySet
    val d0 = (f: String) => source.d0(string2Arrow(f)).toString
    val d1 = (f: String) => source.d1(string2Arrow(f)).toString
    val ids = (o: String) => arrow2string(source.id(stringToObject(o)))
    
    val composition = (f: String, g: String) => source.m(string2Arrow(f), string2Arrow(g)) map arrow2string
    
    for {
      _ <- OKif(source.isFinite, "Need a finite category")
      _ <- OKif(objects.size == source.objects.size, "some objects have the same string repr")
      _ <- OKif(arrows.size == source.arrows.size, "some arrows have the same string repr")
      g <- Graph.build(source.name, objects, arrows, d0, d1)
      typedIds <- ids.typed[g.Node => g.Arrow]
      typedComp <- composition.typed[(g.Arrow, g.Arrow) => Option[g.Arrow]]
      data = CategoryData(g)(typedIds, typedComp) // intellij shows a false negative
      category <- data.build
      c <- category.typed[Cat]
    } yield c
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
      override val graph: Graph = Graph.ofPoset(theName, poset)
      type Node = T
      type Arrow = (T, T)

      override def id(o: Obj): Arrow = arrow((o, o))

      override def m(f: Arrow, g: Arrow): Option[Arrow] = (f, g) match {
        case (f: (T, T), g: (T, T)) =>
          Option(f._1, g._2).filter(_ => f._2 == g._1) map arrow
      }
    }
  }

  def asCat(source: Category): Cat = convert2Cat(source)(_.toString).getOrElse(
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
    * @param composition source table of arrows composition (may be incomplete)
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
  ): Result[Category] = {
    for {
      g <- Graph.build(name, objects, domain.keySet, domain, codomain)
      c <- CategoryData.partial[T](g)(composition, compositionFactory).build
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
    CategoryData.partial[T](Graph.discrete[T](objects, s"Discrete_${objects.size}")
    )().build iHope
  }

  def composablePairs(graph: Graph): Iterable[(graph.Arrow, graph.Arrow)] = {
    for (f <- graph.arrows; g <- graph.arrows if graph.follows(g, f)) yield (f, g)
  }

  /**
    * Factory method. Parses a string and builds a category from it.
    *
    * @param input input to parse
    * @return the category
    */
  def read(input: Reader): Result[Cat] = {
    (new Parser).readCategory(input)
  }

  /**
    * Factory method. Parses a string and builds a category from it.
    *
    * @param input the string to parse
    * @return the category
    */
  def read(input: CharSequence): Result[Cat] = {
    val r = (new Parser).readCategory(input)
    val r1 = r.map { _.withSource(input.toString) }
    r1
  }

  class Parser extends Graph.Parser {

    def readCategory(input: CharSequence): Result[Cat] =
      explain(parseAll(category, input))

    def category: Parser[Result[Cat]] =
      (name ?) ~ "(" ~ graphData ~ (("," ~ multTable) ?) ~ ")" ^^ {
        case nameOpt ~ "(" ~ gOpt ~ mOpt ~ ")" =>
          val name = nameOpt getOrElse Good("c")
          val graphOpt = (gOpt andAlso name) map {
            case (g, n) => g build n
          }

          mOpt match {
            case None =>
              buildCategory(graphOpt, Map.empty)
            case Some("," ~ Good(m)) =>
              buildCategory(graphOpt, m)
            case Some("," ~ multTableErrors) =>
              val res = multTableErrors orCommentTheError "Failed to parse composition table" returning _0_
              res
            case Some(garbage) => Result.error(s"bad data: $garbage")
          }

        case nonsense => Result.error(s"malformed <<$nonsense>>")
      }

    private def buildCategory(
      gOpt: Result[Graph],
      multTable: Map[(String, String), String]): Result[Cat] = {
      for {
        g: Graph <- gOpt
        data = CategoryData.partial[String](g)(multTable, arrowBuilder)
        raw <- data.build
        cat <- convert2Cat(raw)()
      } yield cat
    }

    def multTable: Parser[Result[Map[(String, String), String]]] =
      "{" ~ repsep(multiplication, ",") ~ "}" ^^ {
        case "{" ~ m ~ "}" =>
          val r1 = Result.traverse(m).map(_.toMap)
          r1
        case nonsense => Result.error(s"malformed <<$nonsense>>")
      }

    def multiplication: Parser[Result[((String, String), String)]] = {
      word ~ ("o"|"∘") ~ word ~ "=" ~ word ^^ { 
        case g ~ "o" ~ f ~ "=" ~ h => Good(((f, g), h))
        case someShit =>
          Result.error(s"Failed to parse $someShit")
      }
    }

    def readCategory(input: Reader): Result[Cat] = {
      explain(parseAll(category, input))
    }
  }

  private[cat] val arrowBuilder = (p:(String, String)) => Option(s"${p._2}∘${p._1}")

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
  lazy val ParallelPair = category"ParallelPair:({0, 1}, {a:0->1, b:0->1})"

  /**
    * Category <b>Z2</2> - a two-element monoid
    */
  lazy val Z2 = category"Z2: ({1}, {1: 1->1, a: 1->1}, {1 ∘ 1 = 1, 1 ∘ a = a, a ∘ 1 = a, a ∘ a = 1})"

  lazy val Z3 = category"Z3: ({0}, {0: 0->0, 1: 0->0, 2: 0->0}, {1 ∘ 1 = 2, 1 ∘ 2 = 0, 2 ∘ 1 = 0, 2 ∘ 2 = 1})"

  lazy val Z4 = category"Z4: ({0}, {0: 0->0, 1: 0->0, 2: 0->0, 3:0->0}, {1 ∘ 1 = 2, 1 ∘ 2 = 3, 2 ∘ 1 = 3, 2 ∘ 2 = 0, 2 ∘ 3 = 1, 3 ∘ 2 = 1, 3 ∘ 3 = 2})"

  /**
    * "Split Monomorphism" category (see http://en.wikipedia.org/wiki/Morphism)
    * Two objects, and a split monomorphism from a to b
    */
  lazy val SplitMono =
    category"SplitMono: ({a,b}, {ab: a->b, ba: b->a, bb: b->b}, {ab ∘ ba = bb, bb ∘ bb = bb})"

  /**
    * Commutative square category
    */
  lazy val Square = category"Square:({a,b,c,d}, {ab: a->b, ac: a->c, bd: b->d, cd: c->d, ad: a->d})"

  /**
    * Pullback category: a → c ← b
    */
  lazy val Pullback = category"Pullback:({a,b,c}, {ac: a->c, bc: b->c})"

  /**
    * Pushout category: b ← a → c
    */
  lazy val Pushout = category"Pushout:({a,b,c}, {ab: a->b, ac: a->c})"

  /**
    * This is the hardest category for logic calculations 
    *                        c  
    *                        ↑
    * Pushout4 category: b ← a → d
    *                        ↓
    *                        e
    */
  lazy val Pushout4 = category"Pushout4:({a,b,c,d,e}, {ab: a->b, ac: a->c, ad: a->d, ae: a->e})"

  /**
    * Sample W-shaped category: a     c      e
    *                            ↘  ↙ ↘  ↙
    *                              b     d
    */
  lazy val W = category"W:({a,b,c,d,e}, {ab: a->b, cb: c->b, cd: c->d, ed: e->d})"

  /**
    * Sample M-shaped category:     b      d
    *                             ↙  ↘  ↙  ↘
    *                            a     c      e
    */
  lazy val M = category"M:({a,b,c,d,e}, {ba: b->a, bc: b->c, dc: d->c, de: d->e})"
  
  /**
    * A segment of simplicial category.
    * Represents three sets (empty, singleton and two-point) and
    * all their possible functions.
    */
  lazy val Simplicial3: Cat = asCat(apply("Simplicial3",
    Set("0", "1", "2"),
    Map("0_1" -> "0", "0_2" -> "0", "2_1" -> "2", "2_a" -> "2", "2_b" -> "2", "a" -> "1", "b" -> "1", "swap" ->
      "2"), // d0
    Map("0_1" -> "1", "0_2" -> "2", "2_1" -> "1", "2_a" -> "2", "2_b" -> "2", "a" -> "2", "b" -> "2", "swap" ->
      "2"), // d1
    Map(("0_1", "a") -> "0_2",
      ("0_1", "b") -> "0_2",
      ("2_1", "a") -> "2_a",
      ("2_1", "b") -> "2_b",
      ("a", "swap") -> "b",
      ("a", "2_a") -> "a",
      ("b", "swap") -> "a",
      ("b", "2_a") -> "a",
      ("b", "2_b") -> "b",
      ("swap", "swap") -> "2",
      ("swap", "2_a") -> "2_a",
      ("swap", "2_b") -> "2_b",
      ("2_a", "2_a") -> "2_a",
      ("2_b", "2_b") -> "2_b",
      ("2_a", "swap") -> "2_b",
      ("2_b", "swap") -> "2_a"
    ),
    arrowBuilder
  ).getOrElse(throw new InstantiationException("Bad Simplicial3")))
  
  lazy val AAAAAA = category"AAAAAA: ({1,2,3,4,5,6}, {12: 1->2, 23: 2->3, 34: 3->4, 45: 4->5, 56: 5->6, 61: 6->1})"

  lazy val NaturalNumbers: Category = fromPoset("ℕ", PoSet.ofNaturalNumbers)

  lazy val SimpleCategories = List(_0_, _1_, _2_, _3_, _4_, _5_, _1plus1_)
  
  lazy val LessSimpleCategories = List(
    W, // this one is the hardest for logic calculations
    ParallelPair, Pullback, Pushout, /*Pushout4,*/ SplitMono, Square,
    Z2, Z3, Z4,
    AAAAAA, Simplicial3, M)
  
  lazy val SomeKnownCategories: List[Cat] = SimpleCategories ++ LessSimpleCategories

  lazy val KnownCategories: List[Category] =
    NaturalNumbers::Pushout4::SomeKnownCategories.sortBy(_.arrows.size)

  lazy val KnownFiniteCategories: List[Category] =
    KnownCategories filter (_.isFinite)

  implicit class CategoryString(val sc: StringContext) extends AnyVal {
    def category(args: Any*): Cat = {
      val buffer = bufferFromContext(sc, args: _*)
      read(buffer) match {
        case Good(c) => c
        case bad =>
          val name = buffer.toString.split(":", 2).head
          val error: String =
            bad.listErrors.headOption.flatMap(ex => Option(ex.getMessage)).
              getOrElse(bad.errorDetails.mkString)
          throw new InstantiationException(s"$name: $error")
      }
    }
  }
}
