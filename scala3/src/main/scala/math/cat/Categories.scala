package math.cat

import math.Base._
import math.cat.Categories._
import math.cat.Graph._
import math.cat.construction.{CategoryData, CategoryFactory}
import math.sets.PoSet
import scalakittens.Result._
import scalakittens.{Bad, Good, Result}

import java.io.Reader
import scala.collection.{IterableOnce, mutable}
import scala.language.{implicitConversions, postfixOps}

/**
  * A bunch of specific categories in this object
  */
object Categories extends CategoryFactory:

  /**
    * Simplified version of case categories,
    * where nodes and arrows are string-typed.
    * TODO: start using opaque types.
    */
  type Cat = Category {
    type Node = String
    type Arrow = String
  }
  
  /**
    * Empty category
    */
  lazy val`𝟘` : Cat = fromSegment(0, "𝟘")

  /**
    * Singleton category
    */
  lazy val`𝟙` : Cat = fromSegment(1, "𝟙")

  /**
    * Discrete 2-object category
    */
  lazy val _1plus1_ : Cat = asCat(discrete(Set("a", "b")))

  /**
    * Category <b>2</b>: 2 objects linearly ordered
    */
  lazy val `𝟚` : Cat = fromSegment(2, "𝟚")
  /**
    * Category <b>3</b>: 3 objects linearly ordered
    */
  lazy val `𝟛` : Cat = fromSegment(3, "𝟛")

  /**
    * Category <b>4</b>: 4 objects linearly ordered
    */
  lazy val `𝟜` : Cat = fromSegment(4, "𝟜")

  /**
    * Category <b>5</b>: 5 objects linearly ordered
    */
  lazy val `𝟝` : Cat = fromSegment(5, "𝟝")

  /**
    * Category with 2 objects and 2 parallel arrows from one to another
    */
  lazy val ParallelPair = category"ParallelPair:({0, 1}, {a:0->1, b:0->1})"

  /**
    * Category <b>Z2</2> - a two-element monoid
    */
  lazy val Z2 = category"Z2: ({1}, {1: 1 -> 1, a: 1 -> 1}, {1 ∘ 1 = 1, 1 ∘ a = a, a ∘ 1 = a, a ∘ a = 1})"

  lazy val Z3 = category"Z3: ({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 ∘ 1 = 2, 1 ∘ 2 = 0, 2 ∘ 1 = 0, 2 ∘ 2 = 1})"

  lazy val Z4 = category"Z4: ({0}, {0: 0->0, 1: 0->0, 2: 0->0, 3:0->0}, {1 ∘ 1 = 2, 1 ∘ 2 = 3, 2 ∘ 1 = 3, 2 ∘ 2 = 0, 2 ∘ 3 = 1, 3 ∘ 2 = 1, 3 ∘ 3 = 2})"

  /**
    * "Split Monomorphism" category (see http://en.wikipedia.org/wiki/Morphism)
    * Two objects, and a split monomorphism from a to b
    */
  lazy val SplitMono =
    category"SplitMono: ({a,b}, {ab: a -> b, ba: b -> a, bb: b -> b}, {ab ∘ ba = bb, bb ∘ bb = bb})"

  /**
    * Commutative square category
    */
  lazy val Square = category"Square:({a,b,c,d}, {ab: a -> b, ac: a -> c, bd: b -> d, cd: c -> d, ad: a -> d})"

  /**
    * Pullback category: a → c ← b
    */
  lazy val Pullback = category"Pullback:({a,b,c}, {ac: a -> c, bc: b -> c})"

  /**
    * Pushout category: b ← a → c
    */
  lazy val Pushout = category"Pushout:({a,b,c}, {ab: a -> b, ac: a -> c})"

  /**
    * This is the hardest category for logic calculations 
    *                        c  
    *                        ↑
    * Pushout4 category: b ← a → d
    *                        ↓
    *                        e
    */
  lazy val Pushout4 = category"Pushout4:({a,b,c,d,e}, {ab: a -> b, ac: a -> c, ad: a -> d, ae: a -> e})"

  /**
    * Sample W-shaped category: a     c      e
    *                            ↘  ↙ ↘  ↙
    *                              b     d
    */
  lazy val W = category"W:({a,b,c,d,e}, {ab: a -> b, cb: c -> b, cd: c -> d, ed: e -> d})"

  /**
    * Sample M-shaped category:     b      d
    *                             ↙  ↘  ↙  ↘
    *                            a     c      e
    */
  lazy val M = category"M:({a,b,c,d,e}, {ba: b -> a, bc: b -> c, dc: d -> c, de: d -> e})"
  
  /**
    * A segment of simplicial category.
    * Represents three sets (empty, singleton and two-point) and
    * all their possible functions.
    */
  lazy val Simplicial3: Cat = apply("Simplicial3",
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
  ).map(asCat) orCommentTheError "Bad Simplicial3" iHope

  /**
    * Evacuation plan category. See https://twitter.com/aik099/status/702928717266489345
    */
  def AAAAAA =
    category"AAAAAA: ({1,2,3,4,5,6}, {12: 1 -> 2, 23: 2 -> 3, 34: 3 -> 4, 45: 4 -> 5, 56: 5 -> 6, 61: 6 -> 1})"

  /**
    * The partial order of natural numbers, as a category.
    */
  lazy val NaturalNumbers: Category = fromPoset[BigInt]("ℕ", PoSet.ofNaturalNumbers)

  lazy val SimpleCategories = List(`𝟘`,`𝟙`, `𝟚`, `𝟛`, `𝟜`,
    _1plus1_,
    Z2, Z3, Z4
  )

  lazy val LessSimpleCategories = List(
    W, // this one is the hardest for logic calculations
    `𝟝`,
    ParallelPair, Pullback, Pushout, /*Pushout4,*/ SplitMono, Square,
    AAAAAA,
    Simplicial3, M)

  lazy val SomeKnownCategories = SimpleCategories ++ LessSimpleCategories

  lazy val KnownCategories: List[Category] =
    (NaturalNumbers::Pushout4::SomeKnownCategories).sortBy(_.arrows.size)

  lazy val KnownFiniteCategories: List[Category] =
    KnownCategories filter (_.isFinite)

  extension(sc: StringContext)
    def category(args: Any*): Cat = read(bufferFromContext(sc, args*)) iHope
      
  /**
    * Creates an opposite category from this one.
    * That is, all arrows are inverted.
    *
    * @return this<sup>op</sup>
    */
  def op(c: Category): Category =
    val opgraph = ~c
    new Category(opgraph.name):
      override val graph: Graph = opgraph
      override def nodes = c.nodes.asInstanceOf[Nodes]
      override def id(o: Obj): Arrow = c.id(o)
      override def m(f: Arrow, g: Arrow): Option[Arrow] =
        c.m(g, f).map(asArrow)
        
      override lazy val op = c      
