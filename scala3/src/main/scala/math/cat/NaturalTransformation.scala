package math.cat

import math.Base.*
import math.cat.NaturalTransformation.printMapDifference
import math.sets.Sets.*
import scalakittens.{Cache, Result}
import scalakittens.Result.{Outcome, *}
import SetFunction.*
import scalakittens.Params.*

import scala.annotation.targetName
import scala.language.{implicitConversions, postfixOps}
import scala.annotation.targetName

/**
  * Natural transformation class: morphisms for functors.
  *
  * The following three requirements are checked:
  * f and g are from the same category
  * f and g are to the same category
  * the following squares are commutative:
  *    f[a]: f[x] ---> f[y]
  *            |         |
  *       t[x] |         | t[y]
  *            |         |
  *            V         V
  *    g[a]: g[x] ---> g[y]
  */
abstract class NaturalTransformation(
  val tag: String,
  val d0: Functor,
  val d1: Functor) extends Morphism[Functor, Functor]:
  self =>
  lazy val domainCategory:   Category = d0.d0
  lazy val codomainCategory: Category = d1.d1
  def calculateMappingAt(x: d0.d0.Obj): d1.d1.Arrow

  private lazy val mappingAt: d0.d0.Obj => d1.d1.Arrow =
    Cache[d0.d0.Obj, d1.d1.Arrow](tag, calculateMappingAt, domainCategory.isFinite)

  def apply(x: Any): d1.d1.Arrow = mappingAt(x)

  /**
    * Produces f∘g
    * @param g previous
    * @return
    */
  @targetName("compose")
  infix def ∘(g: NaturalTransformation): NaturalTransformation =
    
    def compositionAt(x: d0.d0.Obj): d1.d1.Arrow =
      val fHere:  d1.d1.Arrow = self(x)
      val gThere: d1.d1.Arrow = g(x)
      d1.d1.m(gThere, fHere) getOrElse
        cannotDo(s"Bad transformation for $x for $fHere and $gThere")

    new NaturalTransformation(s"${self.tag} ∘ ${g.tag}", g.d0, self.d1):
      def calculateMappingAt(x: d0.d0.Obj): d1.d1.Arrow =
        val arrow = compositionAt(x)
        require (arrow.toString != "()", s"WTF, it's a bug: bad arrow $arrow")
        arrow

  end ∘

  infix def named(newTag: String): NaturalTransformation =
    new NaturalTransformation(newTag, self.d0, self.d1):
      def calculateMappingAt(x: d0.d0.Obj): d1.d1.Arrow =
        self.calculateMappingAt(x)

  override lazy val toString: String =
    val s = String.valueOf(tag)
    if s.isEmpty then details else s

  private lazy val asMap: Map[d0.d0.Obj, d1.d1.Arrow] =
    if d0.d0.isFinite then
      debug(s"building map for $tag")
      buildMap(d0.d0.objects, o => apply(o)) 
    else Map.empty

  override lazy val hashCode: Int = d0.hashCode | d1.hashCode * 17 | asMap.hashCode * 127

  def details = s"NT($tag)(${
    if domainCategory.isFinite then
      asString(domainCategory.listOfObjects.map(o => s"$o->(${apply(o)})"))
    else s"${d0.tag}->${d1.tag}"
  })"
  
  override def equals(x: Any): Boolean = x match
    case nt: NaturalTransformation => equalsWithDetails(nt)
    case otherwise => false

  private[cat] def equalsWithDetails(
    other: NaturalTransformation,
    printDetails: Boolean = false,
    context: String = "..."): Boolean =
    (this eq other) || (
      (d0 == other.d0) &&
      (d1 == other.d1) &&
      (asMap == other.asMap)
    ) || {
        val foundBad: Option[Any] = domainCategory.objects find (o =>
          val first: d1.d1.Arrow = this(o)
          val second: other.d1.d1.Arrow = other(o)
          val same = first == second
          if !same && printDetails then
            val first2 = this(o)
            val second2 = other(o)
            printMapDifference(asFunction(first), asFunction(second), context)
          !same
        )

        foundBad.isEmpty
    }

object NaturalTransformation:

  def printMapDifference(sf1: SetFunction, sf2: SetFunction, context: String): Unit =
    val diff = SetFunction.Diff(sf1, sf2)
    if !diff.badKeys.isEmpty then
      System.err.println(s"wow, bad keys ${diff.badKeys}")

    if !diff.distinctValuesAt.isEmpty then
      System.err.println(s"$context: Different values at these keys: ${diff.distinctValuesAt}")


  def validate[
  X <: Category,
  Y <: Category
  ]( // transforming `f` to `g`
    f: Functor, g: Functor)(
    mappingAt: f.d0.Obj => f.d1.Arrow
  ): Outcome =
    Result.traverse :
      for
        a <- f.d0.arrows
      yield
        val x0: f.d0.Obj = f.d0.d0(a)
        val x1: f.d0.Obj = f.d0.d1(a)
        val fa = f.arrowsMapping(a)
        val ga = g.arrowsMapping(a)
        val tx0: f.d1.Arrow = mappingAt(x0)
        val tx1: f.d1.Arrow = mappingAt(x1)
        val right_then_down: Option[f.d1.Arrow] = f.d1.m(fa, tx1) // a: x0->x1, fa: F[x0]->F[x1]; tx1: F[x1]->G[x1]
        val down_then_right: Option[f.d1.Arrow] = f.d1.m(tx0, ga)
        OKif(right_then_down == down_then_right, s"Natural transformation law broken for $a")

  /**
    * Builds a natural transformation
    *
    * @param f first functor
    * @param g   second functor
    * @param mappings a set morphism that for each domain object x returns f(x) -> g(x)
    */
  def build(tag: String = "", f: Functor, g: Functor)
  (
    mappings: f.d0.Obj => f.d1.Arrow
  ): Result[NaturalTransformation] =
    validate(f, g)(mappings) returning new NaturalTransformation(tag, f, g):
      override def calculateMappingAt(x: d0.d0.Obj): d1.d1.Arrow =
        mappings(x)

  /**
    * Builds an identity natural transformation id[f]: f -> f
    *
    * @param f the functor for which we are building the identity transformation
    * @return identity natural transformation for the functor
    */
  def id(f: Functor): NaturalTransformation =

    def `id of f(x)`(x: f.d0.Obj): f.d1.Arrow =
      val `f(x)`: f.d1.Obj = f.objectMapping(x)
      f.d1.id(`f(x)`)

    new NaturalTransformation("Id", f, f):

      override def calculateMappingAt(x: d0.d0.Obj): d1.d1.Arrow =
        `id of f(x)`(x)
