package math.cat

import math.Base.*
import math.cat.NaturalTransformation.printMapDifference
import math.sets.Sets.*
import scalakittens.{Cache, Result}
import scalakittens.Result.{Outcome, *}
import SetFunction.*
import scalakittens.Params.*

import java.rmi.server.LogStream.log
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
  val tag: Any,
  val d0: Functor,
  val d1: Functor) extends Morphism[Functor, Functor]:
  self =>
  lazy val domainCategory:   Category = d0.d0
  lazy val codomainCategory: Category = d1.d1

  def calculateMappingAt(x: d0.d0.Obj): d1.d1.Arrow

  lazy val mappingAt = Cache[d0.d0.Obj, d1.d1.Arrow](domainCategory.isFinite, calculateMappingAt)

  def ap0(x: Any): Any = {
    val y = mappingAt(x)
    y
  }

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
      def calculateMappingAt(x: d0.d0.Obj): d1.d1.Arrow = {
        val arrow = compositionAt(x)
        if (arrow.toString == "()")
          println("WTF, it's a bug!")
        arrow
      }
  
  end ∘

  infix def named(newTag: Any): NaturalTransformation =
    new NaturalTransformation(newTag, self.d0, self.d1):
      def calculateMappingAt(x: d0.d0.Obj): d1.d1.Arrow =
        self.calculateMappingAt(x)

  override def toString: String =
    val s = String.valueOf(tag)
    if s.isEmpty then details else s


  def asMap: Map[d0.d0.Obj, d1.d1.Arrow] =
    if d0.d0.isFinite then buildMap(d0.d0.objects, o => apply(o)) 
    else Map.empty

  override lazy val hashCode: Int = d0.hashCode | d1.hashCode * 17 | {
    val amhc = asMap.hashCode
    amhc.hashCode * 127
  }

  def details = s"NT($tag)(${
    if domainCategory.isFinite then
      domainCategory.listOfObjects.map(o => s"$o->(${apply(o)})").mkString(", ")
    else s"${d0.tag}->${d1.tag}"
  })"
  
  override def equals(x: Any): Boolean = x match
    case nt: NaturalTransformation => equalsWithDetails(nt)
    case otherwise => false

  private[cat] def equalsWithDetails(
    other: NaturalTransformation,
    printDetails: Boolean = false,
    context: String = "..."): Boolean =
    (this eq other) || {
      val hc = hashCode
      val otherHC = other.hashCode
      hc == otherHC &&
      d0 == other.d0 &&
      d1 == other.d1 && {
        val foundBad: Option[Any] = domainCategory.objects find (o =>
          val first: d1.d1.Arrow = this(o)
          val second: other.d1.d1.Arrow = other(o)
          val same = first == second
          if !same && printDetails then
            val first2 = this(o)
            val second2 = other(o)
            printMapDifference(asFunction(first), asFunction(second), context)
//            System.err.println(s"same?: $same")
          !same
        )

        foundBad.isEmpty
    }}

object NaturalTransformation:

  def printMapDifference(sf1: SetFunction, sf2: SetFunction, context: String): Unit =
    val diff = SetFunction.Diff(sf1, sf2)
    if diff.badKeys.nonEmpty then
      System.err.println("wow, bad keys $badkeys")

    if diff.distinctValuesAt.nonEmpty then
      System.err.println(s"$context: Different values at these keys: ${diff.distinctValuesAt}")


  def validate[
  X <: Category,
  Y <: Category
  ]( // transforming `f` to `g`
    f: Functor, g: Functor)(
    mappingAt: f.d0.Obj => f.d1.Arrow
  ): Outcome =
    Result.traverse {
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
        OKif(right_then_down == down_then_right, s"Nat'l transform law broken for $a")
    }
  
  /**
    * Builds a natural transformation
    *
    * @param f first functor
    * @param g   second functor
    * @param mappings a set morphism that for each domain object x returns f(x) -> g(x)
    */
  def build(tag: Any = "", f: Functor, g: Functor)
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
