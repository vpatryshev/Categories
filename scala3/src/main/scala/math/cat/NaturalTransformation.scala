package math.cat

import math.Base._
import math.cat.NaturalTransformation.printMapDifference
import math.sets.Sets._
import scalakittens.Result
import scalakittens.Result.{Outcome, _}
import SetFunction._

import scala.language.{implicitConversions, postfixOps}

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
abstract class NaturalTransformation(val tag: Any) extends Morphism[Functor, Functor]:
  self =>
  lazy val domainCategory:   Category = d0.d0
  lazy val codomainCategory: Category = d1.d1

  def transformPerObject(x: d0.d0.Obj): d1.d1.Arrow
  def apply(x: Any): d1.d1.Arrow = transformPerObject(x)

  /**
    * Produces g∘f
    * @param g next
    * @return
    */
  infix def andThen(g: NaturalTransformation): NaturalTransformation =
    
    def comp(x: d0.d0.Obj): d1.d1.Arrow =
      val fHere:  d1.d1.Arrow = self(x)
      val gThere: d1.d1.Arrow = g(x)
      d1.d1.m(fHere, gThere) getOrElse
        cannotDo(s"Bad transformation for $x for $fHere and $gThere")

    def composed[T](x: T) = comp(x)

    new NaturalTransformation(s"${g.tag} ∘ ${self.tag}"):
      val d0: Functor = self.d0
      val d1: Functor = g.d1
      def transformPerObject(x: d0.d0.Obj): d1.d1.Arrow = composed(x)
  
  end andThen

  infix def named(newTag: Any): NaturalTransformation = new NaturalTransformation(newTag):
    val d0: Functor = self.d0
    val d1: Functor = self.d1
    def transformPerObject(x: d0.d0.Obj): d1.d1.Arrow =
      self.transformPerObject(x)

  private lazy val asMap: Map[d0.d0.Obj, d1.d1.Arrow] =
    if d0.d0.isFinite then buildMap(d0.d0.objects, o => transformPerObject(o)) else Map.empty
  
  override lazy val hashCode: Int = d0.hashCode | d1.hashCode*17 | asMap.hashCode*127
  
  override def toString: String =
    val s = String.valueOf(tag)
    if s.isEmpty then details else s
  
  def details = s"NT($tag)(${
    if domainCategory.isFinite then
      domainCategory.listOfObjects.map(o => s"$o->(${transformPerObject(o)})").mkString(", ")
    else s"${d0.tag}->${d1.tag}"
  })"
  
  override def equals(x: Any): Boolean = x match
    case nt: NaturalTransformation => equalsWithDetails(nt, printDetails = false)
    case otherwise => false

  private[cat] def equalsWithDetails(other: NaturalTransformation, printDetails: Boolean): Boolean =
    (this eq other) || (
      hashCode == other.hashCode &&
      d0 == other.d0 &&
      d1 == other.d1 && {
        val foundBad: Option[Any] = domainCategory.objects find (o =>
          val first: d1.d1.Arrow = transformPerObject(o)
          val second: other.d1.d1.Arrow = other.transformPerObject(o)
          val same = first == second
          if !same && printDetails then
            printMapDifference(asFunction(first), asFunction(second))
            println(s"same?: $same")
          !same
        )

        foundBad.isEmpty
    })

object NaturalTransformation:

  def printMapDifference(sf1: SetFunction, sf2: SetFunction): Unit =
    val diff = SetFunction.Diff(sf1, sf2)
    if diff.badKeys.nonEmpty then
      println("wow, bad keys $badkeys")

    if diff.distinctValuesAt.nonEmpty then
      println(s"Different values at these keys: ${diff.distinctValuesAt}")


  def validate[
  X <: Category,
  Y <: Category
  ]( // transforming `f` to `g`
    f: Functor, g: Functor)(
    transformPerObject: f.d0.Obj => f.d1.Arrow
  ): Outcome =
    Result.traverse {
      for
        a <- f.d0.arrows
      yield
        val x0: f.d0.Obj = f.d0.d0(a)
        val x1: f.d0.Obj = f.d0.d1(a)
        val fa = f.arrowsMapping(a)
        val ga = g.arrowsMapping(a)
        val tx0: f.d1.Arrow = transformPerObject(x0)
        val tx1: f.d1.Arrow = transformPerObject(x1)
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
    validate(f, g)(mappings) returning new NaturalTransformation(tag):
      val d0: Functor = f
      val d1: Functor = g
      override def transformPerObject(x: d0.d0.Obj): d1.d1.Arrow =
        mappings(x)

  /**
    * Builds an identity natural transformation id[f]: f -> f
    *
    * @param f the functor for which we are building the identity transformation
    * @return identity natural transformation for the functor
    */
  def id(f: Functor): NaturalTransformation =

    def `id of f(x)`(x: f.d0.Obj): f.d1.Arrow =
      val `f(x)`: f.d1.Obj = f.objectsMapping(x)
      f.d1.id(`f(x)`)

    new NaturalTransformation("Id"):
      val d0: Functor = f
      val d1: Functor = f

      override def transformPerObject(x: d0.d0.Obj): d1.d1.Arrow =
        `id of f(x)`(x)
