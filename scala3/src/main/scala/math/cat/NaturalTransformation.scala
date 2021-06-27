package math.cat

import scala.language.implicitConversions
import scala.language.postfixOps
import scalakittens.Result
import Result._
import scalakittens.Result.Outcome
import math.Base._
import math.cat.NaturalTransformation.printMapDifference

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
  def apply(x: Any): d1.d1.Arrow = transformPerObject(d0.d0.node(x))

  /**
    * Produces g∘f
    * @param g next
    * @return
    */
  def andThen(g: NaturalTransformation): NaturalTransformation =
    
    def comp(x: d0.d0.Obj): d1.d1.Arrow = {
      val fHere:  d1.d1.Arrow = d1.d1.arrow(self(x))
      val fThere: d1.d1.Arrow = d1.d1.arrow(g(x))
      val compOpt: Option[d1.d1.Arrow] = d1.d1.m(fHere, fThere)
      compOpt getOrElse(
        { // TODO: use Result.traverse
          throw new IllegalArgumentException(s"Bad transformation for $x for $fHere and $fThere")
        }
      )
    }

    def composed[T](x: T) = comp(d0.d0.node(x))

    new NaturalTransformation(s"${g.tag} ∘ ${self.tag}"):
      val d0: Functor = self.d0
      val d1: Functor = g.d1
      def transformPerObject(x: d0.d0.Obj): d1.d1.Arrow = d1.d1.arrow(composed(x))
  
  def named(newTag: Any): NaturalTransformation = new NaturalTransformation(newTag):
    val d0: Functor = self.d0
    val d1: Functor = self.d1
    def transformPerObject(x: d0.d0.Obj): d1.d1.Arrow =
      self.transformPerObject(x.asInstanceOf[self.d0.d0.Obj]).asInstanceOf[d1.d1.Arrow]

  private lazy val asMap: Map[d0.d0.Obj, d1.d1.Arrow] =
    if (d0.d0.isFinite) d0.d0.objects map (o => o -> transformPerObject(o)) toMap else Map.empty
  
  override lazy val hashCode: Int = d0.hashCode | d1.hashCode*17 | asMap.hashCode*31
  
  override def toString: String =
    val s = String valueOf tag
    if (s.isEmpty) details else s
  
  def details = s"NT($tag)(${
    if (domainCategory.isFinite) {
      domainCategory.listOfObjects.map(o => s"$o->(${transformPerObject(d0.d0.node(o))})").mkString(", ")
    } else s"${d0.tag}->${d1.tag}"
  })"
  
  override def equals(x: Any): Boolean = equalsWithDetails(x, printDetails = false)

  private[cat] def equalsWithDetails(x: Any, printDetails: Boolean): Boolean = x match
    case other: NaturalTransformation =>
      (this eq other) || (
        hashCode == other.hashCode &&
          d0 == other.d0 &&
          d1 == other.d1 && {
          val foundBad: Option[Any] = domainCategory.objects find (o =>
            val first: d1.d1.Arrow = transformPerObject(d0.d0.node(o))
            val second: other.d1.d1.Arrow = other.transformPerObject(o.asInstanceOf[other.d0.d0.Obj])
            val same = first == second
            if (!same && printDetails) then
              printMapDifference(first.asInstanceOf[SetFunction], second.asInstanceOf[SetFunction])

            !same
          ) // checking it every time takes time

          foundBad.isEmpty
        })
    case otherwise => false

object NaturalTransformation:

  def printMapDifference(sf1: SetFunction, sf2: SetFunction): Unit =
    val f1 = sf1.toSet.toMap
    val f2 = sf2.toSet.toMap
    if f1.keySet != f2.keySet then
      val badkeys = f1.keySet.diff(f2.keySet).union(f2.keySet.diff(f1.keySet))
      println("wow, bad keys $badkeys")
    else
      println(s"Different values at these keys: ${f1.keySet.filter(k => f1(k) != f2(k))}")
  
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
        val ga = g.arrowsMapping(g.d0.arrow(a))
        val tx0: f.d1.Arrow = transformPerObject(x0)
        val tx1: f.d1.Arrow = transformPerObject(x1)
        val rightdown: Option[f.d1.Arrow] = f.d1.m(fa, tx1) // a: x0->x1, fa: F[x0]->F[x1]; tx1: F[x1]->G[x1]
        val downright: Option[f.d1.Arrow] = f.d1.m(tx0, f.d1.arrow(ga))
        OKif(rightdown == downright, s"Nat'l transform law broken for $a")
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
        d1.d1.arrow(mappings(f.d0.obj(x)))

  /**
    * Builds an identity natural transformation id[f]: f -> f
    *
    * @param f the functor for which we are building the identity transformation
    * @return identity natural transformation for the functor
    */
  def id(f: Functor): NaturalTransformation =

    def `id of f(x)`(x: f.d0.Obj): f.d1.Arrow = {
      val `f(x)`: f.d1.Obj = f.objectsMapping(x)
      f.d1.id(`f(x)`)
    }

    new NaturalTransformation("Id"):
      val d0: Functor = f
      val d1: Functor = f

      override def transformPerObject(x: d0.d0.Obj): d1.d1.Arrow = {
        val actual_x = f.d0.obj(x)
        val `identity on f(x)` = d1.d1.arrow(`id of f(x)`(actual_x))
        `identity on f(x)`
      }
