package math.cat

import scala.language.implicitConversions
import scala.language.postfixOps
import scalakittens.Result
import Result._
import scalakittens.Result.Outcome
import math.Base._

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
abstract class NaturalTransformation extends Morphism[Functor, Functor] { self =>
  val tag: Any
  lazy val domainCategory:   Category = notNull(notNull(d0, "Missing d0").d0, s"Missing d0.d0 in $d0")
  lazy val codomainCategory: Category = notNull(notNull(d1, "Missing d1").d1, "Missing d1.d1")

  def transformPerObject(x: d0.d0.Obj): d1.d1.Arrow
  def apply(x: Any): d1.d1.Arrow = transformPerObject(d0.d0.node(x))

  /**
    * Produces g∘f
    * @param g next
    * @return
    */
  def andThen(g: NaturalTransformation): NaturalTransformation = {
    
    def comp(x: d0.d0.Obj): d1.d1.Arrow = {
      val fHere: d1.d1.Arrow =
        codomainCategory.arrow(self(x))
      val fThere: d1.d1.Arrow =
        codomainCategory.arrow(g(x))
      val compOpt: Option[d1.d1.Arrow] = d1.d1.m(fHere, fThere)
      compOpt getOrElse(
        {
          throw new IllegalArgumentException(s"Bad transformation for $x for $fHere and $fThere")
        }
      )
    }

    def composed[T](x: T) = {
      comp(d0.d0.node(x))
    }

    new NaturalTransformation {
      val tag = s"${g.tag} ∘ ${self.tag}"
      val d0: Functor = self.d0
      val d1: Functor = g.d1
      def transformPerObject(x: d0.d0.Obj): d1.d1.Arrow = d1.d1.arrow(composed(x))
    }
  }
  
  def named(newTag: Any): NaturalTransformation = new NaturalTransformation {
    val tag = newTag
    val d0: Functor = self.d0
    val d1: Functor = self.d1
    def transformPerObject(x: d0.d0.Obj): d1.d1.Arrow =
      self.transformPerObject(x.asInstanceOf[self.d0.d0.Obj]).asInstanceOf[d1.d1.Arrow]
  }

  private[cat] lazy val asMap: Map[d0.d0.Obj, d1.d1.Arrow] =
    if (d0.d0.isFinite) d0.d0.objects map (o => o -> transformPerObject(o)) toMap else Map.empty
  
  override lazy val hashCode: Int = d0.hashCode | d1.hashCode*17 | asMap.hashCode*31
  
  override def toString: String = {
    val s = String valueOf tag
    if (s.isEmpty) details else s
  }
  
  def details = s"NT($tag)(${
    if (domainCategory.isFinite) {
      domainCategory.listOfObjects.map(o => s"$o->(${transformPerObject(d0.d0.node(o))})").mkString(", ")
    } else ""
  })"
  
  override def equals(x: Any): Boolean = equalsWithDetails(x, printDetails = false)

  private[cat] def equalsWithDetails(x: Any, printDetails: Boolean): Boolean = x match {
    case other: NaturalTransformation =>
      (this eq other) || (
        hashCode == other.hashCode &&
          d0 == other.d0 &&
          d1 == other.d1 && {
          val foundBad: Option[Any] = domainCategory.objects find (o => {
            val first: d1.d1.Arrow = transformPerObject(d0.d0.node(o))
            val second = other.transformPerObject(o.asInstanceOf[other.d0.d0.Obj])
            val same = first == second
            if (!same && printDetails) {
              val f1 = first.asInstanceOf[SetFunction].toSet.toMap
              val f2 = second.asInstanceOf[SetFunction].toSet.toMap
              if (f1.keySet != f2.keySet) {
                val badkeys = f1.keySet.diff(f2.keySet).union(f2.keySet.diff(f1.keySet))
                println("wow, bad keys $badkeys")
              } else {
                val whatbad = f1.keySet.find(k => f1(k) != f2(k))
                println(whatbad)
              }
            }
            !same
          }
            ) // checking it every time takes time

          foundBad.isEmpty
        })
    case otherwise => false
  }
}

object NaturalTransformation {

  def validate[
  X <: Category,
  Y <: Category
  ]( // transforming `f` to `g`
    f: Functor, g: Functor, domainCategory: Category, codomainCategory: Category)(
    transformPerObject: f.d0.Obj => f.d1.Arrow
  ): Outcome = {
      
    OKif(domainCategory == g.d0, s"Functors must be defined on the same categories") andAlso
        OKif(codomainCategory == g.d1, s"Functors must map to the same categories") andAlso
    Result.traverse {
      for {
        a <- f.d0.arrows
      } yield {
        val x0: f.d0.Obj = f.d0.d0(a)
        val x1: f.d0.Obj = f.d0.d1(a)
        val faOpt = forValue(f.arrowsMapping(a))
        val gaOpt = forValue(g.arrowsMapping(g.d0.arrow(a)))
        val rr = for {
          fa <- faOpt
          ga <- gaOpt
        } yield {
          val r = Result.forValue {
            val tx0: f.d1.Arrow = transformPerObject(x0)
            val tx1: f.d1.Arrow = transformPerObject(x1)
            val rightdown: Option[f.d1.Arrow] = f.d1.m(fa, tx1) // a: x0->x1, fa: F[x0]->F[x1]; tx1: F[x1]->G[x1]
            val downright: Option[f.d1.Arrow] = f.d1.m(tx0, f.d1.arrow(ga))
            val checked = rightdown == downright
            require(checked, s"Nat'l transform law broken for $a")
          }
          r
        }

        val r = rr.flatten
        r
      }
  }
  }

  def build0(theTag: Any = "", from0: Functor, to0: Functor)
    (
      mappings: from0.d0.Obj => from0.d1.Arrow
    ): Any = {
    //    if (1 == 1) throw new IllegalArgumentException(s"domain ${from0} missing for NT $theTag")
    require(from0 != null, s"domain missing for NT $theTag")
    notNull(from0, s"domain missing for NT $theTag")
    notNull(to0, s"codomain missing for NT $theTag")
    val validated = validate(from0, to0, from0.d0, from0.d1)(mappings)
    
    validated
  }
  
  /**
    * Builds a natural transformation
    *
    * @param from0 first functor
    * @param to0   second functor
    * @param mappings a set morphism that for each domain object x returns f(x) -> g(x)
    */
  def build(theTag: Any = "", from0: Functor, to0: Functor)
  (
    mappings: from0.d0.Obj => from0.d1.Arrow
  ): Result[NaturalTransformation] = {
//    if (1 == 1) throw new IllegalArgumentException(s"domain ${from0} missing for NT $theTag")
    require(from0 != null, s"domain missing for NT $theTag")
    
    notNull(from0, s"domain missing for NT $theTag")
    notNull(to0, s"codomain missing for NT $theTag")
    val validated = validate(from0, to0, from0.d0, from0.d1)(mappings)
    validated returning new NaturalTransformation {
      val tag: Any = theTag
      val d0: Functor = from0
      val d1: Functor = to0
      override def transformPerObject(x: d0.d0.Obj): d1.d1.Arrow =
        codomainCategory.arrow(mappings(from0.d0.obj(x)))
    }
  }

  /**
    * Builds an identity natural transformation id[f]: f -> f
    *
    * @param functor the functor for which we are building the identity transformation
    * @return identity natural transformation for the functor
    */
  def id(functor: Functor): NaturalTransformation = {

    def objectMap(x: functor.d0.Obj): functor.d1.Arrow =
      functor.d1.id(functor.d1.obj(functor.objectsMapping(x)))

    new NaturalTransformation {
      val tag = "Id"
      val d0: Functor = functor
      val d1: Functor = functor

      override def transformPerObject(x: d0.d0.Obj): d1.d1.Arrow =
        codomainCategory.arrow(objectMap(functor.d0.obj(x)))
    }
  }

}
