package math.cat

import scalakittens.Result
import Result._
import scalakittens.Result.Outcome

/**
  * Natural transformation class: morphisms for functors.
  *
  * The following three requirements are checked:
  * f and g are from the same category
  * f and g are to the same category
  * the following squares are commutative:
  *    f[a]: f[x] --→ f[y]
  *            |         |
  *       t[x] |         | t[y]
  *            |         |
  *            V         V
  *    g[a]: g[x] --→ g[y]
  */
abstract class NaturalTransformation extends Morphism[Functor, Functor] { self ⇒
  val tag: Any
  lazy val domainCategory: Category = d0.d0
  lazy val codomainCategory: Category = d1.d1 // == d0.d1, of course

  def transformPerObject(x: domainCategory.Obj): codomainCategory.Arrow
  def apply(x: Any): codomainCategory.Arrow = transformPerObject(domainCategory.obj(x))

  // TODO: check the preconditions, return an option
  def compose(next: NaturalTransformation): NaturalTransformation = {
    
    def comp(x: domainCategory.Obj): codomainCategory.Arrow = {
      val fHere: codomainCategory.Arrow =
        codomainCategory.arrow(transformPerObject(domainCategory.obj(x)))
      val fThere: codomainCategory.Arrow =
        codomainCategory.arrow(next.transformPerObject(next.domainCategory.obj(x)))
      val compOpt: Option[codomainCategory.Arrow] = codomainCategory.m(fHere, fThere)
      compOpt getOrElse(
          throw new IllegalArgumentException(s"Bad transformation for $x for $fHere and $fThere"))
    }

    def composed[T](x: T) = {
      comp(domainCategory.obj(x))
    }

    new NaturalTransformation {
      val tag = s"${next.tag} ∘ ${self.tag}"
      val d0: Functor = self.d0
      val d1: Functor = next.d1
      def transformPerObject(x: domainCategory.Obj): codomainCategory.Arrow = codomainCategory.arrow(composed(x))
    }
  }
  
  private[cat] lazy val asMap: Map[domainCategory.Obj, codomainCategory.Arrow] =
    if (domainCategory.isFinite) domainCategory.objects map (o ⇒ o → transformPerObject(o)) toMap else Map.empty
  
  override lazy val hashCode: Int = d0.hashCode | d1.hashCode*17 | asMap.hashCode*31
  
  override def toString = s"NT($tag)(${
    if (domainCategory.isFinite) {
      domainCategory.listOfObjects.map(o ⇒ s"$o→(${transformPerObject(o)})").mkString(", ")
    } else ""
  })"
  
  override def equals(x: Any): Boolean = x match {
    case other: NaturalTransformation ⇒
      (this eq other) || (
        hashCode == other.hashCode &&
        d0 == other.d0 &&
        d1 == other.d1 && {
          val foundBad: Option[Any] = domainCategory.objects find (o ⇒ {
            val first = transformPerObject(o)
            val second = other.transformPerObject(o.asInstanceOf[other.domainCategory.Obj])
            val same = first == second
// the following code is, sorry, for debugging. Maybe better to have tests. later.
//            if (!same) {
//              val f1 = first.asInstanceOf[SetFunction].toSet.toMap
//              val f2 = second.asInstanceOf[SetFunction].toSet.toMap
//              if (f1.keySet != f2.keySet) {
//                val badkeys = f1.keySet.diff(f2.keySet).union(f2.keySet.diff(f1.keySet))
//                println("wtf, bad keys $badkeys")
//                badkeys
//              } else {
//                val whatbad = f1.keySet.find(k ⇒ f1(k) != f2(k))
//
//                println(whatbad)
//                whatbad
//              }
//            }
            !same
          }
          ) // checking it every time takes time
          
          foundBad.isEmpty
        })
    case otherwise ⇒ false
  }
}

object NaturalTransformation {

  def validate[
  X <: Category,
  Y <: Category
  ]( // transforming `f` to `g`
    f: Functor, g: Functor, domainCategory: Category, codomainCategory: Category)(
    transformPerObject: f.d0.Obj ⇒ f.d1.Arrow
  ): Outcome =
    OKif(domainCategory == g.d0, s"Functors must be defined on the same categories") andAlso
    OKif(codomainCategory == g.d1, s"Functors must map to the same categories") andAlso
    Result.traverse {
    for {
      a ← f.d0.arrows
    } yield {
      val x0: f.d0.Obj = f.d0.d0(a)
      val x1: f.d0.Obj = f.d0.d1(a)
      val faOpt = forValue(f.arrowsMapping(a))
      val gaOpt = forValue(g.arrowsMapping(g.d0.arrow(a)))
      val rr = for {
        fa ← faOpt
        ga ← gaOpt
      } yield {
        val r = Result.forValue {
          val tx0: f.d1.Arrow = transformPerObject(x0)
          val tx1: f.d1.Arrow = transformPerObject(x1)
          val rightdown: Option[f.d1.Arrow] = f.d1.m(fa, tx1) // a: x0→x1, fa: F[x0]→F[x1]; tx1: F[x1]→G[x1]
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



  /**
    * Builds a natural transformation
    *
    * @param from0 first functor
    * @param to0   second functor
    * @param mappings a set morphism that for each domain object x returns f(x) → g(x)
    */
  def build(theTag: Any = "", from0: Functor, to0: Functor)
  (
    mappings: from0.d0.Obj ⇒ from0.d1.Arrow
  ): Result[NaturalTransformation] = {
    val validated = validate(from0, to0, from0.d0, from0.d1)(mappings)
    validated returning new NaturalTransformation {
      val tag = theTag
      val d0: Functor = from0
      val d1: Functor = to0
      override def transformPerObject(x: domainCategory.Obj): codomainCategory.Arrow =
        codomainCategory.arrow(mappings(from0.d0.obj(x)))
    }
  }

  /**
    * Builds an identity natural transformation id[f]: f → f
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
      
      override def transformPerObject(x: domainCategory.Obj): codomainCategory.Arrow =
        codomainCategory.arrow(objectMap(functor.d0.obj(x)))
    }
  }

}
