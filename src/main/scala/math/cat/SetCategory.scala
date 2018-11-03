package math.cat

import java.io.Reader

import j.math.cat.BasePair
import math.cat.Sets._
import SetCategory._

/**
  * Category where objects are sets
  *
  * @author vpatryshev
  */


  class SetCategory(objects: BigSet[Set[Any]]) extends
    Category[Set[Any], TypelessSetMorphism](graphOfSets(objects)) {

  def unit(x: Set[Any]): TypelessSetMorphism = TypelessSetMorphism.unit(x)

  /*override*/ def m(f: TypelessSetMorphism, g: TypelessSetMorphism): Option[TypelessSetMorphism] = {
    require(f.d1 eq g.d0, "Domain and codomain should match")
    f compose g
  }

  override def validate(): Unit

  = {
    // it IS a category
  }

  override def toString: String = "Category of all Scala Sets"

  override def hom(x: Set[Any], y: Set[Any]): Set[TypelessSetMorphism] =
    TypelessSetMorphism.exponent(x, y)

  override def isMonomorphism(f: TypelessSetMorphism): Boolean =
    f.d0.forall(x => f.d0.forall(y => !(f(x) == f(y)) || x == y))

  override def isEpimorphism(arrow: TypelessSetMorphism): Boolean =
    arrow.d1.forall(y => arrow.d0.exists(x => y == arrow(x)))

  //  @Override
  override def equalizer(f: TypelessSetMorphism, g: TypelessSetMorphism): Option[TypelessSetMorphism] = {
    require((f.d0 eq g.d0) && (f.d1 eq g.d1))
    Option(TypelessSetMorphism.inclusion(f.d0, x => f(x) == g(x)))
  }

  override def coequalizer(f: TypelessSetMorphism, g: TypelessSetMorphism): Option[TypelessSetMorphism] = {
    require(areParallel(f, g))
    val factorset: Sets.FactorSet[Any] = new Sets.FactorSet[Any](f.d1)

    for (x <- f.d0) {
      factorset.merge(f(x), g(x))
    }
    Option(TypelessSetMorphism.forFactorset(factorset))
  }

  override val unit: Set[Any] => TypelessSetMorphism =
    TypelessSetMorphism.unit
  
  override val m: (TypelessSetMorphism, TypelessSetMorphism) => Option[TypelessSetMorphism] = (f, g) => f compose g
}

object SetCategory {
  private type Sets = BigSet[Set[Any]]

  private def graphOfSets(nodes: Sets): Graph[Set[Any], TypelessSetMorphism] = {
    val arrows = BigSet[TypelessSetMorphism]()

    new Graph[Set[Any], TypelessSetMorphism](nodes, arrows, _.d0, _.d1)
  }

//  val Setf = new SetCategory(FiniteSets)
}
