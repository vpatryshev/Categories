package math.sets

import math.Base._

import scala.language.postfixOps

/**
  * Implements factorset functionality. A factorset may be built given a BinaryRelation,
  * or incrementally, when the client provides pairs of equivalent elements.
  * The factorset is not lazy; equivalence classes are stored in a map, and the resulting
  * factorset is returned as a new HashSet.
  *
  * @tparam X element type
  */
case class FactorSet[X](domain: Set[X]) extends Set[Set[X]]:

  /**
    * @return the latest version of factorset built here.
    */
  lazy val content: Set[Set[X]] = equivalenceClasses.values.toSet
  
  /**
    * Maps elements of the main set to their equivalence classes (they constitute the factorset).
    */
  private var equivalenceClasses: Map[X, Set[X]] =
    domain.foldLeft(Map[X, Set[X]]()) ((m, x) => m + (x -> Set(x)))

  /**
    * Builds a factorset of a given set, by the transitive closure of a given relation.
    *
    * @param sourceSet base set
    * @param r   binary relation
    */
  def this(sourceSet: Set[X], r: BinaryRelation[X, X]) =
    this(sourceSet)
    factorByRelation(r)

  /**
    * Given a <code>BinaryRelation r</code>, merges equivalent classes if they
    * contain elements that are in <code>r</code>.
    *
    * @param r the binary relation. Does not have to be symmetrical or transitive.
    */
  private def factorByRelation(r: BinaryRelation[X, X]): Unit =
    for {
      x1 <- domain
      x2 <- domain
      if r(x1, x2) || r(x2, x1)} merge(x1, x2)

  /**
    * Merges equivalence classes for two elements
    *
    * @param x1 first element
    * @param x2 second element
    */
  def merge(x1: X, x2: X): Unit =
    for (
      class1 <- equivalenceClasses.get(x1);
      class2 <- equivalenceClasses.get(x2) if x1 != x2) {
      val merged: Set[X] = class1 ++ class2
      for (x3 <- merged) {
        equivalenceClasses = equivalenceClasses + (x3 -> merged)
      }
    }

  /**
    * @return the function from the domain set to the factorset.
    */
  def asFunction: Function[X, Set[X]] = equivalenceClasses

  override def contains(elem: Set[X]): Boolean = equivalenceClasses.exists(_._2 == elem)

  override def incl(elem: Set[X]): Set[Set[X]] = itsImmutable

  override def excl(elem: Set[X]): Set[Set[X]] = itsImmutable

  override def iterator: Iterator[Set[X]] = equivalenceClasses.values.iterator

