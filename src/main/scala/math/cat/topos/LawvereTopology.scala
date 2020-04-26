package math.cat.topos

import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import scalakittens.Result
import scalakittens.Result._

/**
  * See https://ncatlab.org/nlab/show/Lawvere-Tierney+topology
  */
trait LawvereTopology {
  def tag: String

  def topos: CategoryOfDiagrams

  // inclusion of this topology into topos.Ω
  def inclusion: DiagramArrow

  // classifying arros for this inclusion
  def closure: DiagramArrow
}

object LawvereTopology {
  
  def forPredicate(theTopos: CategoryOfDiagrams): theTopos.Predicate => Result[LawvereTopology] =
    (predicate: theTopos.Predicate) => {
        val closureOp = theTopos.χ(predicate)
        mustContainTruth(theTopos)(predicate) andAlso
        mustBeClosed(theTopos)(closureOp) andAlso
        mustBeClosedUnderConjunction(theTopos)(closureOp) returning
        new LawvereTopology {
          val tag: String = s"topology(${predicate.tag})"
          val topos: CategoryOfDiagrams = theTopos

          val inclusion: topos.Predicate = predicate.typed[topos.Predicate] iHope

          val closure: topos.Predicate = closureOp.typed[topos.Predicate] iHope
        }
    }

  def mustContainTruth(topos: CategoryOfDiagrams)(predicate: topos.Predicate): Outcome = {
    OKif(topos.Ω.True ∈ predicate.d0.typed[Diagram].iHope, s"Should contain truth: ${predicate.tag}")
  }

  def mustBeClosed[O, A](topos: CategoryOfDiagrams)(j: topos.Predicate): Outcome = {
    val jj = j andThen j
    OKif(jj == j, s"Should be closed: ${j.tag}")
  }

  def mustBeClosedUnderConjunction[O, A](topos: CategoryOfDiagrams)(j: topos.Predicate): Outcome = {
    val jxj = topos.productOfArrows(j, j)
    val ∧ = topos.Ω.conjunction
    OKif((jxj andThen ∧) == (∧ andThen j),s"Should be closed under conjunction: ${j.tag}")
  }
}
