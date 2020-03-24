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
  private[topos] var DEBUG: Boolean = false


  def forPredicate(theTopos: CategoryOfDiagrams): theTopos.Predicate => Result[LawvereTopology] =
    (predicate: theTopos.Predicate) => {
      lazy val closureOp = ???
      mustContainTruth(theTopos)(predicate) andAlso
        mustBeClosed(theTopos)(predicate) andAlso
        mustBeClosedUnderConjunction(theTopos)(predicate) returning
        new LawvereTopology {
          val tag: String = s"topology(${predicate.tag})"
          def topos: CategoryOfDiagrams = theTopos

          def inclusion: theTopos.Predicate = predicate

          def closure: theTopos.Predicate = closureOp
        }
    }

  def mustContainTruth(topos: CategoryOfDiagrams)(predicate: topos.Predicate): Outcome =
    OKif(topos.Ω.True ∈ predicate.d0, s"Should contain truth: ${predicate.tag}")

  def mustBeClosed[O, A](topos: CategoryOfDiagrams)(predicate: topos.Predicate): Outcome = {
    val j = topos.χ(predicate)
    val jj = j compose j
    val sameThing = jj.equalsWithDetails(j, DEBUG)
    if (DEBUG) println(sameThing)
    OKif(jj == j, s"Should be closed: ${predicate.tag}")
  }

  // like j^j=j, or something?
  def mustBeClosedUnderConjunction[O, A](topos: CategoryOfDiagrams)(predicate: topos.Predicate): Outcome = {
    val j = topos.χ(predicate)
    
    // have to compare /\ o pair(j,j) and j o /\ from OmegaxOmega to Omega
    // question is, how do we build pair(j,j)? No definition yet
    OKif(false, s"Should be closed under conjunction: ${predicate.tag}")
  }
}
