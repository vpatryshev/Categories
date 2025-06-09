package math.cat.topos

import math.cat.Functor
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import scalakittens.Result
import scalakittens.Result.*

import scala.language.postfixOps

/**
  * See https://ncatlab.org/nlab/show/Lawvere-Tierney+topology
  */
trait LawvereTopology:
  def tag: String

  def topos: CategoryOfDiagrams

  // inclusion of this topology into topos.Ω
  def inclusion: DiagramArrow

  // classifying arrow for this inclusion
  def closure: DiagramArrow

object LawvereTopology:
  
  def forPredicate(theTopos: CategoryOfDiagrams): theTopos.Predicate => Result[LawvereTopology] =
    (predicate: theTopos.Predicate) =>
        val closureOp = theTopos.χ(predicate)
        mustContainTruth(theTopos)(predicate) andAlso
        mustBeClosed(theTopos)(closureOp) andAlso
        mustBeClosedUnderConjunction(theTopos)(closureOp) returning
        new LawvereTopology:
          val tag: String = s"topology(${predicate.tag})"
          val topos: CategoryOfDiagrams = theTopos

          val inclusion: topos.Predicate = predicate.typed[topos.Predicate] iHope

          val closure: topos.Predicate = closureOp.typed[topos.Predicate] iHope

  def mustContainTruth(topos: CategoryOfDiagrams): topos.Predicate => Outcome =
    (predicate: topos.Predicate) => OKif(predicate.containsTruth, s"Should contain truth: ${predicate.tag}")

  private def mustBeClosed[O, A](topos: CategoryOfDiagrams)(j: topos.Predicate): Outcome =
    val jj = j ∘ j
    OKif(jj == j, s"Should be closed: ${j.tag}")

  private def mustBeClosedUnderConjunction[O, A](topos: CategoryOfDiagrams)(j: topos.Predicate): Outcome =
    val jxj = topos.productOfArrows(j, j)
    val ∧ = topos.Ω.conjunction
    OKif((∧ ∘ jxj) == (j ∘ ∧), s"Should be closed under conjunction: ${j.tag}")
