package math.cat.topos

import math.cat.Categories.*
import scalakittens.Result.Oops
import scalakittens.{Bad, Good, Result}

import scala.language.reflectiveCalls

trait TestTopologies:

  def topologyCandidates(topos: CategoryOfDiagrams) =
    import topos._
    val subs: List[Diagram] = Ω.subobjects.toList

    val inclusionsToΩ =
      subs map { sub => inclusionOf(sub) in Ω
      } collect { case Good(incl) => incl }

    inclusionsToΩ map predicateForArrowToΩ

  def topologyCandidatesContainingTruth(topos: CategoryOfDiagrams) =
    topologyCandidates(topos).filter(p => LawvereTopology.mustContainTruth(topos)(p).isGood)

  def topologiesTested(topos: CategoryOfDiagrams): Map[String, Result[LawvereTopology]] =
    val predicates: List[topos.Predicate] = topologyCandidates(topos)
    val builder = LawvereTopology.forPredicate(topos)
    predicates.map(p => p.tag -> builder(p)).toMap

  def topologies(topos: CategoryOfDiagrams): Iterable[LawvereTopology] =
    topologiesTested(topos) collect { case (tag, Good(topo)) => topo }
