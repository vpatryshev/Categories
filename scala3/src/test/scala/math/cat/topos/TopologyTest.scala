package math.cat.topos

import math.cat.Categories.*
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import scalakittens.Result.Oops
import scalakittens.{Bad, Good, Result}

import scala.language.reflectiveCalls

class TopologyTest extends Fixtures:

  def topologiesTested(topos: CategoryOfDiagrams): List[Result[LawvereTopology]] =
    import topos._
    val subs: List[topos.Diagramme] = Ω.subobjects.toList

    val inclusionsToΩ =
      subs map { sub =>
        try {
          val supbold = sub.asOldDiagram
          val incl = topos.inclusionOf(supbold) in Ω.asOldDiagram
          incl
        } catch {
          case e: Exception =>
            println(s"Failed on $sub")
            e.printStackTrace()
            Oops(e)
        }
      } collect { case Good(incl) => incl }

    val predicates = inclusionsToΩ map topos.predicateForArrowToΩ

    val builder = LawvereTopology.forPredicate(topos)

    predicates map builder

  def topologies(topos: CategoryOfDiagrams): List[LawvereTopology] =
    topologiesTested(topos) collect { case Good(topo) => topo}

  "Topologies" should {
    "  exist for`𝟘`" in {
      val all = topologies(`Set^𝟘`)
      all.length === 1
    }

    "  exist for`𝟙`" in {
      val candidates = topologiesTested(`Set^𝟙`)
      candidates.size === 4

      val topologies = candidates.filter(_.isGood)
      topologies.size === 2
      for topology <- topologies do expectOk(topology)
      
      ok
    }

    "  exist for _2_" in {
      topologies(`Set^_2_`).size === 4
    }

    "  exist for _3_" in {
      topologies(`Set^_3_`).size === 8
    }

    "  exist for ParallelPair" in {
      topologies(`Set^ParallelPair`).size === 4
    }

    "  exist for Pullback" in {
      topologies(`Set^Pullback`).size === 8
    }

    "  exist for Pushout" in {
      topologies(`Set^Pushout`).size === 8
    }

    "  exist for Z3" in {
      topologies(`Set^Z3`).size === 2
    }

    "  exist for HalfSimplicial" in {
      topologies(`Set^Simplicial`).size === 6
    }

  }
