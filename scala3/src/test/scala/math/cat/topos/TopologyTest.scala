package math.cat.topos

import math.cat.Categories._
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import scalakittens.{Good, Result}

import scala.language.reflectiveCalls

class TopologyTest extends Fixtures:
  
  def topologiesTested(cat: Cat): List[Result[LawvereTopology]] =
    val topos = new CategoryOfDiagrams(cat)
    import topos._
    val subs: List[Diagram] = Ω.subobjects.toList

    val inclusionsToΩ =
      subs map (inclusionOf(_)  in Ω) collect { case Good(incl) => incl }

    val builder = LawvereTopology.forPredicate(topos)

    val predicates = inclusionsToΩ map predicateForArrowToΩ

    predicates map builder
  
  def topologies(cat: Cat): List[LawvereTopology] =
    topologiesTested(cat) collect { case Good(topo) => topo}
  
  "Topologies" should {
    "exist for _0_" in {
      val all = topologies(_0_)
      all.length === 1
    }

    "exist for _1_" in {
      val candidates = topologiesTested(_1_)
      candidates.size === 4
      
      expectOk(candidates(3))
      expectError(candidates(0), "Should contain truth")
      expectOk(candidates(1))
      expectError(candidates(2), "Should be closed:", "under conjunction")
    }

    "exist for _2_" in {
      topologies(_2_).size === 4
    }

    "exist for _3_" in {
      topologies(_3_).size === 8
    }

    "exist for ParallelPair" in {
      topologies(ParallelPair).size === 4
    }

    "exist for Pullback" in {
      topologies(Pullback).size === 8
    }

    "exist for Pushout" in {
      topologies(Pushout).size === 8
    }

    "exist for Z3" in {
      topologies(Z3).size === 2
    }

    "exist for HalfSimplicial" in {
      topologies(Simplicial3).size === 6
    }

  }
