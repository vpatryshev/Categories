package math.cat.topos

import scala.language.reflectiveCalls
import math.cat.Categories._
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import scalakittens.{Good, Result}

class TopologyTest extends Fixtures {
  
  def topologiesOn(cat: Cat): List[Result[LawvereTopology]] = {
    val topos = new CategoryOfDiagrams(cat)
    import topos._
    val subs: List[Diagram] = Ω.subobjects.toList

    val inclusions = subs map (inclusionOf(_)  in Ω)

    val builder = LawvereTopology.forPredicate(topos)

    val predicates = Result.traverse( for {
      fOpt <- inclusions
      predicateOpt = fOpt map predicateFor
    } yield predicateOpt).iHope.toList

    val topologiesMaybe = predicates map builder
    topologiesMaybe
  }
  
  "Topologies" should {
    "exist for _0_" in {
      val topologiesMaybe = topologiesOn(_0_)
      topologiesMaybe.length === 1
      expectOk(topologiesMaybe.head)
    }

    "exist for _1_" in {
      val topologiesMaybe = topologiesOn(_1_)
      topologiesMaybe.size === 4
      
      expectOk(topologiesMaybe(3))
      expectError(topologiesMaybe(0), "Should contain truth")
      expectOk(topologiesMaybe(1))
      expectError(topologiesMaybe(2), "Should be closed:", "under conjunction")
    }

    "exist for _2_" in {
      val topologiesMaybe = topologiesOn(_2_)
      topologiesMaybe.size === 15
      val topologies = topologiesMaybe collect { case Good(topo) => topo}
      topologies.size === 4
    }

    "exist for _3_" in {
      val topologiesMaybe = topologiesOn(_3_)
      topologiesMaybe.size === 64
      val topologies = topologiesMaybe collect { case Good(topo) => topo}
      topologies.size === 8
    }

    "exist for ParallelPair" in {
      val topologiesMaybe = topologiesOn(ParallelPair)
      topologiesMaybe.size === 39

      val topologies = topologiesMaybe collect { case Good(topo) => topo}
      topologies.size === 4
    }

    "exist for Pullback" in {
      val topologiesMaybe = topologiesOn(Pullback)
      topologiesMaybe.size === 85

      val topologies = topologiesMaybe collect { case Good(topo) => topo}
      topologies.size === 8
    }

    "exist for Pushout" in {
      val topologiesMaybe = topologiesOn(Pushout)
      topologiesMaybe.size === 73

      val topologies = topologiesMaybe collect { case Good(topo) => topo}
      topologies.size === 8
    }

    "exist for Z3" in {
      val topologiesMaybe = topologiesOn(Z3)
      topologiesMaybe.size === 4

      val topologies = topologiesMaybe collect { case Good(topo) => topo}
      topologies.size === 2
    }

    "exist for HalfSimplex" in {
      val topologiesMaybe = topologiesOn(Simplex3)
      topologiesMaybe.size === 27

      val topologies = topologiesMaybe collect { case Good(topo) => topo}
      topologies.size === 6
    }

  }
}
