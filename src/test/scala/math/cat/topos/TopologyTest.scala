package math.cat.topos

import scala.language.reflectiveCalls
import math.cat.Categories._
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import scalakittens.Result

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

    val topologies = predicates map builder
    topologies
  }
  
  "Topologies" should {
    "exist for _0_" in {
      val topologies = topologiesOn(_0_)
      topologies.length === 1
      expectOk(topologies.head)
    }

    "exist for _1_" in {
      val topologies = topologiesOn(_1_)
      topologies.size === 4
      
      expectOk(topologies(3))
      expectError(topologies(0), "Should contain truth")
      expectOk(topologies(1))
      expectError(topologies(2), "Should be closed:", "under conjunction")
    }

    "exist for _2_" in {
      val topologies = topologiesOn(_2_)
      topologies.size === 15

//      expectOk(topologies(3))
//      expectError(topologies(0), "Should contain truth")
//      expectOk(topologies(1))
//      expectError(topologies(2), "Should be closed:", "under conjunction")
    }

    "exist for _3_" in {
      val topologies = topologiesOn(_3_)
      topologies.size === 64

//      expectOk(topologies(3))
//      expectError(topologies(0), "Should contain truth")
//      expectOk(topologies(1))
//      expectError(topologies(2), "Should be closed:", "under conjunction")
    }

    "exist for ParallelPair" in {
      val topologies = topologiesOn(ParallelPair)
      topologies.size === 39

//      expectOk(topologies(3))
//      expectError(topologies(0), "Should contain truth")
//      expectOk(topologies(1))
//      expectError(topologies(2), "Should be closed:", "under conjunction")
    }

    "exist for Pullback" in {
      ok
    }

    "exist for Pushout" in {
      ok
    }

    "exist for Z3" in {
      ok
    }
  }
}
