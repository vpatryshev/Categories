package math.cat.topos

import math.cat.Categories.*
import scalakittens.Result.Oops
import scalakittens.{Bad, Good, Result}

import scala.language.reflectiveCalls

class TopologyTest extends Fixtures with TestTopologies:


  "Topologies" should :
    "  exist for 𝟘" in :

      val goodOnes = topologies(`Set^𝟘`)
      goodOnes.size === 1

    "  exist for 𝟙" in :
      val topologies = topologiesTested(`Set^𝟙`).filter(_._2.isGood)
      topologies.size === 2
      for topology <- topologies do expectOk(topology._1, topology._2)

      ok

    "  exist for 𝟚" in :
      topologies(`Set^𝟚`).size === 4

    "  exist for 𝟛" in :
      topologies(`Set^𝟛`).size === 8

    "  exist for ParallelPair" in :
      topologies(`Set^ParallelPair`).size === 4

    "  exist for Pullback" in :
      topologies(`Set^Pullback`).size === 8

    "  exist for Pushout" in :
      topologies(`Set^Pushout`).size === 8

    "  exist for Z3" in :
      topologies(`Set^Z3`).size === 2

    "  exist for HalfSimplicial" in :
      topologies(`Set^Simplicial`).size === 6
