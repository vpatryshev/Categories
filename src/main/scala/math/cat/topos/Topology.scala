package math.cat.topos

import scalakittens.Result
import scalakittens.Result._

/**
  * See https://ncatlab.org/nlab/show/Lawvere-Tierney+topology
  */
trait Topology[O,A] {
  def topos: Topos[O,A]
  
  // inclusion of this topology into topos.Ω
  def inclusion: A
  
  // classifying arros for this inclusion
  def closure: A
}

object Topology {
  
  def containsTruth[O, A](topos: Topos[O, A], inclusion: A): Boolean = false
  def isClosed[O, A](topos: Topos[O, A], inclusion: A): Boolean = false
  def closedUnderConjunction[O, A](topos: Topos[O, A], inclusion: A) = false
  
  def forInclusion[O, A](theTopos: Topos[O, A], theInclusion: A): Result[Topology[O, A]] = {
    val closureOp = ???
    OKif(containsTruth(theTopos, theInclusion), "Should contain truth") andAlso
    OKif(isClosed(theTopos, theInclusion), "Should be closed") andAlso
    OKif(closedUnderConjunction(theTopos, theInclusion), "Should be closed under conjunction") returning
    new Topology[O,A] {
      def topos: Topos[O, A] = theTopos
      def inclusion: A = theInclusion
      def closure: A = closureOp
    }
  }
}
