package math.cat.topos

import scalakittens.Result
import scalakittens.Result._

/**
 * See https://ncatlab.org/nlab/show/Lawvere-Tierney+topology
 * 
 * TODO: add tests and examples
 */
trait Topology[O,A]:

  // inclusion of this topology into topos.Ω
  def inclusion: A
  
  // classifying arrows for this inclusion
  def closure: A

object Topology:
  private type ToposWith[O, A] = Topos:
    type Obj = O
    type Arrow = A

  private def containsTruth[O, A](topos: ToposWith[O,A], inclusion: A): Boolean = false
  private def isClosed[O, A](topos: ToposWith[O,A], inclusion: A): Boolean = false
  private def closedUnderConjunction[O, A](topos: ToposWith[O,A], inclusion: A) = false
  
  def forInclusion[O, A](theTopos: ToposWith[O,A], theInclusion: A): Result[Topology[O, A]] =
    lazy val closureOp = ???
    OKif(containsTruth(theTopos, theInclusion), "Should contain truth") andAlso
    OKif(isClosed(theTopos, theInclusion), "Should be closed") andAlso
    OKif(closedUnderConjunction(theTopos, theInclusion), "Should be closed under conjunction") returning
    new Topology[O,A]:
      def topos: ToposWith[O,A] = theTopos
      def inclusion: A = theInclusion
      def closure: A = closureOp
