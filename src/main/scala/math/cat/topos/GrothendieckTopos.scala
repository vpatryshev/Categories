package math.cat.topos

import math.cat.{NaturalTransformation, SetFunction}
import math.sets.Sets._
import scalakittens.Result

trait GrothendieckTopos extends Topos { this: CategoryOfDiagrams =>

  /**
    * Omega, subobject classifier.
    */
  object Ω extends Diagram("Ω", domain) {
    // For each object `x` we produce a set of all subobjects of `Representable(x)`.
    // These are values `Ω(x)`. We cache them in the following map map `x ⇒ Ω(x)` .
    private val subrepresentablesIndexed: Map[domain.Obj, Set[Diagram]] = subobjectsOfRepresentables

    // this one is consumed by Functor constructor
    val objectsMapping: d0.Obj ⇒ d1.Obj =
      (d: d0.Obj) ⇒ d1.obj(subrepresentablesIndexed(domain.obj(d): domain.Obj))

    // for each arrow `a: x -> y` produce a transition `Ω(x) -> Ω(y)`.
    private def am(a: domain.Arrow): SetFunction = {
      val x = domain.d0(a)
      val y = domain.d1(a)
      val d0 = subrepresentablesIndexed(x) // `Ω(x)` = all subobjects of `Representable(x)`
      val d1 = subrepresentablesIndexed(y) // `Ω(y)` = all subobjects of `Representable(y)`

      // How one diagram is transformed via `a`:
      // For each `rx ⊂ Repr(x)` we have to produce a diagram `ry ⊂ Repr(y)`
      def diaMap(rx: Diagram): Diagram /*a subrepresentable on `x`*/ = {
        // this is how elements of objects projections, that is, subterminals, are transformed by `a`
        val om1 = transformingOfSubrepresentables(a, rx) _

        // this is how, given an arrow `b`, the new diagram gets from one point to another
        def am1(b: domain.Arrow): SetFunction = {
          val x1 = om1(domain.d0(b)) //  {f ∈ hom(y, d0(b)) | a o f ∈ r1(d0(b)}
          val y1 = om1(domain.d1(b)) //  {f ∈ hom(y, d1(b)) | a o f ∈ r1(d1(b)}

          // A function fom x1 to y1 - it does the transition
          SetFunction.build("", x1, y1, g ⇒ domain.m(domain.arrow(g), b).get // it must be defined
          ) iHope
        }

        Diagram.build("", domain)(om1, am1) iHope
      }

      SetFunction.build(s"[$a]", d0.untyped, d1.untyped, d ⇒ diaMap(d.asInstanceOf[Diagram])).iHope
    }

    val arrowsMappingCandidate: d0.Arrow ⇒ d1.Arrow =
      (a: XArrow) ⇒ d1.arrow(am(domain.arrow(a)))

    /**
      * Given an arrow `a`, 
      * {f ∈ hom(y, x1) | a o f ∈ r1(x1)}
      *
      * @param a  an arrow
      * @param rx a subrepresentable
      * @param x1 an object in domain (a "state")
      * @return
      */
    private def transformingOfSubrepresentables(a: domain.Arrow, rx: Diagram)(x1: domain.Obj): set = {
      val y = domain.d1(a)
      val rx_at_x1 = rx(x1)
      for {
        f <- domain.hom(y, x1)
        candidate <- domain.m(a, domain.arrow(f))
        if rx_at_x1 contains candidate
      } yield f
    }

    validate iHope

    lazy val False: Point = Ω.points.head
    lazy val True: Point = Ω.points.last
  }

  /**
    * Gvien an inclusion (a natural transformation from a diagram A to a diagram B), and an object x in domain
    * produce a function that maps elements of A(x) to elements of Ω(x)
    * @param inclusion the inclusion
    * @param x an object of domain
    * @return a function A(x) -> Ω(x)
    */
  private[topos] def classifyingMapAt(inclusion: Arrow)(x: domain.Obj): SetFunction = {
    val A: Diagram = inclusion.d1
    val B: Diagram = inclusion.d0

    val Ax = A(x)
    val Bx = B(x) // Bx is a subset of Ax

    // for each element ax of set Ax find all arrows x->y 
    // that map ax to an ay that belongs to By 
    def myArrows(ax: Any): Set[(Any, set)] = {
      domain.objects map {
        y => {
          val all_arrows_to_y: domain.Arrows = domain.hom(domain.obj(x), y)
          def image_via(f: domain.Arrow) = A.functionForArrow(f)(ax)
          val By = B(y)
          def hits_By(f: domain.Arrow) = By.contains(image_via(f))
          y -> Ω.toSet(all_arrows_to_y.filter(hits_By))
        }
      }
    }
    
    def sameMapping(repr: Diagram, mapping: Map[Any, set]): Boolean = {
      domain.objects.forall(o => mapping(o) == repr(o))
    }
    
    def myRepresentable(ax: Any): Any = {
      val arrows = myArrows(ax).toMap
      val choices = Ω(x) find {
        repr => sameMapping(repr.asInstanceOf[Diagram], arrows)
      }
      scalakittens.Result(choices).orCommentTheError(s"No representable found for $ax -> $arrows").iHope
    }
    
    val fOpt = SetFunction.build(s"[χ($x)]", Ax, Ω(x), ax => myRepresentable(ax))
    fOpt.iHope
  }

  /**
    * Builds a map that classifies a subobject
    * B ---> 1
    * v      v
    * |      |
    * v      v
    * A ---> Ω
    * 
    * @param inclusion B >--> A - a natural transformation from diagram B to diagram A
    * @return A -> Ω
    */
  def classifyingMap(inclusion: Arrow): Arrow = {
    val objToFunction: domain.Obj => SetFunction = classifyingMapAt(inclusion)
    
    val ntOpt = NaturalTransformation.build(inclusion.d1, Ω)(x => inclusion.d1.d1.arrow(objToFunction(domain.obj(x))))
    
    ntOpt.iHope
  }

}
