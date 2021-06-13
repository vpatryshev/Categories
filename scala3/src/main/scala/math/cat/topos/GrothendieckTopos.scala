package math.cat.topos

import scala.language.postfixOps
import reflect.Selectable.reflectiveSelectable
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.cat.{Category, CategoryData, Functor, NaturalTransformation, SetFunction}
import math.sets.Functions
import math.sets.Sets
import math.sets.Sets._
import scalakittens.Result
import Result._
import math.Base.concat
import math.cat.SetMorphism

import scala.collection.mutable

// see also http://www.cs.man.ac.uk/~david/categories/book/book.pdf - ML implementation of topos

trait GrothendieckTopos
  extends GrothendieckToposLogic
{ topos =>
  type Obj = Diagram
  type Arrow = DiagramArrow
   
  val domain: Category

  type Mapping = domain.Obj => Any => Any

  def inclusionOf(p: Point): { def in(diagram: Diagram): Result[DiagramArrow] }

  private[topos] def subobjectsOfRepresentables: Map[domain.Obj, Set[Diagram]]

  /**
    * Subobject classifier. Ω is "Option-Z" on your Mac.
    */
  object Ω extends Diagram("Ω", this) { Ω =>
    // For each object `x` we produce a set of all subobjects of `Representable(x)`.
    // These are values `Ω(x)`. We cache them in the following map map `x => Ω(x)` .
    private[topos] val subrepresentablesIndexed: Map[domain.Obj, Set[Diagram]] = subobjectsOfRepresentables

    // this one is consumed by Functor constructor
    val objectsMapping: d0.Obj => d1.Obj =
      (d: d0.Obj) => d1.obj(subrepresentablesIndexed(domain.obj(d): domain.Obj))

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
        def om1(o: domain.Obj): set = transformingOfSubrepresentables(a, rx)(o)
        def om2(o: Ω.topos.domain.Obj): set = om1(domain.asObj(o))

        // this is how, given an arrow `b`, the new diagram gets from one point to another
        def am1(b: Ω.topos.domain.Arrow): SetFunction = {
          val same_b = domain.arrow(b)
          val x1 = om1(domain.d0(same_b)) //  {f ∈ hom(y, d0(b)) | a compose f ∈ r1(d0(b)}
          val y1 = om1(domain.d1(same_b)) //  {f ∈ hom(y, d1(b)) | a compose f ∈ r1(d1(b)}

          // A function fom x1 to y1 - it does the transition
          new SetFunction("", x1, y1, g => domain.m(domain.arrow(g), same_b).get)
        }

        Diagram("", topos)(om2, am1) // no validation, we know it's ok
      }

      // no validation here, the function is known to be ok
      new SetFunction(s"[$a]", d0.untyped, d1.untyped, d => diaMap(d.asInstanceOf[Diagram]))
    }

    protected val arrowsMappingCandidate: d0.Arrow => d1.Arrow =
      (a: XArrow) => d1.arrow(am(domain.arrow(a)))

    /**
      * Given an arrow `a`, 
      * {f ∈ hom(y, x1) | a compose f ∈ r1(x1)}
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

    Functor.validateFunctor(this) iHope

    // TODO: redefine as classifying an empty
    lazy val False: Point = points.head named "⊥"

    // TODO: redefine as classifying an identity
    lazy val True: Point = points.last named "⊤"

    /**
      * Intersection of two subrepresentables on object `x`
      * @param a first subrepresentable
      * @param b second subrepresentable
      * @return their intersection
      */
    private[topos] def intersection(a: Diagram, b: Diagram): Diagram = {
      val om = (o: domain.Obj) => a(o) & b(o)

      // this is how, given an arrow `b`, the new diagram gets from one point to another
      def am(f: domain.Arrow): SetFunction = {
        val x = om(domain.d0(f))
        val y = om(domain.d1(f))

        a.asFunction(a.arrowsMapping(a.d0.arrow(f))).restrictTo(x, y).iHope
      }

      Diagram(s"${a.tag} ∩ ${b.tag}", topos)(
        o => om(domain.obj(o)),
        f => am(domain.arrow(f))
      )
    }
    
    lazy val conjunction: DiagramArrow = {

      def conjunctionOfTwoSubreps(pair: Any): Diagram = pair match {
        case (a: Diagram, b: Diagram) => 
          intersection(a,b)
        case bs => 
          throw new IllegalArgumentException(s"Expected a pair of diagrams, got $bs")
      }

      def calculatePerObject(x: ΩxΩ.d0.Obj): SetFunction = {
        val dom = ΩxΩ(x)
        val codom = Ω(x)
        new SetFunction(s"∧[$x]", dom.untyped, codom, pair => conjunctionOfTwoSubreps(pair))
      }


      val cache: mutable.Map[ΩxΩ.d0.Obj, SetFunction] =
        mutable.Map[ΩxΩ.d0.Obj, SetFunction]()
      
      new DiagramArrow {
        val tag = "∧"
        override val d0: Functor = ΩxΩ
        override val d1: Functor = Ω

        def perObject(x: d0.d0.Obj): SetFunction = {
          val x_in_ΩxΩ = x.asInstanceOf[ΩxΩ.d0.Obj]
          cache.getOrElseUpdate(x_in_ΩxΩ, calculatePerObject(x_in_ΩxΩ))
          
        }

        override def transformPerObject(x: domainCategory.Obj): codomainCategory.Arrow =
          codomainCategory.arrow(perObject(d0.d0.obj(x)))
      }
    }

    lazy val disjunction: DiagramArrow = {
      new DiagramArrow {
        val tag = "v"
        override val d0: Functor = ΩxΩ
        override val d1: Functor = Ω

        /**
          * Union of two subrepresentables on object `x`
          * @param a first subrepresentable
          * @param b second subrepresentable
          * @return their intersection
          */
        private def union(a: Diagram, b: Diagram): Diagram = {
          val om = (o: domain.Obj) => a.setAt(o) | b.setAt(o)

          // this is how, given an arrow `b`, the new diagram gets from one point to another
          def am(f: domain.Arrow): SetFunction = {
            val o = domain.d0(f)
            val ao = a(o)
            val bo = b(o)
            val x = om(o)
            val y = om(domain.d1(f))

            val functiona = a.asFunction(a.arrowsMapping(a.d0.arrow(f)))
            val functionb = b.asFunction(b.arrowsMapping(b.d0.arrow(f)))
            def unionOfMappings(z: Any): Any =
              if (ao(z)) functiona(z)
              else if (bo(z)) functionb(z)
              else throw new IllegalArgumentException(s"$z was supposed to be in $ao or in $bo")

            new SetFunction("", x, y, unionOfMappings)
//            val unionFunctionMaybe = SetFunction.build(x, y, unionOfMappings)
//            unionFunctionMaybe.iHope
          }

          val tag = s"${a.tag} ∪ ${b.tag}"

          Diagram(tag, topos)(
            o => om(domain.obj(o)), f => am(domain.arrow(f)))
        }

        def disjunctionOfTwoSubreps(pair: Any): Diagram = pair match {
          case (a: Diagram, b: Diagram) => union(a,b)
        }

        def perObject(x: d0.d0.Obj): SetFunction = {
          val dom = ΩxΩ(x)
          val codom = Ω(x)
          new SetFunction(s"v[$x]", dom.untyped, codom, pair => disjunctionOfTwoSubreps(pair))
//          SetFunction.build(s"v[$x]", dom.untyped, codom, pair => disjunctionOfTwoSubreps(pair)).iHope
        }

        override def transformPerObject(x: domainCategory.Obj): codomainCategory.Arrow =
          codomainCategory.arrow(perObject(d0.d0.obj(x)))
      }
    }

    lazy val implication: DiagramArrow = {
      val inclusion: DiagramArrow = inclusionOf(Ω1) in ΩxΩ iHope
      
      χ(inclusion, "=>")
    }
  }

  val ΩxΩ: Obj = product2(Ω, Ω)
 
  private lazy val firstProjectionOf_ΩxΩ =
    buildArrow("π1", ΩxΩ, Ω, firstProjection)

  /**
    * An equalizer of first projection and intersection
    */
  lazy val Ω1: Diagram = ΩxΩ.filter("<", _ => { case (a: Diagram, b: Diagram) => a ⊂ b })

  /**
    * Diagonal for Ω
    */
  lazy val Δ_Ω: DiagramArrow = buildArrow("Δ", Ω, ΩxΩ,
    _ => (subrep: Any) => (subrep, subrep)
  )

  /**
    * Given an inclusion (a natural transformation from a diagram A to a diagram B), and an object x in domain
    * produce a function that maps elements of A(x) to elements of Ω(x)
    * @param inclusion the inclusion
    * @param x an object of domain
    * @return a function A(x) -> Ω(x)
    */
  private[topos] def χAt(inclusion: Arrow)(x: domain.Obj): SetFunction = {
    val A: Diagram = inclusion.d1.asInstanceOf[Diagram] // TODO: get rid of casting
    val B: Diagram = inclusion.d0.asInstanceOf[Diagram] // TODO: get rid of casting

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
    
    new SetFunction(s"[χ($x)]", Ax, Ω(x), ax => myRepresentable(ax))
  }

  /**
    * Builds a map that classifies a subobject
    * B ----> 1
    * v      v
    * |      |
    * v      v
    * A ----> Ω
    * 
    * @param inclusion B >--> A - a natural transformation from diagram B to diagram A
    * @return A -> Ω
    */
  def χ(inclusion: Arrow, theTag: Any): Predicate = {
    val objToFunction: domain.Obj => SetFunction = χAt(inclusion)

    new Predicate {
      val d0: Diagram = inclusion.d1.asInstanceOf[Diagram] // TODO: get rid of casting
      val tag = theTag

      override def transformPerObject(x: domainCategory.Obj): codomainCategory.Arrow =
        codomainCategory.arrow(objToFunction(domain.obj(x)))
    }
  }


  def χ(inclusion: Arrow): Predicate = {
    χ(inclusion, s"χ(${inclusion.tag})")
  }


  trait Includer {
    val subdiagram: Diagram

    def in(diagram: Diagram): Result[DiagramArrow] = {
      val results: IterableOnce[Result[(domain.Obj, subdiagram.d1.Arrow)]] = {
        for {
          x <- domain.objects.iterator
          in = SetFunction.inclusion(subdiagram(x), diagram(x))
          pair = in map (x -> subdiagram.d1.arrow(_))
        } yield pair
      }

      val mapOpt = Result traverse results

      val result = for {
        map <- mapOpt
        arrow <- NaturalTransformation.build(s"${subdiagram.tag}⊂${diagram.tag}", subdiagram, diagram)(map.toMap)
      } yield arrow

      result
    }
  }

  def inclusionOf(diagram: Diagram): Includer =
    new Includer { val subdiagram: Diagram = diagram }

  /**
    * Builds a `DiagramArrow`, given domain, codomain, and a mapping
    * @param tag arrow tag
    * @param from domain
    * @param to codomain
    * @param mapping maps objects to functions
    * @return a natural transformation (crashes if not)
    */
  def buildArrow(tag: Any, from: Diagram, to: Diagram,
    mapping: Mapping): DiagramArrow = {
    NaturalTransformation.build(tag, from, to)(
      (o: from.d0.Obj) => buildOneArrow(tag, from, to, mapping)(o)).iHope
  }

  // π1
  protected val firstProjection: Mapping = Functions.constant[domain.Obj, Any => Any] {
    case (a, b) => a
    case trash =>
      throw new IllegalArgumentException(s"Expected a pair, got $trash")
  }

  // π2
  protected val secondProjection: Mapping = Functions.constant[domain.Obj, Any => Any] {
    case (a, b) => b
    case trash =>
      throw new IllegalArgumentException(s"Expected a pair, got $trash")
  }

  /**
    * Given a `from` and `to` diagrams, build an arrow
    * `from(o)` -> `to(o)`, for each given `o`,
    * using the provided mapping
    *
    * @param tag tag of a natural transformation
    * @param from domain diagram
    * @param to codomain diagram
    * @param mapping given an object `o`, produce a function over this object
    * @param o the object
    * @return an arrow (it's a `SetFunction`, actually)
    */
  protected def buildOneArrow(
    tag: Any,
    from: Diagram,
    to: Diagram,
    mapping: Mapping
  )(o: from.d0.Obj): from.d1.Arrow = {
    val domO: domain.Obj = domain.obj(o)
    val funOpt = SetFunction.build(s"$tag[$o]", from(o), to(o), mapping(domO))
    from.d1.arrow(funOpt.iHope)
  }

  /**
    * Given arrows `f` and `g`, builds an arrow (f×g): dom(f)×dom(g) -> codom(f)×codom(g)
    *
    * @param f first component
    * @param g second component
    * @return a product of `f` and `g`
    */
  def productOfArrows(f: DiagramArrow, g: DiagramArrow): DiagramArrow = {

    val mapping: Mapping = x => {
      val fx = f(x).asInstanceOf[SetMorphism[Any, Any]]
      val gx = g(x).asInstanceOf[SetMorphism[Any, Any]]

      { case (a, b) => (fx(a), gx(b)) }
    }

    val fd9: Any = f.d0
    buildArrow(
      concat(f.tag, "×", g.tag),
      product2(f.d0.asInstanceOf[Diagram], g.d0.asInstanceOf[Diagram]), // TODO: remove casting
      product2(f.d1.asInstanceOf[Diagram], g.d1.asInstanceOf[Diagram]), // TODO: remove casting
      mapping)
  }

  private[topos] case class product2builder(x: Diagram, y: Diagram) {

    private def productAt(o: domain.Obj) = Sets.product2(x(o), y(o))
    private def mappingOfObjects(o: domain.Obj): set = productAt(o).untyped

    def transition(z: Diagram)(a: domain.Arrow)(pz: Any) = {
      val sf: SetFunction = z.asFunction(z.arrowsMapping(z.d0.arrow(a)))
      sf(pz)
    }

    private def mappingOfArrows(a: domain.Arrow): SetFunction = {
      val from = productAt(domain.d0(a))
      val to = productAt(domain.d1(a))
      def f(p: Any): Any = p match {
        case (px, py) => (transition(x)(a)(px), transition(y)(a)(py))
        case other =>
          throw new IllegalArgumentException(s"Expected a pair of values, got $other")
      }
      new SetFunction("", from.untyped, to.untyped, f)
    }

    val diagram = Diagram(s"${x.tag}×${y.tag}", topos)(mappingOfObjects, a => mappingOfArrows(a))
  }

  /**
    * Cartesian product of two diagrams
    * TODO: figure out how to ensure the same d0 in both Di
    */
  def product2(x: Diagram, y: Diagram): Diagram = product2builder(x, y).diagram

  def standardInclusion(p: Point, d: Diagram): Result[DiagramArrow] = {
    inclusionOf(p) in d map { q => {
      uniqueFromTerminalTo(p) andThen q named p.tag
    } }
  }

}
