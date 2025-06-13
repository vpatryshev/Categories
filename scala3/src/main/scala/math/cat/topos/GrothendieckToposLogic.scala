package math.cat.topos

import math.Base.concat
import math.cat.SetFunction.*
import math.cat.topos.CategoryOfDiagrams.{BaseCategory, DiagramArrow}
import math.cat.{Morphism, SetFunction}
import math.sets.Sets
import Sets.{set, setOf}
import scalakittens.{Cache, Params, Result}

import scala.collection.mutable
import scala.language.{implicitConversions, postfixOps}

trait GrothendieckToposLogic:
  topos: GrothendieckTopos =>

// TODO: use the right tagging, find the right predicate
//  private val cache = mutable.Map[(String, Predicate, Predicate), Predicate]()

  object Predicates:

    def tuplingAt(left: Predicate, right: Predicate, o: domain.Obj): SetFunction =
      val dom = left.setAt(o)
      if Params.fullCheck then
        require(right.setAt(o) == dom)

      val po: SetFunction = left.transformAt(o)
      val qo: SetFunction = right.transformAt(o)
      new SetFunction(
        s"PQ->ΩxΩ($o)",
        dom, ΩxΩ(o),
        v => (po(v), qo(v))
      )

    def binaryOpMappingAt(
                        ΩxΩ_to_Ω: DiagramArrow,
                        left: Predicate,
                        right: Predicate,
                        o: domain.Obj): Ω.d1.Arrow =
      val PQtoΩxΩ: SetFunction = tuplingAt(left, right, o)
      val pairAtEmpty = PQtoΩxΩ.mapping(Set())
      val op: SetFunction = ΩxΩ_to_Ω(o).asInstanceOf[SetFunction]
      val opAtPairAtEmpty = op.mapping(pairAtEmpty)
      val theMapping = PQtoΩxΩ andThen op
      val result: SetFunction = theMapping.getOrElse(throw new IllegalStateException("Failed to compose"))
      val resultAtEmpty = result.mapping(Set())
      result

  abstract class Predicate(myTag: String, override val d0: Diagram) extends DiagramArrow(myTag, d0, Ω):
    p: DiagramArrow =>

    def containsTruth: Boolean = Truth ∈ d0

    private def wrapTag(tag: Any): String =
      val ts = tag.toString
      if ts.contains("∧") || ts.contains("∨") || ts.contains("=>") then
        s"($ts)" else ts

    private def tag2(tag1: Any, op: String, tag2: Any): String = 
      concat(wrapTag(tag1), op, wrapTag(tag2))

    def setAt(o: Any): set =
      val function: SetFunction = p(o).asInstanceOf[SetFunction] // d1.d1.Arrow = SetFunction, and if we d
      setOf(function.d0)

    private[GrothendieckToposLogic] def transformAt(o: Any): SetFunction =
      apply(o).asInstanceOf[SetFunction]

    def binaryOp(ΩxΩ_to_Ω: DiagramArrow)(q: Predicate): Predicate =
      binaryOpNamed(ΩxΩ_to_Ω, ΩxΩ_to_Ω.tag)(q)

    def binaryOpNamed(ΩxΩ_to_Ω: DiagramArrow, opTag: String)(q: Predicate): Predicate =
      evalBinaryOp(ΩxΩ_to_Ω, opTag)(q)
      
    val bop: Cache[(DiagramArrow, String), Predicate => Predicate] =
      Cache[(DiagramArrow, String), Predicate => Predicate](
        (arrow, opTag) => Cache[Predicate, Predicate](evalBinaryOp(arrow, opTag)(_)))
    
    private def evalBinaryOp(ΩxΩ_to_Ω: DiagramArrow, newTag: String)(q: Predicate): Predicate =
      requireCompatibility(q)

      new Predicate(newTag, p.d0):
        def calculateMappingAt(o: d0.d0.Obj): d1.d1.Arrow =
          Predicates.binaryOpMappingAt(ΩxΩ_to_Ω, p, q, o)

    end evalBinaryOp
    
    private def requireCompatibility(q: Predicate): Unit =
      require(q.d0 == p.d0)

      for o <- domainCategory.objects do
        require(p.setAt(o) == q.setAt(o), s"Different domains at $o for ${p.tag} and ${q.tag}")

    /**
      * Conjunction with another predicate
      *
      * @return a function that takes another predicate and returns their conjunction
      */
    lazy val ∧ : Predicate => Predicate = binaryOp(Ω.conjunction)

    /**
      * Disjunction with another predicate
      * @return  a function that takes another predicate and returns their disjunction
      */
    lazy val ∨ : Predicate => Predicate = binaryOp(Ω.disjunction)

    /**
      * implication of another predicate
      * @return  a function that takes another predicate `q` and returns `this implies q`
      */
    lazy val ⟹ : Predicate => Predicate = binaryOp(Ω.implication)

  def ¬(p: topos.Predicate): topos.Predicate =
    p.binaryOpNamed(Ω.implication, "¬")(FalsePredicate)

  lazy val FalsePredicate: topos.Predicate = predicateFor(Falsehood)

  lazy val TruePredicate: topos.Predicate = predicateFor(Truth)

  /**
    * Builds a predicate for an arrow to Ω
    * @param f arrow from an object to Ω
    * @return an arrow X -> Ω
    */
  infix def predicateForArrowToΩ(f: DiagramArrow): topos.Predicate =
    f.d0 match
      case d: topos.Diagram =>
        new topos.Predicate(f.tag, d):
          override def calculateMappingAt(x: d0.d0.Obj): d1.d1.Arrow = f.calculateMappingAt(x)

      case basura => throw new IllegalArgumentException(s"WTF: basura $basura")

  /**
    * Builds a predicate for a point in Ω
    * For a given point, produces an arrow pt -> Ω
    */
  val predicateFor: Point => Predicate = Cache[topos.Point, Predicate](calculatePredicate)

  private infix def calculatePredicate(pt: Point): Predicate =

    val inclusion: DiagramArrow = topos.standardInclusion(pt, Ω) iHope

    new Predicate(pt.tag, _1):
      override def calculateMappingAt(x: d0.d0.Obj): d1.d1.Arrow =
        inclusion.calculateMappingAt(x)

  /**
    * An arrow from terminal to the point as a diagram
    * @return
    */
  def uniqueFromTerminalTo(p: Point): Arrow =
    new DiagramArrow(p.tag, _1, p.asDiagram):
      override def calculateMappingAt(o: d0.d0.Obj): d1.d1.Arrow =
        val value = p(o)
        new SetFunction(s"tag($o)", _1(o), Set(value), _ => value)

  lazy val initialT: Result[Obj] = BaseCategory.initial map constSet("initial", Sets.`∅`)
  
  lazy val _0: Obj = initialT iHope

  lazy val terminalT: Result[Obj] = BaseCategory.terminal map constSet("terminal", Sets.`{∅}`)

  lazy val _1: Obj = terminalT iHope

  private[topos] def constSet(name: String, value: set)(obj: BaseCategory.Obj): Obj =
    topos.const(name, value)
