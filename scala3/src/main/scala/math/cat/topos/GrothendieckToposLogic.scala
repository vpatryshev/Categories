package math.cat.topos

import math.Base.concat
import math.cat.SetFunction.*
import math.cat.topos.CategoryOfDiagrams.{BaseCategory, DiagramArrow, const}
import math.cat.{Morphism, SetFunction}
import math.sets.Sets
import Sets.{set, setOf}
import scalakittens.Result

import scala.collection.mutable
import scala.language.{implicitConversions, postfixOps}

trait GrothendieckToposLogic:
  topos: GrothendieckTopos =>

// TODO: use the right tagging, find the right predicate
//  private val cache = mutable.Map[(String, Predicate, Predicate), Predicate]()

  abstract class Predicate(myTag: Any) extends DiagramArrow(myTag):
    p: DiagramArrow =>

    val d1: Diagram = Ω

    private def wrapTag(tag: Any): String =
      val ts = tag.toString
      if (ts.contains("∧") || ts.contains("∨") || ts.contains("=>"))
        s"($ts)" else ts

    private def tag2(tag1: Any, op: String, tag2: Any): String = 
      concat(wrapTag(tag1), op, wrapTag(tag2))

    private def setAt(o: Any): set =
      val function: SetFunction = p.mappingAt(o).asInstanceOf[SetFunction] // d1.d1.Arrow = SetFunction, and if we d
      setOf(function.d0)

    private def transformAt(o: Any): SetFunction =
      mappingAt(o).asInstanceOf[SetFunction]

    def binaryOp(ΩxΩ_to_Ω: DiagramArrow)(q: Predicate): Predicate =
      binaryOpNamed(q, ΩxΩ_to_Ω, ΩxΩ_to_Ω.tag)

    def binaryOpNamed(q: Predicate, ΩxΩ_to_Ω: DiagramArrow, name: Any): Predicate =
    // TODO: when identification is fixed (finding the right point), uncomment
    // cache.getOrElseUpdate((name, p, q), 
      evalBinaryOp(q, ΩxΩ_to_Ω, name)
    //)

    def binopMappingAt(ΩxΩ_to_Ω: DiagramArrow, p: Predicate, q: Predicate, o: d0.d0.Obj): d1.d1.Arrow =
      val dom = p.setAt(o)
      val d0 = dom.head
      require(q.setAt(o) == dom)

      val po: SetFunction = p.transformAt(o)
      val pom = po.mapping
      val qo: SetFunction = q.transformAt(o)
      val qom = qo.mapping
      val PQtoΩxΩ: SetFunction =
        new SetFunction(
          s"PQ->ΩxΩ($o)",
          dom, ΩxΩ(o),
          v => (po(v), qo(v))
        )
      val pairAtEmpty = PQtoΩxΩ.mapping(Set())
      val op: SetFunction = ΩxΩ_to_Ω(o).asInstanceOf[SetFunction]
      val opAtPairAtEmpty = op.mapping(pairAtEmpty)
      val theMapping = PQtoΩxΩ andThen op
      val result: SetFunction = theMapping.getOrElse(throw new IllegalStateException("Failed to compose"))
      val resultAtEmpty = result.mapping(Set())
      result

    def evalBinaryOp(q: Predicate, ΩxΩ_to_Ω: DiagramArrow, newTag: Any): Predicate =
      requireCompatibility(q)

      new Predicate(newTag):
        val d0 = p.d0
        def mappingAt(o: d0.d0.Obj): d1.d1.Arrow =
          binopMappingAt(ΩxΩ_to_Ω, p, q, o)

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
    lazy val ∧ = binaryOp(Ω.conjunction)

    /**
      * Disjunction with another predicate
      * @return  a function that takes another predicate and returns their disjunction
      */
    lazy val ∨ = binaryOp(Ω.disjunction)

    /**
      * implication of another predicate
      * @return  a function that takes another predicate `q` and returns `this implies q`
      */
    lazy val ⟹ = binaryOp(Ω.implication)

  def ¬(p: topos.Predicate): topos.Predicate =
    p.binaryOpNamed(FalsePredicate, Ω.implication, "¬")

  lazy val FalsePredicate: topos.Predicate = predicateFor(Ω.False)

  lazy val TruePredicate: topos.Predicate = predicateFor(Ω.True)

  /**
    * Builds a predicate for an arrow to Ω
    * @param f arrow from an object to Ω
    * @return an arrow X -> Ω
    */
  infix def predicateForArrowToΩ(f: DiagramArrow): topos.Predicate =
    new topos.Predicate(f.tag):
      override val d0: Obj = f.d0
      override def mappingAt(x: d0.d0.Obj): d1.d1.Arrow =
        f.mappingAt(x)

  /**
    * Builds a predicate for a point in Ω
    * @param pt the point
    * @return an arrow pt -> Ω
    */
  infix def predicateFor(pt: Point): Predicate =

    val inclusion: DiagramArrow = topos.standardInclusion(pt, Ω) iHope

    new Predicate(pt.tag):
      override val d0: Obj = _1

      override def mappingAt(x: d0.d0.Obj): d1.d1.Arrow =
        inclusion.mappingAt(x)

  /**
    * An arrow from terminal to the point as a diagram
    * @return
    */
  def uniqueFromTerminalTo(p: Point): Arrow =
    new DiagramArrow(p.tag):

      override val d0: Diagram = _1
      override val d1: Diagram = p.asDiagram

      override def mappingAt(o: d0.d0.Obj): d1.d1.Arrow =
          val value = p(o)
          new SetFunction(s"tag($o)", _1(o), Set(value), _ => value)

  val initialT: Result[Obj] = BaseCategory.initial map constSet("initial", Sets.Empty)
  
  lazy val _0: Obj = initialT iHope

  val terminalT: Result[Obj] = BaseCategory.terminal map constSet("terminal", Sets.Unit)
  
  val _1: Obj = terminalT iHope

  private[topos] def constSet(name: String, value: set)(obj: BaseCategory.Obj): Diagram =
    const(name, topos)(value)
