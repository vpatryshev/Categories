package math.cat.topos

import math.Base.concat
import math.cat.SetFunction.*
import math.cat.topos.CategoryOfDiagrams.{BaseCategory, DiagramArrow}
import math.cat.{Morphism, SetFunction}
import math.sets.Sets
import Sets.{set, setOf}
import scalakittens.{Params, Result}

import scala.collection.mutable
import scala.language.{implicitConversions, postfixOps}

trait GrothendieckToposLogic:
  topos: GrothendieckTopos =>

// TODO: use the right tagging, find the right predicate
//  private val cache = mutable.Map[(String, Predicate, Predicate), Predicate]()

  abstract class Predicate(myTag: Any, override val d0: Diagramme) extends DiagramArrow(myTag, d0, Ω):
    p: DiagramArrow =>

    def debugM(x: String) = {
      val o = topos.domain.asNode(x)
/*
        override def mappingAt(o: d0.d0.Obj): d1.d1.Arrow =
          val dom = setAt(o)
          if (Params.fullCheck)
            require(q.setAt(o) == dom)
          val po = p.transformAt(o)
          val qo = q.transformAt(o)

          val PQtoΩxΩ: SetFunction =
            new SetFunction(
              s"PQ->ΩxΩ($o)",
              dom, ΩxΩ.source(o),
              v => (po(v), qo(v))
            )
    
          val op: SetFunction = asFunction(ΩxΩ_to_Ω(o))
          val theMapping = PQtoΩxΩ andThen op

          theMapping.getOrElse(throw new IllegalStateException("Failed to compose"))

 */
      val res = mappingAt(o)
      res
    }

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
    
    def evalBinaryOp(q: Predicate, ΩxΩ_to_Ω: DiagramArrow, newTag: Any): Predicate =
      if (Params.fullCheck)
        requireCompatibility(q)

      new Predicate(newTag, p.d0):
        def mappingAt(o: d0.d0.Obj): d1.d1.Arrow =
          val dom = setAt(o)
          if (Params.fullCheck)
            require(q.setAt(o) == dom)
          val po = p.transformAt(o)
          val qo = q.transformAt(o)

          val PQtoΩxΩ: SetFunction =
            new SetFunction(
              s"PQ->ΩxΩ($o)",
              dom, ΩxΩ.source(o),
              v => (po(v), qo(v))
            )
    
          val op: SetFunction = asFunction(ΩxΩ_to_Ω(o))
          val theMapping = PQtoΩxΩ andThen op

          val result: SetFunction = theMapping.getOrElse(throw new IllegalStateException("Failed to compose"))
          result
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
    f.d0 match
      case d: topos.Diagramme =>
        new topos.Predicate(f.tag, d):
          override def mappingAt(x: d0.d0.Obj): d1.d1.Arrow = f.mappingAt(x)

      case d: Diagram =>
        new topos.Predicate(f.tag, d.source.asInstanceOf[topos.Diagramme]):
          override def mappingAt(x: d0.d0.Obj): d1.d1.Arrow = f(mappingAt(x))

      case basura => throw new IllegalArgumentException(s"WTF: basura $basura")


  /**
    * Builds a predicate for a point in Ω
    * @param pt the point
    * @return an arrow pt -> Ω
    */
  infix def predicateFor(pt: Point): Predicate =

    val inclusion: DiagramArrow = topos.standardInclusion(pt, Ω) iHope

    new Predicate(pt.tag, _1.source.asInstanceOf[GrothendieckToposLogic.this.Diagramme]):

      override def mappingAt(x: d0.d0.Obj): d1.d1.Arrow =
        inclusion.mappingAt(x)

  /**
    * An arrow from terminal to the point as a diagram
    * @return
    */
  def uniqueFromTerminalTo(p: Point): Arrow =
    new DiagramArrow(p.tag, _1, p.asDiagram):

      override def mappingAt(o: d0.d0.Obj): d1.d1.Arrow =
          val value = p(o)
          new SetFunction(s"tag($o)", _1.source(o), Set(value), _ => value)

  lazy val initialT: Result[Obj] = BaseCategory.initial map constSet("initial", Sets.`∅`)
  
  lazy val _0: Obj = initialT iHope

  lazy val terminalT: Result[Obj] = BaseCategory.terminal map constSet("terminal", Sets.`{∅}`)
  
  lazy val _1: Obj = terminalT iHope

  private[topos] def constSet(name: String, value: set)(obj: BaseCategory.Obj): Diagram =
    topos.const(name, value)
