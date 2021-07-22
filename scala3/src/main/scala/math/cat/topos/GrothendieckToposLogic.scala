package math.cat.topos

import math.Base.concat
import math.cat.SetFunction._
import math.cat.topos.CategoryOfDiagrams.{BaseCategory, DiagramArrow, const}
import math.cat.{Morphism, SetFunction}
import math.sets.Sets.{set, setOf}
import scalakittens.Result

import scala.collection.mutable
import scala.language.postfixOps

trait GrothendieckToposLogic:
  topos: GrothendieckTopos =>

// TODO: use the right tagging, find the right predicate
//  private[this] val cache = mutable.Map[(String, Predicate, Predicate), Predicate]()

  abstract class Predicate(myTag: Any) extends DiagramArrow(myTag):
    p: DiagramArrow =>
  
    val d1: Diagram = Ω

    private def wrapTag(tag: Any): String =
      val ts = tag.toString
      if (ts contains "∧") || (ts contains "∨") || (ts contains "=>")
      then s"($ts)" else ts

    private def tag2(tag1: Any, op: String, tag2: Any): String = 
      concat(wrapTag(tag1), op, wrapTag(tag2))

    private def setAt(o: Any): set =
      val o1 = d0.d0.obj(o)
      val function = asFunction(p.transformPerObject(o1))
      setOf(function.d0)

    private def transformAt(o: Any): SetFunction =
      asFunction(transformPerObject(d0.d0.obj(o)))

    private[topos] def binaryOp(ΩxΩ_to_Ω: DiagramArrow)(q: Predicate): Predicate =
      binaryOpNamed(q, ΩxΩ_to_Ω, ΩxΩ_to_Ω.tag)

    private[topos] def binaryOpNamed(q: Predicate, ΩxΩ_to_Ω: DiagramArrow, name: Any): Predicate =
    // TODO: when identification is fixed (finding the right point), uncomment
    // cache.getOrElseUpdate((name, p, q), 
      evalBinaryOp(q, ΩxΩ_to_Ω, name)
    //)
    
    private def evalBinaryOp(q: Predicate, ΩxΩ_to_Ω: DiagramArrow, newTag: Any): Predicate =
      requireCompatibility(q)

      new Predicate(newTag):
        val d0 = p.d0

        override def transformPerObject(o: d0.d0.Obj): d1.d1.Arrow =
          val dom = setAt(o)
          require(q.setAt(o) == dom)
          val po = p.transformAt(o)
          val qo = q.transformAt(o)

          val PQtoΩxΩ: SetFunction =
            new SetFunction(
              s"PQ->ΩxΩ($o)",
              dom, ΩxΩ(o),
              v => (po(v), qo(v))
            )
    
          val op: SetFunction = asFunction(ΩxΩ_to_Ω(o))
          val maybeFunction = PQtoΩxΩ andThen op
          d1.d1.arrow(Result(maybeFunction).iHope)

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
  def predicateForArrowToΩ(f: DiagramArrow): topos.Predicate =
    new topos.Predicate(f.tag):
      override val d0: Obj = f.d0.asInstanceOf[Obj] // TODO: get rid of casting
      override def transformPerObject(x: d0.d0.Obj): d1.d1.Arrow =
        val x_in_domain_of_f = f.d0.d0.obj(x)
        val arrow_in_domain_of_f = f.transformPerObject(x_in_domain_of_f)
        val arrow_in_codomain_of_f = d1.d1.arrow(arrow_in_domain_of_f)
        arrow_in_codomain_of_f

  /**
    * Builds a predicate for a point in Ω
    * @param pt the point
    * @return an arrow pt -> Ω
    */
  def predicateFor(pt: Point): Predicate =

    val inclusion: DiagramArrow = topos.standardInclusion(pt, Ω) iHope

    new Predicate(pt.tag):
      override val d0: Obj = _1

      override def transformPerObject(x: d0.d0.Obj): d1.d1.Arrow =
        val xInInclusion = inclusion.d0.d0.obj(x)
        val arrowInInclusion = inclusion.transformPerObject(xInInclusion)
        d1.d1.arrow(arrowInInclusion)

  /**
    * An arrow from terminal to the point as a diagram
    * @return
    */
  def uniqueFromTerminalTo(p: Point): Arrow =
    new DiagramArrow(p.tag):

      override val d0: Diagram = _1
      override val d1: Diagram = p.asDiagram

      override def transformPerObject(o: d0.d0.Obj): d1.d1.Arrow =
        d1.d1.arrow {
          val value = p(o)
          new SetFunction(s"tag($o)", _1(o), Set(value), _ => value)
        }

  val initialT: Result[Obj] = BaseCategory.initial map constSet("initial")
  
  lazy val _0: Obj = initialT iHope

  val terminalT: Result[Obj] = BaseCategory.terminal map constSet("terminal")
  
  val _1: Obj = terminalT iHope

  private[topos] def constSet(name: String)(obj: BaseCategory.Obj): Diagram =
    const(name, topos)(obj.asInstanceOf[set])
