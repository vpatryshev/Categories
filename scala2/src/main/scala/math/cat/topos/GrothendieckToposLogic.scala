package math.cat.topos

import math.cat.SetFunction
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.sets.Sets.{set, setOf}
import scalakittens.Result

import scala.collection.mutable
import scala.language.postfixOps

trait GrothendieckToposLogic { topos: CategoryOfDiagrams =>

// TODO: use the right tagging, find the right predicate
//  private[this] val cache = mutable.Map[(String, Predicate, Predicate), Predicate]()

  trait Predicate extends DiagramArrow { p: DiagramArrow =>
    val d1: Diagram = Ω

    private def wrapTag(tag: Any): String = {
      val ts = tag.toString
      if (ts.contains("∧") || ts.contains("∨") || ts.contains("=>")) s"($ts)" else ts
    }

    private def tag2(tag1: Any, op: String, tag2: Any): String = s"${wrapTag(tag1)} $op ${wrapTag(tag2)}"

    private def setAt(o: Any): set = {
      val o1 = domainCategory.obj(o)
      val function = p.transformPerObject(o1).asInstanceOf[SetFunction]
      setOf(function.d0)
    }

    private def transformAt(o: Any): SetFunction =
      transformPerObject(domainCategory.obj(o)).asInstanceOf[SetFunction]

    private def binaryOp(q: Predicate, opName: String, ΩxΩ_to_Ω: DiagramArrow): Predicate = {
      binaryOpNamed(q, ΩxΩ_to_Ω, tag2(p.tag, opName, q.tag))
    }

    private[GrothendieckToposLogic] def binaryOpNamed(q: Predicate, ΩxΩ_to_Ω: DiagramArrow, name: String): Predicate =
    // TODO: when identification is fixed (finding the right point), uncomment
    // cache.getOrElseUpdate((name, p, q), 
        evalBinaryOp(q, ΩxΩ_to_Ω, name)
    //)
    
    private[GrothendieckToposLogic] def evalBinaryOp(q: Predicate, ΩxΩ_to_Ω: DiagramArrow, newTag: String): Predicate = {
      requireCompatibility(q)

      new Predicate {
        val d0: Obj = p.d0
        val tag: Any = newTag

        override def transformPerObject(o: domainCategory.Obj): codomainCategory.Arrow = {
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

          val op: SetFunction = ΩxΩ_to_Ω(o).asInstanceOf[SetFunction]
          val maybeFunction = PQtoΩxΩ andThen op
          codomainCategory.arrow(Result(maybeFunction).iHope)
        }
      }
    }

    private def requireCompatibility(q: Predicate): Unit = {
      require(q.d0 == p.d0)

      for (o <- domainCategory.objects) {
        require(p.setAt(o) == q.setAt(o), s"Different domains at $o for ${p.tag} and ${q.tag}")
      }
    }

    /**
      * Conjunction with another predicate
      *
      * @param q another predicate
      * @return their conjunction
      */
    def ∧(q: topos.Predicate): topos.Predicate = binaryOp(q, "∧", Ω.conjunction)

    /**
      * Disjunction with another predicate
      * @param q another predicate
      * @return their disjunction
      */
    def ∨(q: topos.Predicate): topos.Predicate = binaryOp(q, "∨", Ω.disjunction)

    /**
      * implication of another predicate
      * @param q another predicate
      * @return this implies q
      */
    def ⟹(q: topos.Predicate): topos.Predicate = binaryOp(q, "⟹", Ω.implication)
  }


  def ¬(p: topos.Predicate): topos.Predicate =
    p.binaryOpNamed(FalsePredicate, Ω.implication, "¬")


  lazy val FalsePredicate: topos.Predicate = predicateFor(Ω.False)

  lazy val TruePredicate: topos.Predicate = predicateFor(Ω.True)

  /**
    * Builds a predicate for an arrow to Ω
    * @param f arrow from an object to Ω
    * @return an arrow X -> Ω
    */
  def predicateForArrowToΩ(f: DiagramArrow): topos.Predicate = {
    new topos.Predicate {
      override val d0: Obj = f.d0
      override val tag: Any = f.tag
      override def transformPerObject(x: domainCategory.Obj): codomainCategory.Arrow = {
        val x_in_domain_of_f = f.domainCategory.obj(x)
        val arrow_in_domain_of_f = f.transformPerObject(x_in_domain_of_f)
        val arrow_in_codomain_of_f = codomainCategory.arrow(arrow_in_domain_of_f)
        arrow_in_codomain_of_f
      }
    }
  }

  /**
    * Builds a predicate for a point in Ω
    * @param pt the point
    * @return an arrow pt -> Ω
    */
  def predicateFor(pt: Point): Predicate = {

    val inclusion: DiagramArrow = standardInclusion(pt, Ω) iHope

    new Predicate {
      override val d0: Obj = _1
      override val tag: Any = pt.tag

      override def transformPerObject(x: domainCategory.Obj): codomainCategory.Arrow = {
        val xInInclusion = inclusion.domainCategory.obj(x)
        val arrowInInclusion = inclusion.transformPerObject(xInInclusion)
        codomainCategory.arrow(arrowInInclusion)
      }
    }
  }
}
