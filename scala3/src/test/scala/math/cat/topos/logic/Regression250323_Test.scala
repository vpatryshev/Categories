package math.cat.topos.logic

import math.cat.topos.{CategoryOfDiagrams, Fixtures}
import math.cat.SetFunction
import math.cat.Categories.*
import org.specs2.execute.Result as MatchResult

import scala.language.reflectiveCalls
import scala.language.implicitConversions

class Regression250323_Test extends Fixtures:

  "Logic" should:
    val `Set^𝟙` = CategoryOfDiagrams(`𝟙`)
    val sut = `Set^𝟙`
    val Ω = `Set^𝟙`.Ω
    val ΩxΩ = `Set^𝟙`.ΩxΩ
    val Ω1 = `Set^𝟙`.Ω1
    val True: `Set^𝟙`.Predicate = `Set^𝟙`.Truth asPredicateIn `Set^𝟙`
    val False = `Set^𝟙`.Ω.False asPredicateIn `Set^𝟙`

    "work for adjunctions in set to 𝟙" in :
      import `Set^𝟙`.Predicates.*
      val False0 = False("0")
      val p = Ω.points.head asPredicateIn `Set^𝟙`
      val q = Ω.points.head asPredicateIn `Set^𝟙`
      val p_and_q = p ∧ q
      val pnqAt0 = p.binaryOp(Ω.conjunction)(q) // , "TEST CONJUNCTION OF FALSE AND FALSE")
      val r = `Set^𝟙`.Ω.points.head asPredicateIn `Set^𝟙`
      val q2r = q ⟹ r
      val true_0: True.d1.d1.Arrow = True("0")
      true_0.isInstanceOf[SetFunction] must beTrue
      val true_0_f = true_0.asInstanceOf[SetFunction]
      val true_0_at_empty = true_0_f.apply(Set.empty)
      val p_0 = p("0") //, "p, expected false")
      val q_0 = q("0")//, "q, expected false")
      val pq_0 = p_and_q("0").asInstanceOf[SetFunction]//, "pq, expected false")
      val pq_0_at_empty = pq_0(Set.empty)
      val pairOfFalses = (pq_0_at_empty, pq_0_at_empty)
      val pairFT = (pq_0_at_empty, true_0_at_empty)
      val implication = Ω.implication
      val implicationFun = implication("0").asInstanceOf[SetFunction]
      val mustBeTrueAt0 = implicationFun(pairFT) // we pass a (false,true) to a Map, and we get a (true)
      val p_q_true: `Set^𝟙`.Predicate = p_and_q.binaryOp(implication)(True) //, "TESTING")
      p_and_q must be_==(p)
      val omega0 = Ω("0")
      (omega0 contains pq_0_at_empty) must beTrue
      binaryOpMappingAt(implication, p, True, "0") must be_==(true_0)
      val v0: p_q_true.d1.d1.Arrow = binaryOpMappingAt(implication, p_and_q, True, "0")
      val PQtoΩxΩ: SetFunction = tuplingAt(p_and_q, True, "0")
      val pairAtEmpty = PQtoΩxΩ.mapping(Set())
      val omega2_0 = `Set^𝟙`.ΩxΩ("0")
      (omega2_0 contains pairAtEmpty) must beTrue
      val omega1_0 = Ω1("0")
      (omega1_0 contains pairAtEmpty) must beTrue
      val op: SetFunction = implicationFun
      val opAtPairAtEmpty = op.mapping(pairAtEmpty)
      opAtPairAtEmpty must be_==(true_0_at_empty)
      val theMapping = PQtoΩxΩ andThen op
      val result: SetFunction = theMapping.getOrElse(throw new IllegalStateException("Failed to compose"))
      val resultAtEmpty = result.mapping(Set())
      resultAtEmpty must be_==(true_0_at_empty)

      v0 must be_==(true_0)
      val valueAt0: p_q_true.d1.d1.Arrow = p_q_true("0")// v0 is true, but this one is false, wtf?
      valueAt0 must be_==(true_0)
      p_q_true must be_==(True)
      p_and_q ⟹ False must be_==(True)
      val left = p_and_q ⟹ r
      left must be_==(True)
      val r1 = False ⟹ q2r
      r1 must be_==(True)
      val right = p ⟹ q2r
      right must be_==(True)

      left must be_==(right)
