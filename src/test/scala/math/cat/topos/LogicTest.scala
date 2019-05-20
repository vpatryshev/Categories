package math.cat.topos

import math.cat.Category._
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.cat.{Category, SetFunction}
import math.sets.Sets
import math.sets.Sets.set
import org.specs2.matcher.MatchResult
import scalakittens.Result._

class LogicTest extends Fixtures {

  "True and False" should {
    "exist for _0_" in {
      val topos = new CategoryOfDiagrams(_0_)
      import topos._
      val tTrue = Ω.True.mapping
      val tFalse = Ω.False.mapping
      tTrue === tFalse // that's a degenerate topos, but tags are still distinct
    }

    def checkAt(point0: Any)(mappings: (String, set)*): MatchResult[Any] = {
      point0 match {
        case d: Diagram ⇒
          (traverse {
            for {(k, v) ← mappings} yield OKif(d(k) == v, s"Failed on $k, expected $v, got ${d(k)}")
          } andThen OK) === OK
        case trash ⇒ failure(s"Expected a diagram, got $trash")
      }
      ok
    }


    "exist for _1_" in {
      val topos = new CategoryOfDiagrams(_1_)
      val omega = topos.Ω
      checkAt(omega.False("0"))("0" → Sets.Empty)
      checkAt(omega.True("0"))("0" → Set("0.0"))
    }

    "exist for _2_" in {
      val topos = new CategoryOfDiagrams(_2_)
      val omega = topos.Ω
      checkAt(omega.False("0"))("0" → Sets.Empty, "1" → Sets.Empty)
      checkAt(omega.False("1"))("0" → Sets.Empty, "1" → Sets.Empty)

      checkAt(omega.True("0"))("0" → Set("0.0"), "1" → Set("0.1"))
      checkAt(omega.True("1"))("0" → Sets.Empty, "1" → Set("1.1"))
    }

    "exist for _3_" in {
      val topos = new CategoryOfDiagrams(_3_)
      val omega = topos.Ω
      checkAt(omega.False("0"))("0" → Sets.Empty, "1" → Sets.Empty, "2" → Sets.Empty)
      checkAt(omega.False("1"))("1" → Sets.Empty, "2" → Sets.Empty)
      checkAt(omega.False("2"))("2" → Sets.Empty)

      checkAt(omega.True("0"))("0" → Set("0.0"), "1" → Set("0.1"), "2" → Set("0.2"))
      checkAt(omega.True("1"))("0" → Sets.Empty, "1" → Set("1.1"), "2" → Set("1.2"))
      checkAt(omega.True("2"))("0" → Sets.Empty, "1" → Sets.Empty, "2" → Set("2.2"))
    }

    "exist for ParallelPair" in {
      val topos = new CategoryOfDiagrams(ParallelPair)
      val omega = topos.Ω
      checkAt(omega.False("0"))("0" → Sets.Empty, "1" → Sets.Empty)
      checkAt(omega.False("1"))("0" → Sets.Empty, "1" → Sets.Empty)

      checkAt(omega.True("0"))("0" → Set("0"), "1" → Set("a", "b"))
      checkAt(omega.True("1"))("1" → Set("1"))
    }

    "exist for Pullback" in {
      val topos = new CategoryOfDiagrams(Pullback)
      val omega = topos.Ω
      val False = omega.False
      checkAt(False("a"))("a" → Sets.Empty, "b" → Sets.Empty, "c" → Sets.Empty)
      checkAt(False("b"))("a" → Sets.Empty, "b" → Sets.Empty, "c" → Sets.Empty)
      checkAt(False("c"))("a" → Sets.Empty, "b" → Sets.Empty, "c" → Sets.Empty)

      val True = omega.True
      checkAt(True("a"))("a" → Set("a"), "b" → Sets.Empty, "c" → Set("ac"))
      checkAt(True("b"))("a" → Sets.Empty, "b" → Set("b"), "c" → Set("bc"))
      checkAt(True("c"))("a" → Sets.Empty, "b" → Sets.Empty, "c" → Set("c"))
    }

    "exist for Pushout" in {
      val topos = new CategoryOfDiagrams(Pushout)
      val omega = topos.Ω
      val False = omega.False
      checkAt(False("a"))("a" → Sets.Empty, "b" → Sets.Empty, "c" → Sets.Empty)
      checkAt(False("b"))("a" → Sets.Empty, "b" → Sets.Empty, "c" → Sets.Empty)
      checkAt(False("c"))("a" → Sets.Empty, "b" → Sets.Empty, "c" → Sets.Empty)

      val True = omega.True
      checkAt(True("a"))("a" → Set("a"), "b" → Set("ab"), "c" → Set("ac"))
      checkAt(True("b"))("a" → Sets.Empty, "b" → Set("b"), "c" → Sets.Empty)
      checkAt(True("c"))("a" → Sets.Empty, "b" → Sets.Empty, "c" → Set("c"))
    }

    "exist for Z3" in {
      val topos = new CategoryOfDiagrams(Z3)
      val omega = topos.Ω
      checkAt(omega.False("0"))("0" → Sets.Empty)
      checkAt(omega.True("0"))("0" → Set("0", "1", "2"))
    }
  }

  "Conjunction" should {

    def checkProperties(topos: GrothendieckTopos, what: String): MatchResult[Any] = {
      import topos._
      val desc = s"Testing ${domain.name} conjunction"
      val True = predicateFor(Ω.True)
      val False = predicateFor(Ω.False)
      checkThatIn(topos).mustBeMonoid[Predicate](
        "conjunction",
        True,
        (p: Predicate, q: Predicate) ⇒ p ∧ q
      )

      for { p ← Ω.points} {
        println(s"  checking conjunction with False for ${p.tag}")
        val pp = predicateFor(p)
        (False ∧ pp) === False
      }
      ok
    }

    def checkTrue(topos: GrothendieckTopos): MatchResult[Any] = {
      import topos._
      val desc = s"Testing ${domain.name} True value"

      def diagonalMap_Ω(x: topos.domain.Obj): SetFunction = {
        SetFunction.build(s"Δ[$x]", Ω(x), ΩxΩ(x), (subrep: Any) ⇒ (subrep, subrep)).iHope
      }

      val conjunction = Ω.conjunction

      val True = Ω.True
      val pointOfTrueAndTrue = True.transform(Δ_Ω)

      val monomorphismMaybe = inclusionOf(pointOfTrueAndTrue) in ΩxΩ
      val monomorphism: DiagramArrow = monomorphismMaybe iHope

      for {
        o ← domain.objects
      } {
        val p = pointOfTrueAndTrue(o)
        p aka s"$desc, @$o" must_==(Ω.True(o), Ω.True(o))
        val monoAt_o = monomorphism(o)
        val actual = monoAt_o.asInstanceOf[SetFunction](p)
        actual aka s"$desc, @$o" must_== p
      }

      val classifierForTT: DiagramArrow = classifyingMap(monomorphism)
      val theyAreTheSame = classifierForTT equals conjunction // nice to have this line, to check the comparison

      if (!theyAreTheSame) {
        for {
          o0 ← domain.objects
        } {
          val o = classifierForTT.domainCategory.obj(o0)
          val con_o = classifierForTT.transformPerObject(o).asInstanceOf[SetFunction].toList.sortBy(_._1.toString)
          val tru_classif_o =
            conjunction.transformPerObject(o.asInstanceOf[conjunction.domainCategory.Obj]).asInstanceOf[SetFunction].toList.sortBy(_._1.toString)

          val pairs = con_o zip tru_classif_o

          pairs foreach {
            case ((k1, v1), (k2, v2)) ⇒
              k1 === k2
              v1 aka s"At $k1 at $o" must_== v2
          }

          tru_classif_o === con_o
        }
      }

      classifierForTT aka desc must_== conjunction
    }

    def check(category: Category): MatchResult[Any] = {
      val topos = new CategoryOfDiagrams(category)
      checkTrue(topos)
      checkProperties(topos, "conjunction")
    }

    "exist for all known domains" in {
      KnownCategories filter (_.isFinite) foreach check
      ok
    }
  }

  "Disjunction" should {

    def check(cat: Category): MatchResult[Any] = {
      val topos = new CategoryOfDiagrams(cat)
      import topos._
      val desc = s"Testing ${cat.name} disjunction"
      println(desc)
      val True = predicateFor(Ω.True)
      val False = predicateFor(Ω.False)
      checkThatIn(topos).mustBeMonoid[Predicate](
        "disjunction",
        False,
        (p: Predicate, q: Predicate) ⇒ p ∨ q
      )

      for { p ← Ω.points} {
        println(s"  checking disjunction with False for ${p.tag}")
        val pp = predicateFor(p)
        (True ∨ pp) === True
      }
      ok
    }

    "exist for all known domains" in {
      
      def check(cat: Category): MatchResult[Any] = {
        val topos = new CategoryOfDiagrams(cat)
        import topos._
        val desc = s"Testing ${cat.name} disjunction"
        println(desc)
        val True = predicateFor(Ω.True)
        val False = predicateFor(Ω.False)
        checkThatIn(topos).mustBeMonoid[Predicate](
          "disjunction",
          False,
          (p: Predicate, q: Predicate) ⇒ p ∨ q
        )

        for { p ← Ω.points} {
          println(s"  checking disjunction with False for ${p.tag}")
          val pp = predicateFor(p)
          (True ∨ pp) === True
        }
        ok
      }

      check(_3_)

      KnownCategories filter (_.isFinite) foreach check

      ok
    }
  }
  
  "Distributivity laws of logic" in {

    // distributivity of conjunction over disjunction
    def conjunctionOverDisjunction(topos: GrothendieckTopos)(
      p: topos.Predicate, q: topos.Predicate, pAndQ: topos.Predicate, r: topos.Predicate)= {
      val p_qr = p ∧ (q ∨ r)
      val pq_pr = pAndQ ∨ (p ∧ r)
      p_qr === pq_pr
    }

    // distributivity of disjunction over conjunction
    def disjunctionOverConjunction(topos: GrothendieckTopos)(
      p: topos.Predicate,
      q: topos.Predicate, pOrQ: topos.Predicate, r: topos.Predicate)= {
      val p_qr = p ∨ (q ∧ r)
      val pq_pr = pOrQ ∧ (p ∨ r)
      p_qr === pq_pr
    }

    def checkDistributivity(cat: Category): MatchResult[Any] = {
      val topos = new CategoryOfDiagrams(cat)
      import topos._
      val points = Ω.points

      val desc = s"Testing ${cat.name} distributivity laws"
      println(desc)

      for { pt1 ← points } {
        println(s"  distributivity at ${pt1.tag}")
        val p = predicateFor(pt1)

        for { pt2 ← points } {
          val q = predicateFor(pt2)
          val pAndQ = p ∧ q
          val pOrQ = p ∨ q

          for { pt3 ← points } {
            val r: Predicate = predicateFor(pt3)
            conjunctionOverDisjunction(topos)(p, q, pAndQ, r)
            disjunctionOverConjunction(topos)(p, q, pOrQ, r)
          }
        }
      }

      ok
    }

    "hold for all known domains" in {

      checkDistributivity(_3_)

      KnownCategories filter (_.isFinite) foreach checkDistributivity

      ok
    }
  }
}
