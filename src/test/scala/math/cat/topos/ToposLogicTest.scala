package math.cat.topos

import scala.language.reflectiveCalls
import math.cat.Categories._
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.cat.{Category, SetFunction}
import math.sets.Sets
import math.sets.Sets.set
import org.specs2.matcher.MatchResult
import scalakittens.Result._

class ToposLogicTest extends Fixtures {
  
  val categoriesToTest: List[Cat] = SomeKnownCategories
  
  val enabled: Set[String] = Set("negation")
  def ignored(name: String) = false // !enabled(name)

  "True and False" should {
    "exist for _0_" in {
      val topos = new CategoryOfDiagrams(_0_)
      import topos._
      Ω.True.toString === "⊤"
      val tTrue = Ω.True.mapping
      val tFalse = Ω.False.mapping
      tTrue === tFalse // that's a degenerate topos, but tags are still distinct
    }

    def checkAt(point: Any)(mappings: (String, set)*): MatchResult[Any] = {
      point match {
        case d: Diagram ⇒
          (traverse {
            for {(k, v) ← mappings } yield OKif(d(k) == v, s"Failed on $k, expected $v, got ${d(k)}")
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
      val desc = s"Testing $what over ${domain.name}"
      val True = Ω.True.asPredicate
      val False = Ω.False.asPredicate
      checkThatIn(topos).mustBeMonoid[Predicate](
        "conjunction",
        True,
        (p: Predicate, q: Predicate) ⇒ p ∧ q
      )

      for { pt ← Ω.points } {
        println(s"  checking conjunction with False for ${pt.tag}")
        val p = pt.asPredicate
        (False ∧ p) === False
      }
      ok
    }

    def checkTrue(topos: GrothendieckTopos): MatchResult[Any] = {
      import topos._
      val desc = s"Testing True value over ${domain.name}"

      def diagonalMap_Ω(x: topos.domain.Obj): SetFunction = {
        SetFunction.build(s"Δ[$x]", Ω(x), ΩxΩ(x), (subrep: Any) ⇒ (subrep, subrep)).iHope
      }

      val conjunction = Ω.conjunction

      val True = Ω.True
      
      // TODO(vlad): stop using this `transform`, it makes no sense.
      // We just need an composition of point with Δ_Ω
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

      val classifierForTT: DiagramArrow = χ(monomorphism)
      val theyAreTheSame = classifierForTT == conjunction // nice to have this line, to check the comparison

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

    "work for all known domains" in {
      categoriesToTest filter (_.isFinite) foreach check
      ok
    }
  }

  "Disjunction" should {

    "work for all known domains" in {

      def check(cat: Category): MatchResult[Any] = {
        val topos = new CategoryOfDiagrams(cat)
        import topos._
        val desc = s"Testing disjunction over ${cat.name}"
        println(desc)
        val True = Ω.True.asPredicate
        val False = Ω.False.asPredicate
        checkThatIn(topos).mustBeMonoid[Predicate](
          "disjunction",
          False,
          (p: Predicate, q: Predicate) ⇒ p ∨ q
        )

        for { pt ← Ω.points } {
          println(s"  checking disjunction with False for ${pt.tag}")
          val p = pt.asPredicate
          (True ∨ p) === True
        }
        ok
      }

      categoriesToTest filter (_.isFinite) foreach check

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

      val desc = s"Testing distributivity laws over ${cat.name}"
      println(desc)

      for { pt1 ← points } {
        println(s"  distributivity at ${pt1.tag}")
        val p = pt1.asPredicate

        for { pt2 ← points } {
          val q = pt2.asPredicate
          val pAndQ = p ∧ q
          val pOrQ = p ∨ q

          for { pt3 ← points } {
            val r: Predicate = pt3.asPredicate
            conjunctionOverDisjunction(topos)(p, q, pAndQ, r)
            disjunctionOverConjunction(topos)(p, q, pOrQ, r)
          }
        }
      }

      ok
    }

    "hold for all known domains" in {
      categoriesToTest filter (_.isFinite) foreach checkDistributivity

      ok
    }
  }

  "Implication" should {

    "work for all known domains" in {

      def check(cat: Category): MatchResult[Any] = {
        val topos = new CategoryOfDiagrams(cat)
        import topos._
        val desc = s"Testing implication over ${cat.name}"
        println(desc)
        val True = Ω.True.asPredicate
        val False = Ω.False.asPredicate

        for { pt1 ← Ω.points } {
          println(s"  checking Truth ⟹ ${pt1.tag}")
          val p = pt1.asPredicate
          (True ⟹ p) === p
          println(s"  checking False ⟹ ${pt1.tag}")
          (False ⟹ p) === True
          println(s"  checking ${pt1.tag} ⟹ ${pt1.tag}")
          (p ⟹ p) === True
          println(s"  checking ${pt1.tag} ⟹ True")
          (p ⟹ True) === True

          println(s"  checking adjunction for ${pt1.tag}")
          for { pt2 ← Ω.points } {
            val q = pt2.asPredicate
            val p_q = p ∧ q

            for { pt3 ← Ω.points } {
              val r = pt3.asPredicate
              val q2r = q ⟹ r
              val left = p_q ⟹ r
              val right = p ⟹ q2r
              left === right
            }
          }

          println(s"  checking adjunction for ${pt1.tag}")
          for { pt2 ← Ω.points } {
            val q = pt2.asPredicate
            val p_q = p ∧ q

            for { pt3 ← Ω.points } {
              val r = pt3.asPredicate
              val q2r = q ⟹ r
              val left = p_q ⟹ r
              val right = p ⟹ q2r
              left === right
            }
          }

          println(s"  checking conjunction distributivity for ${pt1.tag}")
          for { pt2 ← Ω.points } {
            val q = pt2.asPredicate
            val p_and_q = p ∧ q

            for { pt3 ← Ω.points } {
              val r = pt3.asPredicate
              val r2p = r ⟹ p
              val r2q = r ⟹ q
              val left = r2p ∧ r2q
              val right = r ⟹ p_and_q
              left === right
            }
          }

          println(s"  checking disjunction distributivity for ${pt1.tag}")
          for { pt2 ← Ω.points } {
            val q = pt2.asPredicate
            val p_or_q = p ∨ q

            for { pt3 ← Ω.points } {
              val r = pt3.asPredicate
              val p2r = p ⟹ r
              val q2r = q ⟹ r
              val left = p2r ∧ q2r
              val right = p_or_q ⟹ r
              left === right
            }
          }
        }
        ok
      }

      categoriesToTest filter (_.isFinite) foreach check

      ok
    }
  }


  "Negation" should {

    "work for all known domains" in {
      val d = describe("fuck")

      def check(cat: Category): MatchResult[Any] = {
        val topos = new CategoryOfDiagrams(cat)
        import topos._
        
        val desc = s"Testing negation over ${cat.name}"
        println(desc)
        val True = Ω.True.asPredicate
        val False = Ω.False.asPredicate

        ¬(True) === False
        ¬(False) === True

        for { pt1 ← Ω.points } {
          println(s"  checking triple negation for ${pt1.tag}")
          val p = pt1.asPredicate
          val not_p = ¬(p)
          ¬(¬(not_p)) === not_p

          println(s"  checking negation of disjunction for ${pt1.tag}")
          for {pt2 ← Ω.points} {
            val q = pt2.asPredicate
            ¬(p ∨ q) === ¬(p) ∧ ¬(q)
          }
        }

        ok
      }

      categoriesToTest filter (_.isFinite) foreach check

      ok
    }
  }
}
