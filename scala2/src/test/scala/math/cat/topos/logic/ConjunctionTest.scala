package math.cat.topos.logic

import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.cat.topos.{CategoryOfDiagrams, Fixtures, GrothendieckTopos}
import math.cat.{Category, SetFunction}
import org.specs2.matcher.MatchResult
import org.specs2.matcher.ShouldMatchers.thisValue
import scalakittens.Result._

import scala.language.reflectiveCalls

class ConjunctionTest extends Fixtures {

  "Conjunction" should {

    def checkProperties(topos: GrothendieckTopos, what: String): MatchResult[Any] = {
      import topos._
      val desc = s"Testing $what over ${domain.name}"
      val rep = report(domain)(_)
      val True = Ω.True.asPredicate
      val False = Ω.False.asPredicate
      checkThatIn(topos).mustBeMonoid[Predicate](
        "conjunction",
        True,
        (p: Predicate, q: Predicate) => p ∧ q
      )

      for { pt <- Ω.points } {
        rep(s"conjunction with False for ${pt.tag}")
        val p = pt.asPredicate
        (False ∧ p) === False
      }
      ok
    }

    def checkTrue(topos: GrothendieckTopos): MatchResult[Any] = {
      import topos._
      val desc = s"Testing True value over ${domain.name}"

      def diagonalMap_Ω(x: topos.domain.Obj): SetFunction = {
        SetFunction.build(s"Δ[$x]", Ω(x), ΩxΩ(x), (subrep: Any) => (subrep, subrep)).iHope
      }

      val conjunction = Ω.conjunction

      val True = Ω.True
      
      // TODO(vlad): stop using this `transform`, it makes no sense.
      // We just need an composition of point with Δ_Ω
      val pointOfTrueAndTrue = True.transform(Δ_Ω)

      val monomorphismMaybe = inclusionOf(pointOfTrueAndTrue) in ΩxΩ
      val monomorphism: DiagramArrow = monomorphismMaybe iHope

      for {
        o <- domain.objects
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
          o0 <- domain.objects
        } {
          val o = classifierForTT.domainCategory.obj(o0)
          val con_o = classifierForTT.transformPerObject(o).asInstanceOf[SetFunction].toList.sortBy(_._1.toString)
          val tru_classif_o =
            conjunction.transformPerObject(o.asInstanceOf[conjunction.domainCategory.Obj]).asInstanceOf[SetFunction].toList.sortBy(_._1.toString)

          val pairs = con_o zip tru_classif_o

          pairs foreach {
            case ((k1, v1), (k2, v2)) =>
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
}
