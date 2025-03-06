package math.cat.topos.logic

import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.cat.topos.{CategoryOfDiagrams, Fixtures, GrothendieckTopos, Point}
import math.cat.{Category, SetFunction}
import SetFunction.fun
import org.specs2.matcher.MatchResult
import org.specs2.matcher.ShouldMatchers.thisValue
import scalakittens.Result._
import SetFunction._

import scala.language.postfixOps
import scala.reflect.Selectable.reflectiveSelectable

class ConjunctionTest extends Fixtures:

  "Conjunction" should {

    def checkProperties(topos: GrothendieckTopos, number: Int, total: Int, what: String): MatchResult[Any] =
      import topos._
      val desc = s"Testing $what over ${domain.name} ($number/$total)"
      val rep = report(_)
      val True = Ω.True.asPredicateIn(topos)
      val False = Ω.False.asPredicateIn(topos)

      for pt <- Ω.points do
        rep(s"conjunction with False for ${pt.tag}")
        val p: Predicate = pt.asPredicateIn(topos)
        True.getClass === p.getClass
        False.getClass === p.getClass
// fails        False.getClass === (False ∧ p).getClass
        (False ∧ p) === False
      
      checkThatIn(topos, number, total).mustBeMonoid[Predicate](
        "conjunction",
        True,
        (p: Predicate, q: Predicate) => p ∧ q
      )
      ok

    end checkProperties

    def checkTrue(topos: GrothendieckTopos, number: Int, total: Int): MatchResult[Any] =
      import topos._
      val desc = s"Testing True value over ${domain.name} ($number/$total)"

      def diagonalMap_Ω(x: topos.domain.Obj): SetFunction =
        fun(Ω(x), ΩxΩ(x))(s"Δ[$x]", subrep => (subrep, subrep))

      val conjunction = Ω.conjunction

      val True = Ω.True
      
      // TODO(vlad): stop using this `transform`, it makes no sense.
      // We just need an composition of point with Δ_Ω
      val pointOfTrueAndTrue = True.transform(Δ_Ω)

      val monomorphismMaybe = inclusionOf(pointOfTrueAndTrue) in ΩxΩ
      val monomorphism: DiagramArrow = monomorphismMaybe iHope

      for
        o <- domain.objects
      do
        val p = pointOfTrueAndTrue(o)
        p aka s"$desc, @$o" must_== (Ω.True(o), Ω.True(o))
        val monoAt_o = monomorphism(o)
        val actual = asFunction(monoAt_o)(p)
        actual aka s"$desc, @$o" must_== p

      val classifyingArrow: DiagramArrow = χ(monomorphism)
      val theyAreTheSame = classifyingArrow == conjunction // nice to have this line, to check the comparison

      // the following part is for finding exactly where comparison failed
      if !theyAreTheSame then
        for o <- domain.objects do
          val con_o = asFunction(classifyingArrow.mappingAt(o)).toList.sortBy(_._1.toString)
          val tru_classif_o =
            asFunction(conjunction.mappingAt(o)).toList.sortBy(_._1.toString)

          val pairs = con_o zip tru_classif_o

          pairs foreach {
            case ((k1, v1), (k2, v2)) =>
              k1 === k2
              v1 aka s"At $k1 at $o" must_== v2
          }

          tru_classif_o === con_o

      classifyingArrow aka desc must_== conjunction
    end checkTrue

    val testCase = new TestCase:
      def check(category: Category, number: Int, total: Int): MatchResult[Any] =
        val topos = new CategoryOfDiagrams(category)
        checkTrue(topos, number, total)
        checkProperties(topos, number, total, "conjunction")

    
    "work for all known domains" in:
      test(testCase)
  }
