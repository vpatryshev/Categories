package math.cat.topos.logic

import math.cat.Category
import math.cat.topos.{CategoryOfDiagrams, Fixtures}
import org.specs2.matcher.MatchResult
import testing.TestWatch

import scala.language.reflectiveCalls

class SpeedTest extends Fixtures:

  "Speed" should {

    "work for all known domains" in {

      def check(cat: Category): Unit =
        val topos = new CategoryOfDiagrams(cat)
        import topos._
        val desc = s"Speed for ${cat.name}${"."*(14 - cat.name.length)} "
        print(desc)
        val mid = Ω.points.size / 2
        TestWatch.restart()
        for
          i <- 0 to 10
          pt1 = Ω.points(mid)
        do
          val p: topos.Predicate = pt1.asPredicateIn(topos)
          val not_p = ¬(p)
          ¬(¬(not_p)) === not_p
        println(TestWatch.timePassedSec)
      categoriesToTest filter (_.isFinite) foreach check

      ok
    }
  }
