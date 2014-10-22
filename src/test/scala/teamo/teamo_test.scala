package teamo

import org.scalatest._
import org.scalatest.prop._
import scala.concurrent.duration._

class SomeSpec extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  def simulate(t: TeamNature, d: FiniteDuration) = {
   Simulation.run(t,d)
  }

  def moreSlack(team:TeamNature):TeamNature = team + Slack(0.05)

  test("after a while, the team with more slack is more productive") {
    forAll (TeamNatureGen(), CultureGen())
      { (team : TeamNature, cultureWithMoreSlack: Culture) =>
        whenever(cultureWithMoreSlack.slack > team.culture.slack) {
      val aWhile = 20.days
      val results = simulate(team, aWhile)
      val teamWithMoreSlack = team.copy(culture = cultureWithMoreSlack)
      val resultsWithMoreSlack = simulate(teamWithMoreSlack, aWhile)
      resultsWithMoreSlack.productValue should be > results.productValue }
    }
  }
}

