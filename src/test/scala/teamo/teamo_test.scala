package teamo

import org.scalatest._
import org.scalatest.prop._
import scala.concurrent.duration._

class SomeSpec extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  def simulate(t: TeamNature, d: FiniteDuration) = {
   Simulation.run(t,d)
  }

  def moreSlack(team:TeamNature):TeamNature = team.copy(culture = moreSlack(team.culture))
  def moreSlack(culture: Culture) = culture.copy(slack = culture.slack + 0.05)

  test("after a while, the team with more slack is more productive") {
    forAll (TeamNatureGen(),
      CultureGen())
      { (team : TeamNature, culture: Culture) =>
      val aWhile = 90.days
      val results = simulate(team, aWhile)
      val resultsWithMoreSlack = simulate(moreSlack(team), aWhile)
      resultsWithMoreSlack.productValue should be > results.productValue
    }
  }
}

