package teamo

import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop._

import scala.concurrent.duration._

class SomeSpec extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  def simulate(t: Team, c: Culture, d: FiniteDuration) = {
   Simulation.run(t,c,d)
  }

  def moreSlack(culture: Culture) = culture.copy(slack = culture.slack + 0.05)

  test("after a while, the team with more slack is more productive") {
    forAll (TeamGen(),
      CultureGen())
      { (team : Team, culture: Culture) =>
      val aWhile = 90.days
      val results = simulate(team, culture, aWhile)
      val resultsWithMoreSlack = simulate(team, moreSlack(culture), aWhile)
      resultsWithMoreSlack.productValue should be > results.productValue
    }
  }

}

