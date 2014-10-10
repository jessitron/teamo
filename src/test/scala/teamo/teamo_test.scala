package teamo

import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop._

import scala.concurrent.duration._

class SomeSpec extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  def simulate(t: Team, p: TeaMo, c: Culture, d: FiniteDuration) = {
   Simulation.run(t,p,c,d)
  }

  def moreSlack(culture: Culture) = culture.copy(slack = culture.slack + 0.05)

  test("after a while, the team with more slack is more productive") {
    forAll (TeamGen(),
      Gen.const(new TeaMo),
      Gen.const(new Culture))
      { (team : Team, teamo: TeaMo, culture: Culture) =>
      val aWhile = 90.days
      val results = simulate(team, teamo, culture, aWhile)
      val resultsWithMoreSlack = simulate(team, teamo, moreSlack(culture), aWhile)
      resultsWithMoreSlack.productValue should be > results.productValue
    }
  }

}

