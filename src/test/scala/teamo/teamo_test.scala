package teamo;

import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop._

import scala.concurrent.duration._

class SomeSpec extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  def simulate(t: Team, p: Project, c: Culture, d: FiniteDuration) = {
   Simulation.run(t,p,c,d);
  }

  def moreSlack(culture: Culture) = culture.copy(slack = culture.slack + 0.05)

  test("after a while, the team with more slack is more productive") {
    forAll (TeamGen(),
      Gen.const(new Project),
      Gen.const(new Culture))
      { (team : Team, project: Project, culture: Culture) =>
      val aWhile = 90.days
      val results = simulate(team, project, culture, aWhile)
      val resultsWithMoreSlack = simulate(team, project, moreSlack(culture), aWhile)
      resultsWithMoreSlack.productValue should be > results.productValue
    }
  }

}

