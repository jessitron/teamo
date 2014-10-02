package teamo;

import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop._

import scala.concurrent.duration._

class SomeSpec extends FunSuite with GeneratorDrivenPropertyChecks with ShouldMatchers {

  test("after a while, the team with more slack is more productive") {
    forAll { (team : Team, project: Project, culture: Culture) =>
      val aWhile = 3.months
      val results = simulate(team, project, culture, aWhile);
      val resultsWithMoreSlack = simulate(team, project, moreSlack(culture), aWhile)

      resultsWithMoreSlack.productValue > results.productValue
    }
  }

}

