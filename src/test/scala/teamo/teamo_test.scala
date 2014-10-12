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

    /* this should be in a different file. It's testing TeaMo the actor, not the project */
  test("Adding features increases value" /* (it might not always but now it should) */) {
    forAll { (features: Set[Feature], addlFeature: Feature) =>
      // this could use just one actor system
      val sys = ActorSystem(/* unique name */"poo")
      val teamo = sys.actorOf(Props[TeaMo])
      features.foreach { f =>
         teamo ! f
      }
      val valueBefore = teamo ? GetValue
      teamo ! addlFeature
      val valueAfter = teamo ? GetValue

      // after problems happen, this may not be true.
      // need a way to say AdvertisedValue, and get the value w/o problems

      val differenceFuture = valueAfter.flatMap( after =>
          valueBefore.map { before => after - before })

      val difference = Await.result { differenceFuture }

      sys.shutdown()

      difference should be > 0
    }


}

