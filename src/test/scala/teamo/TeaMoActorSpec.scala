package teamo

import akka.actor.{Props, ActorSystem}
import akka.agent.Agent
import akka.util.Timeout
import org.scalatest.{Matchers, FunSuite}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import akka.pattern.ask
import teamo.TeaMo.{TeaMoValue, GetValue}

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

class TeaMoActorSpec extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

import scala.language.existentials
  val magicalBugTracker = new BugTracker(Set()) {
    override def + (p: Problem) = this
  }

  test("Adding features increases value" /* (it might not always but now it should) */) {
    forAll(FeaturesGen(),FeatureGen()) { (features: Set[Feature], addlFeature: Feature) =>
      // this could use just one actor system
      val sys = ActorSystem(/* unique name */ "poo")
      val teamo = sys.actorOf(Props(new TeaMo(Agent(magicalBugTracker))))
      features.foreach { f =>
        teamo ! f
      }

      implicit val timeout:Timeout = 5.seconds
      val valueBefore = (teamo ? GetValue).mapTo[TeaMoValue]
      teamo ! addlFeature
      val valueAfter = (teamo ? GetValue).mapTo[TeaMoValue]

      // after problems happen, this may not be true.
      // need a way to say AdvertisedValue, and get the value w/o problems

      val differenceFuture = valueAfter.flatMap(
        after =>
        valueBefore.map { before => after.value - before.value})

      val difference = Await.result(differenceFuture,5.seconds)
      sys.shutdown()
      difference should be > 0.0
    }
  }
}
