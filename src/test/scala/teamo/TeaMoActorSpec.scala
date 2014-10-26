package teamo

import akka.actor.{Props, ActorSystem}
import akka.agent.Agent
import akka.util.Timeout
import org.scalatest.{Matchers, FunSuite}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import akka.pattern.ask
import teamo.TeaMo.{TeaMoValue, GetValue}

import teamo.Implicits._

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

class TeaMoActorSpec extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

import scala.language.existentials
  val magicalBugTracker = new BugTracker(Set()) {
    override def + (p: Problem) = this // eat problems
  }

  test("Adding features increases value" /* (it might not always but now it should) */) {
    forAll { (features: Set[Feature], addlFeature: Feature, slack: Slack, skillSet: SkillSet) =>
      // this could use just one actor system
      val sys = ActorSystem(/* unique name */ "poo")
      val teamo = sys.actorOf(Props(new TeaMo(Agent(magicalBugTracker))))
      features.foreach { f =>
        teamo ! TeaMo.ImplementedFeature(f, slack, skillSet)
      }

      implicit val timeout:Timeout = 5.seconds
      val valueBefore = (teamo ? GetValue).mapTo[TeaMoValue]
      teamo ! TeaMo.ImplementedFeature(addlFeature, slack, skillSet)
      val valueAfter = (teamo ? GetValue).mapTo[TeaMoValue]

      // after problems happen, this may not be true.
      // need a way to say AdvertisedValue, and get the value w/o problems

      val differenceFuture = valueAfter.flatMap(
        after =>
        valueBefore.map { before =>
           println(s"before = $before after = $after")
        after.value - before.value})

      val difference = Await.result(differenceFuture,5.seconds)
      sys.shutdown()
      difference should be > 0.0
    }
  }
}
