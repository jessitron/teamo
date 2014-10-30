package teamo

import akka.actor.{Props, ActorSystem}
import akka.agent.Agent
import akka.util.Timeout
import org.scalatest.{Matchers, FunSuite}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import akka.pattern.ask
import teamo.TeaMo.{ImplementedWork, TeaMoValue, GetValue}

import teamo.Implicits._

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

class TeaMoActorSpec extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

import scala.language.existentials
  val magicalBugTracker = new BugTracker(Set()) {
    override def + (p: Problem) = this // eat problems
  }
  val dummyCodebase = Codebase()

  test("A good programmer adding features increases value" /* (it might not always but now it should) */) {
    forAll { (features: Set[Feature], addlFeature: Feature, slack: Slack, skillSet: SkillSet) =>
      // this could use just one actor system
      val sys = ActorSystem(/* unique name */ "poo")
      val teamo = sys.actorOf(Props(new TeaMo(Agent(magicalBugTracker), Agent(dummyCodebase))))
      features.foreach { f =>
        teamo ! TeaMo.ImplementedWork(f, slack, skillSet)
      }

      implicit val timeout:Timeout = 5.seconds
      val valueBefore = (teamo ? GetValue).mapTo[TeaMoValue]
      teamo ! ImplementedWork(addlFeature,Slack(2.0),SkillSet(1))
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
