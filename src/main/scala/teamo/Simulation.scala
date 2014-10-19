package teamo

import akka.util.Timeout
import teamo.TeaMo.{TeaMoValue, GetValue}

import scala.concurrent._
import scala.concurrent.duration._
import akka.actor._
import akka.agent.Agent
import akka.pattern._
object Simulation {

  def main(args: Array[String]) {
    val results = run(TeamNature(Culture(slack = 0.5),
         4,
         () => () => Feature(valueAdd = 1, Difficulty(1))
      ), 10.days)
    println("--------------------------")
    println(results)
    println("-----------YAY------------")
  }

  def run(t: TeamNature, d: FiniteDuration) = {
   import ExecutionContext.Implicits.global
    implicit val timeout:Timeout = 3.seconds
    println(s"starting simulation: $t $d ")
    val system = ActorSystem("teamo")

    /* GIT INIT */
    val codebase = Agent(CodeBase(1))

    /* TEAMO */
    val teamo = system.actorOf(Props[TeaMo])

    /* HIRE */
    val developmentTeam = system.actorOf(Props(new Team(t, teamo, codebase)))

    //val timeKeeper = system.actorOf(Props(new TimeKeeper(d,teamo)))

    Timing.wait(d)
    //timeKeeper ! Work(0.millis,new Coder,c,d)
    val valueFuture = teamo ? GetValue

    // thinking about: passing Future out of here, waiting in test
    // Someday maybe the test frameworks will support proper async
    val value = Await.result(valueFuture, 3.seconds).asInstanceOf[TeaMoValue]

    system.shutdown()
    Results (value.value)
  }
}
//this actor could hold the messages for a while, or do its own tracking of start times, if we wanted to
class TimeKeeper(max:FiniteDuration,teamo:ActorRef) extends Actor{
  def receive:Receive = {
   // case w:Work => if (w.startTime + w.time <= max) teamo.forward(w)
    case x =>teamo.forward(x)
  }
}
