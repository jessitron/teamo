package teamo

import java.util.Date
import akka.util.Timeout
import teamo.TeaMo.{TeaMoValue, GetValue}

import scala.concurrent._
import scala.concurrent.duration._
import akka.actor._
import akka.agent.Agent
import akka.pattern._
object Simulation {

  def main(args: Array[String]) {
    val s = 3
    val r = new scala.util.Random(3148006269L)
    val results = run(TeamNature(Culture(Slack(s)),
         5,
         () => () => {
           val usefulness = r.nextDouble * 4; // might be 0. Politics
           Feature(valueAdd = usefulness, Difficulty(1,RealDifficultyGenerator()))
         }
      ), 180.days)
    println("--------------------------")
    println(results)
    println("-----------YAY------------")
    println(s"That was with slack of $s")
  }

  def run(t: TeamNature, d: FiniteDuration,logFile:String = "results.csv") = {
   import ExecutionContext.Implicits.global
    implicit val timeout:Timeout = 3.seconds
    println(s"starting simulation: $t $d ")
    val system = ActorSystem("teamo")
    val output = new FileLogger(logFile)

    /* GIT INIT */
    val codebase = Agent(Codebase(1))
    val bugTracker = Agent(BugTracker())

    /* TEAMO */
    val teamo = system.actorOf(Props(new TeaMo(bugTracker, codebase, output)))

    /* HIRE */
    val developmentTeam = system.actorOf(Props(new Team(t, teamo, codebase,
      bugTracker)))

    //val timeKeeper = system.actorOf(Props(new TimeKeeper(d,teamo)))

    Timing.wait(d)
    //timeKeeper ! Work(0.millis,new Coder,c,d)
    val valueFuture = teamo ? GetValue

    // thinking about: passing Future out of here, waiting in test
    // Someday maybe the test frameworks will support proper async
    val value = Await.result(valueFuture, 3.seconds).asInstanceOf[TeaMoValue]

    say("shutting down")
    system.shutdown()
    output.close()
    Results (value.value)
  }

  def say(msg: String) {
    println(new Date() + " " + msg)
  }
}
//this actor could hold the messages for a while, or do its own tracking of start times, if we wanted to
class TimeKeeper(max:FiniteDuration,teamo:ActorRef) extends Actor{
  def receive:Receive = {
   // case w:Work => if (w.startTime + w.time <= max) teamo.forward(w)
    case x =>teamo.forward(x)
  }
}
