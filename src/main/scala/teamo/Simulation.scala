package teamo

import akka.util.Timeout
import teamo.TeaMo.{TeaMoValue, GetValue}

import scala.collection.mutable
import scala.concurrent._
import scala.concurrent.duration._
import akka.actor._
import akka.pattern._
object Simulation {

  def run(t: Team, c: Culture, d: FiniteDuration) = {
   import ExecutionContext.Implicits.global
    implicit val timeout:Timeout = 3.seconds
    val system = ActorSystem("teamo")
    val teamo = system.actorOf(Props[TeaMo])
    val timeKeeper = system.actorOf(Props(new TimeKeeper(d,teamo)))
    timeKeeper ! Work(0.millis,new Coder,c,d)
    val valueFuture = timeKeeper ? GetValue
    val value = blocking {
      Await.result(valueFuture, 3.seconds).asInstanceOf[TeaMoValue]
    }
    system.shutdown()
    Results (value.value)
  }
}
//this actor could hold the messages for a while, or do its own tracking of start times, if we wanted to
class TimeKeeper(max:FiniteDuration,teamo:ActorRef) extends Actor{
  def receive:Receive = {
    case w:Work => if (w.startTime + w.time <= max) teamo.forward(w)
    case x =>teamo.forward(x)
  }
}
