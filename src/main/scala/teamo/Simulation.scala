package teamo

import akka.util.Timeout
import teamo.TeaMo.{TeaMoValue, GetValue}

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
    teamo ! Work(new Coder,c,d)
    val valueFuture = teamo ? GetValue
    val value = Await.result(valueFuture,3.seconds).asInstanceOf[TeaMoValue]
    system.shutdown()
    Results (value.value)
  }
}
