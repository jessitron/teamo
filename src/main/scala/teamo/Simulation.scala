package teamo;

import scala.concurrent.duration._
import akka.actor._

object Simulation {

  def run(t: Team, c: Culture, d: FiniteDuration) = {

    val system = ActorSystem("teamo")
    val teamo = system.actorOf(Props[TeaMo])
    system.shutdown()
    Results (3 * c.slack)
  }
}
