package teamo;

import scala.concurrent.duration._
import akka.actor._

object Simulation {

  def run(t: Team, p: TeaMo, c: Culture, d: FiniteDuration) = {

    val system = ActorSystem("teamo")
    system.shutdown()
    Results (3 * c.slack)
  }
}
