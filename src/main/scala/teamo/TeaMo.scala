package teamo

import akka.actor.Actor
import teamo.TeaMo.{TeaMoValue, GetValue}

import scala.concurrent.duration._
//this difficulty probably becomes something more complicated later
//as different techs provide different difficulty challenges

case class Work(startTime: FiniteDuration = 0.millis,
                coder:Coder,
                culture:Culture,
                time:FiniteDuration)

case class Difficulty(points: Double)

/* reference equality is important here */
case class Problem(difficulty: Difficulty,
  /* percentage of functionality killed */
  impact: Double)

case class Feature(valueAdd: Double)

class TeaMo extends Actor {

  var features: Set[Feature] = Set()
  var problems: Set[Problem] = Set()

  def receive:Receive= {
    case w:Feature => features = features + w
    case GetValue => sender ! calculateValue
  }

  // This should be an integral over time.
  def calculateValue = {
     // this could be a lot more complicated, it should be
     // a fold over each set. But for now, ultra-simple.
     TeaMoValue(
       features.map(_.valueAdd).sum *
       problems.map(_.impact).map(1-_).foldLeft(1.0)(_*_))
  }
}

object TeaMo{
  case object GetValue
  case class TeaMoValue(value:Double)

}

class Task
