package teamo

import akka.actor.Actor
import teamo.TeaMo.{TeaMoValue, GetValue}

import scala.concurrent.duration._

case class Difficulty(points: Double)

/* reference equality is important here */
class Problem(val difficulty: Difficulty,
  /* percentage of functionality killed */
  val impact: Double){
  override def toString = { s"Problem{$difficulty,$impact)"}
}

case class Feature(valueAdd: Value, difficulty:Difficulty)

case class CodeBase(quality: CodeQuality)

// needs to receive codebase agent
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
