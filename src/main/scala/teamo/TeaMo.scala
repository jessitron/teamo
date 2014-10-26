package teamo

import akka.actor.Actor
import teamo.TeaMo.{ImplementedFeature, TeaMoValue, GetValue}

import scala.concurrent.duration._

case class Difficulty(points: Int, realExpectedDuration: Duration = RealDifficultyGenerator())

/* reference equality is important here */
class Problem(val difficulty: Difficulty,
  /* percentage of functionality killed */
  val impact: Double){
  override def toString = { s"Problem{$impact)"}
}

case class Feature(valueAdd: Value, difficulty:Difficulty)

case class CodeBase(quality: CodeQuality = 1)

// needs to receive codebase agent
class TeaMo extends Actor {

  var features: List[Feature] = List()
  var problems: List[Problem] = List()

  def receive:Receive= {
    case w:ImplementedFeature => features = features :+ w.feature
      progressionOfEvil(w)
    case GetValue => sender ! calculateValue
  }

  def progressionOfEvil(imf:ImplementedFeature) {
    //println("progression of evil"+imf)
    problems = problems :+ ProblemGenerator.generate(imf)
  }
  
  // This should be an integral over time.
  def calculateValue = {
     // this could be a lot more complicated, it should be
     // a fold over each set. But for now, ultra-simple.
    println(features)
    println(problems)
     TeaMoValue(
       features.map(_.valueAdd).sum *
       problems.map(_.impact).map(1-_).foldLeft(1.0)(_*_))
  }
}

object TeaMo{
    case object GetValue
    case class TeaMoValue(value:Double)
    case class ImplementedFeature(feature:Feature,slack:Slack,skill:SkillSet)
}

class Task
