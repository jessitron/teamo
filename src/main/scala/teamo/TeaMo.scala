package teamo

import akka.actor.Actor
import probability_monad.Distribution
import teamo.TeaMo.{ImplementedFeature, TeaMoValue, GetValue}

import scala.concurrent.duration._

case class Difficulty(points: Int, realExpectedDuration: Duration = Difficulty.defaultDistribution.sample(1).head)

object Difficulty{
  private val k = 2.0
  private val theta = 1.0
  val defaultDistribution = Distribution.gamma(k, theta).map(x=> (x * 1.day) + 1.hour).map{
    x=> if(x>7.days) 7.days else x
  }
  def defaultDifficulty = defaultDistribution.sample(1).head
}

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

  def progressionOfEvil(imf:ImplementedFeature){
    //println("progression of evil"+imf)
    problems = problems :+ new Problem(imf.feature.difficulty, slackAndDifficultyToPain(imf))
  }
  
  def slackAndDifficultyToPain(imf:ImplementedFeature):Double = {
    Math.min(Math.max(0,(0.5 - imf.skill.codebaseFamiliarity)/Math.max(0.25,1-imf.slack.value)),1)  
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
