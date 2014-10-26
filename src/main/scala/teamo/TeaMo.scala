package teamo

import akka.actor.Actor
import teamo.TeaMo.{ImplementedFeature, TeaMoValue, GetValue}

import scala.concurrent.duration._




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

