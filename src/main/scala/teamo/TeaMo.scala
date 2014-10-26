package teamo

import akka.actor.Actor
import akka.agent.Agent
import teamo.TeaMo.{ImplementedFeature, TeaMoValue, GetValue, FixedProblem}

import scala.concurrent.duration._




// needs to receive codebase agent
class TeaMo(bugTracker: Agent[BugTracker]) extends Actor {

  var features: List[Feature] = List()

  def receive:Receive= {
    case w:ImplementedFeature =>
     // println(s"Release notes: $w")
      features = features :+ w.feature
      progressionOfEvil(w)
    case FixedProblem(p) => bugTracker.alter { ps => ps - p }
    case GetValue => sender ! calculateValue
  }

  def progressionOfEvil(imf:ImplementedFeature){
    //println("progression of evil"+imf)
    bugTracker.alter(ps => ps + new Problem(imf.feature.difficulty, slackAndDifficultyToPain(imf)))
  }

  def slackAndDifficultyToPain(imf:ImplementedFeature):Double = {
    Math.min(Math.max(0,(0.5 - imf.skill.codebaseFamiliarity)/Math.max(0.25,1-imf.slack.value)),1)
  }

  // This should be an integral over time.
  def calculateValue = {
     // this could be a lot more complicated, it should be
     // a fold over each set. But for now, ultra-simple.
   // println(features)
   // println(bugTracker.get)
     TeaMoValue(
       features.map(_.valueAdd).sum *
       bugTracker.get.problems.map(_.impact).map(1-_).foldLeft(1.0)(_*_))
  }
}

object TeaMo{
    case object GetValue
    case class TeaMoValue(value:Double)
    case class ImplementedFeature(feature:Feature,slack:Slack,skill:SkillSet)
    case class FixedProblem(problem: Problem)
}

