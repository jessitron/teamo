package teamo

import akka.actor.Actor
import akka.agent.Agent
import teamo.TeaMo.{GetProblems, ImplementedWork, TeaMoValue, GetValue}

import scala.concurrent.duration._


// needs to receive codebase agent
class TeaMo(bugTracker: Agent[BugTracker],
  codebaseAgent: Agent[Codebase],
  logger: ValueLogger = PirateLogger()) extends Actor {

  var features: List[Feature] = List()
  def codebase = codebaseAgent.get()
  def problems = bugTracker.get().problems

  def receive:Receive= {
    case w:ImplementedWork => integrateWork(w)
      progressionOfEvil(w)
      loginate()
    case GetValue => sender ! TeaMoValue(calculateValue)
  }

  def loginate() {
    logger.loginate(features.size, valueFromFeatures,
      problems.size, 0, // "impact of problems" no longer supported
      calculateValue)
  }

  def integrateWork(iw:ImplementedWork): Unit ={
    iw.work match {
      case f:Feature => features = features :+ f
      case p:Problem =>  bugTracker.alter{ps => ps - p}
    }
  }

  def progressionOfEvil(iw:ImplementedWork) {
    //println("progression of evil"+imf)
    iw.work match {
      case f:Feature =>     bugTracker.alter(ps => ps ++ ProblemGenerator.generate(iw, codebase))
      case p:Problem => //we are just avoiding evil
    }
  }

  def valueFromFeatures = features.map(_.valueAdd).sum
  // This should be an integral over time.
  def calculateValue = {
     // this could be a lot more complicated, it should be
     // a fold over each set. But for now, ultra-simple.
    val featureValue = valueFromFeatures
    val worth = Worth(featureValue)
    problems.map(_.impact).foldRight(worth)(_.hurt(_)).currentValue
  }
}

object TeaMo{
    case object GetValue
    case object GetProblems
    case class TeaMoValue(value:Double)
    case class ImplementedWork(work:Workable,slack:Slack,skill:SkillSet)
    case class FixedProblem(problem: Problem)
}

class Task
