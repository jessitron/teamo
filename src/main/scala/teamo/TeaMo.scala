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

  def receive:Receive= {
    case w:ImplementedWork => integrateWork(w)
      progressionOfEvil(w)
      loginate()
    case GetValue => sender ! TeaMoValue(calculateValue)
  }

  def loginate() {
    logger.loginate(features.size, valueFromFeatures,
      bugTracker.get().problems.size, impactOfProblems,
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
  def impactOfProblems = bugTracker.get().problems.map(_.impact).map(1-_).foldLeft(1.0)(_*_)
  // This should be an integral over time.
  def calculateValue = {
     // this could be a lot more complicated, it should be
     // a fold over each set. But for now, ultra-simple.
    //println(features.size + " " + features)
    //println(problems)
    val featureValue = valueFromFeatures
    val problemMultiplier = impactOfProblems
    //println(s"$featureValue * $problemMultiplier")
     featureValue * problemMultiplier
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
