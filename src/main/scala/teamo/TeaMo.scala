package teamo

import akka.actor.Actor
import akka.agent.Agent
import teamo.TeaMo.{GetProblems, ImplementedWork, TeaMoValue, GetValue}

import scala.concurrent.duration._

case class Difficulty(points: Int, realExpectedDuration: Duration = RealDifficultyGenerator()){
  def * (d:Double) = copy(realExpectedDuration = realExpectedDuration * d)
}

trait Workable{
  val difficulty:Difficulty
}

/* reference equality is important here */
class Problem(val difficulty: Difficulty,
  /* percentage of functionality killed */
  val impact: Double) extends Workable{
  override def toString = { s"Problem{$impact)"}
}

case class Feature(valueAdd: Value, difficulty:Difficulty) extends Workable

// needs to receive codebase agent
class TeaMo(bugTracker: Agent[BugTracker]) extends Actor {

  var features: List[Feature] = List()

  def receive:Receive= {
    case w:ImplementedWork => integrateWork(w)
      progressionOfEvil(w)
    case GetValue => sender ! calculateValue
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
      case f:Feature =>     bugTracker.alter(ps => ps + ProblemGenerator.generate(iw))
      case p:Problem => //we are just avoiding evil
    }
  }
  
  // This should be an integral over time.
  def calculateValue = {
     // this could be a lot more complicated, it should be
     // a fold over each set. But for now, ultra-simple.
    //println(features.size + " " + features)
    //println(problems)
    val featureValue = features.map(_.valueAdd).sum
    val problemMultiplier = bugTracker.get().problems.map(_.impact).map(1-_).foldLeft(1.0)(_*_)
    //println(s"$featureValue * $problemMultiplier")
     TeaMoValue(featureValue * problemMultiplier )
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
