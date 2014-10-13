package teamo

import java.time.Duration

import SkillSet.SkillLevel
import akka.actor.Actor
import teamo.ProjectManager.{Idle, Finished}

// does this work? Why does this work?

class Team {

}

case class SkillSet(
  codebase: SkillLevel // you know what Clojure gets 100% right? commas as whitespace.
//  , techs: Map[Tech, SkillLevel]
) {
   def addExperience(feature: Feature,
     slack: Double): SkillSet = {
     // here is a function.
     val codebaseGain = feature.difficulty.points * (0.01 + slack)
     // Features could also have tech, someday.
     copy(codebase = codebase + codebaseGain)
   }

}
object SkillSet {
   def starting :SkillSet = SkillSet(0)
   type SkillLevel = Double

}


// but the essential thing is that working on a task doesn't only get that task done.

class Coder(manager: ProjectManager, teamo: TeaMo, culture: Culture) extends Actor {
  //Scala doesnt want us to use their stack lass, and since List is a linked list...
  type Stack[T] = List[T]

  var currentTask: Stack[Feature] = List(petProject) // pet project is never finished (property)
    // feature and problem need a common superclass of Task?
  var workingSince: Date = new Date()
  var finishment: Option[Cancellable] = None

  var skillSet = SkillSet.starting // could be random

  // here's what I want:
  def receive = {
    case Finished => reapBenefits(currentTask.head)
                    teamo ! currentTask.tail;  // Missing: quality level of feature
                    if (currentTask.head == petProject) manager ! Idle else rescheduleFinishment()
    case t: Feature => t::currentTask; rescheduleFinishment()
  }

  def reapBenefits(task: Feature) = {
    skillSet = skillSet.addExperience(task, culture.slack)
  }

  def onPetProject: Boolean = currentTask.size <= 1

  // in this version, there is no % complete. Every task starts over. Not ideal - closer to
  // reality than ignoring cost of context switching and merging though
  def rescheduleFinishment() {
    finishment.map(_.cancel)
    val t = howLongWillThisTake(currentTask.peek)
    finishment = Some(scheduler.scheduleOnce(t, self, Finished))
  }

  def petProject: Feature = ??? // some % learning, varies by coder
  // later: pet project turns out to be incredibly valuable feature.
  // Is probably rejected by management

  def howLongWillThisTakeMe(task: Feature): Duration = {
    if (onPetProject) { Duration.forever }
    else {
      // scale the difficulty of the task into realtime
      // increase based on slack
      // decrease based on skill level
    }
  }

}

class ProjectManager(val teamo:TeaMo) {
  def createTask :Task = ???
}

object ProjectManager{
  case object Idle
  case object Finished
}
