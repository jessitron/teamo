package teamo

import java.util.Date

import SkillSet.SkillLevel
import akka.actor.{ActorRef, Cancellable, Actor}

import scala.concurrent.duration._
// does this work? Why does this work?

// someday: randomly generate starting skillsets etc of members
class Team(culture: Culture, 
           membersCount: Int,
           teamo: ActorRef, butt: () => Feature) extends Actor {

  override def preStart {
     // create team members. This is the manager
  }

  def receive = {
    case Idle => sender ! pullFeatureOutOfButt()
  }

  def pullFeatureOutOfButt(): Feature = butt()

}

case object Idle
case object Finished

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

class Coder(manager: ActorRef, teamo: ActorRef, culture: Culture) extends Actor {
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

  def howLongWillThisTake(feature: Feature):FiniteDuration = ???

  // in this version, there is no % complete. Every task starts over. Not ideal - closer to
  // reality than ignoring cost of context switching and merging though
  def rescheduleFinishment() {
    implicit val executionContext = context.system.dispatcher
    finishment.map(_.cancel())
    val t = howLongWillThisTake(currentTask.head)
    finishment = Some(context.system.scheduler.scheduleOnce(t, self, Finished))
  }

  def petProject: Feature = ??? // some % learning, varies by coder
  // later: pet project turns out to be incredibly valuable feature.
  // Is probably rejected by management

  def howLongWillThisTakeMe(task: Feature): Duration = {
    if (onPetProject) { Duration.Undefined }
    else {
      // scale the difficulty of the task into realtime
      // increase based on slack
      // decrease based on skill level
      // increase for lower codebase quality
     val distribution = FeatureDurationGuesser.howLongWillThisTake(task, skillSet, codeBase /*agent needs passed in */, culture.slack)
     Timing.scale(distribution.get)
    }
  }

}

