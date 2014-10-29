package teamo

import java.util.Date

import SkillSet.SkillLevel
import akka.actor.{Props, ActorRef, Cancellable, Actor}
import akka.agent.Agent
import teamo.TeaMo.ImplementedWork

import scala.concurrent.duration._
// does this work? Why does this work?


// someday: randomly generate starting skillsets etc of members
/*
 * Todo: express property "when there is a problem in the bug tracker it gets worked on first"
 */
class Team(nature: TeamNature, teamo: ActorRef,
  codebase: Agent[Codebase]
  , bugTracker: Agent[BugTracker]) extends Actor {

  var teamMembers: Seq[ActorRef] = Seq()
  val butt = nature.featureSupplierFactory()

  override def preStart {

    val newTeamMembers =  for(i<-0.to(nature.membersCount)) yield
      context.system.actorOf(Props(new Coder(self,teamo,codebase,nature.culture)))

    teamMembers++newTeamMembers

    newTeamMembers.foreach(x => x ! pullFeatureOutOfButt())
  }

  def receive = {
    case Idle => sender ! problem.getOrElse(pullFeatureOutOfButt())
  }

  var handedProblems:Seq[Problem] = Seq()
  def problem(): Option[Problem] = {
    val ret = bugTracker.get.problems.filterNot(handedProblems.contains).headOption
    ret match {
      case Some(x) => handedProblems = handedProblems :+ x
      case _ =>
    }
    ret
  }

  def pullFeatureOutOfButt(): Feature = butt()

}

case object Idle
case object Finished

case class SkillSet(
  codebaseFamiliarity: SkillLevel // you know what Clojure gets 100% right? commas as whitespace.
//  , techs: Map[Tech, SkillLevel]
) {
   def addExperience(feature: Workable,
     slack: Double): SkillSet = {
     // here is a function.
     val codebaseGainRatio = (feature.difficulty.realExpectedDuration.toHours/4) * (0.01 + slack)/100
     // Features could also have tech, someday.
     val newFamiliarity = codebaseFamiliarity + (1-codebaseFamiliarity) * codebaseGainRatio
     copy(codebaseFamiliarity = newFamiliarity)
   }

}
object SkillSet {
   def starting :SkillSet = SkillSet(0)
   type SkillLevel = Double

}


// but the essential thing is that working on a task doesn't only get that task done.

class Coder(manager: ActorRef, teamo: ActorRef, codebase: Agent[Codebase], culture: Culture) extends Actor {
  //Scala doesnt want us to use their stack lass, and since List is a linked list...
  type Stack[T] = List[T]
  val name = Coder.nameGen.sample.get

  var currentTask: Stack[Workable] = List()//List(petProject) // pet project is never finished (property)
    // feature and problem need a common superclass of Task?
  var workingSince: Date = new Date()
  var finishment: Option[Cancellable] = None

  var skillSet = SkillSet.starting // could be random

  // here's what I want:
  def receive = {
    case Finished =>
      val completedWork = currentTask.head
      reapBenefits(completedWork)
                    teamo ! new ImplementedWork(completedWork,culture.slack,skillSet) // Missing: quality level of feature
                    currentTask = currentTask.tail
                    if (currentTask.isEmpty) manager ! Idle else rescheduleFinishment()
                    say("finished " + completedWork)
    case t: Workable => currentTask= t::currentTask; rescheduleFinishment(); say("starting " + t)
    case wat => println("Waaaaah " + name + " doesn't know how to " + wat )
  }

  def say(msg: String) {
    println(new Date() + " " + name + " " + msg)
  }

  def reapBenefits(task: Workable) = {
    skillSet = skillSet.addExperience(task, culture.slack.value)
  }

  //def onPetProject: Boolean = currentTask.size <= 1

  // in this version, there is no % complete. Every task starts over. Not ideal - closer to
  // reality than ignoring cost of context switching and merging though
  def rescheduleFinishment() {
    implicit val executionContext = context.system.dispatcher
    finishment.map(_.cancel())
    val t = howLongWillThisTakeMe(currentTask.head)
    val finiteT = t match {
      case f:FiniteDuration => f
      case _ => println("warning, we got an infinite duration"); 20.millis
    }
    finishment = Some(context.system.scheduler.scheduleOnce(finiteT, self, Finished))
  }

  def petProject: Feature = ??? // some % learning, varies by coder
  // later: pet project turns out to be incredibly valuable feature.
  // Is probably rejected by management

  def howLongWillThisTakeMe(task: Workable): Duration = {
      // scale the difficulty of the task into realtime
      // increase based on slack
      // decrease based on skill level
      // increase for lower codebase quality
     FeatureDurationGuesser.howLongWillThisTake(task.difficulty,
       skillSet,
       codebase().quality,
       culture.slack)
  }

}

object Coder {
  val nameGen = org.scalacheck.Gen.oneOf("Jamie","Pat","Kim","Alex","Addison","Haim","Subash",
    "Saron","Paco","Parag","Andrea","Bodil","Francesco","Mario","Chad","Clement","Viktor")
}

