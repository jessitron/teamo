package teamo

class Team {
}

class Coder(manager: ProjectManager, teamo: TeaMo) extends Actor {
  var currentTask: Stack[Feature] = Stack(petProject); // pet project is never finished (property)
  var workingSince: Date = new Date();
  var finishment: Option[Cancellable] = None;

  // here's what I want:
  def receive = {
    case Finished => teamo ! currentTask.pop; if (currentTask.peek.isPetProject) manager ! Idle else rescheduleFinishment()
    case t: Feature => currentTask.push(t); rescheduleFinishment()

  }

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
    if (currentTask.size == 1) { Duration.forever } // pet project
    else {
     // scale the difficulty of the task into realtime

    }
  }

}

class ProjectManager(val teamo:TeaMo) {
  def createTask :Task = ???
}
