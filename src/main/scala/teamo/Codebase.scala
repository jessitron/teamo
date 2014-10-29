package teamo

case class Codebase(quality: CodeQuality = 1)

class BugTracker(val problems: Set[Problem]) {
  def + (p: Problem) = new BugTracker(problems + p)
  def ++ (p: Set[Problem]) = new BugTracker(problems ++ p)
  def - (p: Problem) = new BugTracker(problems - p)
}

object BugTracker {
  def apply(): BugTracker = new BugTracker(Set())
}

