package teamo;

case class Codebase(quality: CodeQuality = 1)

case class BugTracker(problems: Set[Problem])
