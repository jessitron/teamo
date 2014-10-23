package teamo

import probability_monad.Distribution

import scala.concurrent.duration._

object FeatureDurationGuesser {
   def howLongWillThisTake(task: Difficulty,
                           skillSet: SkillSet,
                           codeBase: CodeQuality,
                           slack: Slack): Duration = {
    //if you have no idea, things take double
    val skillMultiplier = 2-skillSet.codebaseFamiliarity
    //if the codebase is terrible, things take double
    val codeBaseMultiplier = 1
    // println(s"${task.realExpectedDuration.toMillis} $codeBaseMultiplier $skillMultiplier")
    Timing.scale(task.realExpectedDuration * codeBaseMultiplier * skillMultiplier) * (1+slack.value)
  }
}
