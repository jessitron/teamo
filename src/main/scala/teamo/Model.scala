package teamo

import probability_monad.Distribution

import scala.concurrent.duration._

object FeatureDurationGuesser {
   def howLongWillThisTake(task: Difficulty,
                           skillSet: SkillSet,
                           codeBase: CodeQuality,
                           slack: Slack): Distribution[Duration] = {
    val minimum = task.difficulty.points * 1.day * (1 + slack)
    val k = 2.0
    val theta = 1.0
    Distribution.gamma(k, theta).map(_ * 1.day).map(_ + minimum)
  }
}
