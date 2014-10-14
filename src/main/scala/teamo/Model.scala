
object FeatureDurationGuesser {
   def howLongWillThisTake(task: Feature,
                           skillSet: SkillSet,
                           codeBase: CodeQuality,
                           slack: Slack): Distribution[FiniteDuration] = {
    val minimum = task.difficulty.points * 1.day * (1 + slack)
    k = 2.0
    theta = 1.0
    Distribution.gamma(k, theta).map(_ * 1.day).map(_ + minimum)
  }
}
