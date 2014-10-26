package teamo

import probability_monad.Distribution
import teamo.TeaMo.ImplementedFeature

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

object RealDifficultyGenerator {
  private val k = 2.0
  private val theta = 1.0

  val defaultDistribution = Distribution.gamma(k, theta).map(x=> (x * 12.hours) + 1.hour).map{
    x=> if(x>7.days) 7.days else x
  }

  def apply() = defaultDistribution.sample(1).head
}

object ProblemGenerator {
  
  def generate(imf:ImplementedFeature):Problem = 
    new Problem(imf.feature.difficulty, slackAndDifficultyToPain(imf))

  def slackAndDifficultyToPain(imf:ImplementedFeature):Double = {
    Math.min(Math.max(0,(0.5 - imf.skill.codebaseFamiliarity)/Math.max(0.25,1-imf.slack.value)),1)
  }
}
