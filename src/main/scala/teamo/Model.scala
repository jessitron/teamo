package teamo

import probability_monad.Distribution
import teamo.TeaMo.ImplementedWork

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

  val defaultAnnoyance = 0.1

  def generate(iw:ImplementedWork):Set[Problem] = {
    val impact = calculateImpact(iw)
    Set(new Problem(iw.work.difficulty * (impact * 4), impact))
  }
  private def calculateImpact(iw:ImplementedWork):Double = {

    //so if we don't understand the code, half our slack is useless
    val slackValueRatio =  0.5 + (iw.skill.codebaseFamiliarity/2)
    //Note that this is linear, which is very unrealistic.
    val annoyanceDenominator = 1+8*(iw.slack.value * slackValueRatio)
    val actualAnnoyance = defaultAnnoyance/ annoyanceDenominator

    //println(s"Problem annoyance: $actualAnnoyance  $annoyanceDenominator $slackValueRatio")
    Math.min(Math.max(0,actualAnnoyance),1) //last check to make sure it's between 1 and 0
  }
}
