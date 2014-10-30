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

  val typicalFeatureSize: CodeSize = 250.0

  def generate(iw:ImplementedWork, code: Codebase):Set[Problem] = {
    val impact = calculateImpact(iw)
    val problems =
      for (i <- 1.to(qtyOfProblems(iw.skill, code)))
        yield Problem(iw.work.difficulty * (impact * 4), impact)
    problems.toSet
  }

  private def qtyOfProblems(skill: SkillSet, code: Codebase ): Int = {
    val unfamiliarCode = code.size * (1 - skill.codebaseFamiliarity)
    val createdProblems = unfamiliarCode / (10 * typicalFeatureSize)
    rangeLimit(createdProblems.toInt, low = 1, high = 5)
  }

  private def rangeLimit(v: Int, low: Int, high: Int) = Math.min(high, Math.max(low, v))

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

object CodeImpact {

  val maxLinesPerMinute = 0.25

  def increaseInSize(featureSize: Difficulty, slack: Slack): CodeSize = {
    val rushedLOC = featureSize.realExpectedDuration.toUnit(MINUTES) * maxLinesPerMinute
    val slackHelps = (1 + slack.value)
    rushedLOC / slackHelps
  }
}
