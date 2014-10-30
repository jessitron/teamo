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
    val problems =
      for (i <- 1.to(qtyOfProblems(iw.skill, code)))
        yield new Problem(chooseDifficulty(iw, i), calculateImpact(iw))
    problems.toSet
  }

  private def chooseDifficulty(iw: ImplementedWork, i: Int) = {
    val howMuchHarder = Seq(0.125, 0.25, 0.5, 1, 2)(i)
    // ignoring points in difficulty
    iw.work.difficulty * howMuchHarder
  }

  private def qtyOfProblems(skill: SkillSet, code: Codebase ): Int = {
    val unfamiliarCode = code.size * (1 - skill.codebaseFamiliarity)
    val createdProblems = unfamiliarCode / (10 * typicalFeatureSize)
    rangeLimit(createdProblems.toInt, low = 1, high = 5)
  }

  private def rangeLimit(v: Int, low: Int, high: Int) = Math.min(high, Math.max(low, v))

  val impactsFromSmallToLarge: Seq[Workable => Impact] =
    Seq(partOfChange(0.25), partOfChange(0.50), partOfChange(1.00),
      systemwideImpact(0.10), systemwideImpact(0.20), systemwideImpact(1.00))
  val maxSeverity = (impactsFromSmallToLarge.size)

  def impact(severity:Int, w: Workable) = impactsFromSmallToLarge(severity)(w)

  // it's not a var, but it's stateful so I mark it that way
  // this is WAY too harsh. It should skew strongly toward the smaller numbers
  var impactSeverityPattern: Iterator[Int] = Stream.continually(0 until maxSeverity).flatten.iterator

  private def calculateImpact(iw:ImplementedWork): Impact = {
    val nextSeverity = impactSeverityPattern.next

    val slackReducesMaxSeverity = Math.min(4, iw.slack.value.toInt) // by up to 4
    val myMaxSeverity = maxSeverity - slackReducesMaxSeverity

    val mySeverity = Math.min(nextSeverity, myMaxSeverity)

    impact(mySeverity, iw.work)
  }

  def partOfChange(pctBroken: Double)(w: Workable) = w match {
    case f: Feature => BrokenFeature(f.valueAdd * pctBroken)
    case p: Problem => ???
  }
  def systemwideImpact(pctOutage: Double)(w: Workable) = SystemOutage(pctOutage)
}

object CodeImpact {

  val maxLinesPerMinute = 0.25

  def increaseInSize(featureSize: Difficulty, slack: Slack): CodeSize = {
    val rushedLOC = featureSize.realExpectedDuration.toUnit(MINUTES) * maxLinesPerMinute
    val slackHelps = (1 + slack.value)
    rushedLOC / slackHelps
  }
}
