package teamo

import scala.concurrent.duration._

case class Difficulty(points: Int, realExpectedDuration: Duration = RealDifficultyGenerator()){
  def * (d:Double) = copy(realExpectedDuration = realExpectedDuration * d)
}

trait Workable{
  val difficulty:Difficulty
}

trait Impact {
  def hurt(before: Value): Value
}
case class PercentageImpact(damage: Double) extends Impact {
  assert(damage >= 0)
  assert(damage <= 1)
  def hurt(before:Value) = before * (1-damage)
}
case class WorstCase(percentage: Double, minimumDamage: Value) extends Impact {
  def hurt(before:Value) = {
    val percentPain = before * percentage
    val greatestPain = Math.max(percentPain, minimumDamage)
    val after = before - greatestPain
    Math.min(0, after)
  }
}

/* reference equality is important here */
class Problem(val difficulty: Difficulty,
  /* percentage of functionality killed */
  val impact: Impact) extends Workable{
  override def toString = { s"Problem{$impact)"}
}
object Problem {
  def apply(difficulty: Difficulty, percentageImpact: Double): Problem =
    new Problem(difficulty, PercentageImpact(percentageImpact))
}

case class Feature(valueAdd: Value, difficulty:Difficulty) extends Workable
