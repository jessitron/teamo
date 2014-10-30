package teamo

import scala.concurrent.duration._

case class Difficulty(points: Int, realExpectedDuration: Duration = RealDifficultyGenerator()){
  def * (d:Double) = copy(realExpectedDuration = realExpectedDuration * d)
}

trait Workable{
  val difficulty:Difficulty
}

/* reference equality is important here */
class Problem(val difficulty: Difficulty,
  /* percentage of functionality killed */
  val impact: Double) extends Workable{
  override def toString = { s"Problem{$impact)"}
}

case class Feature(valueAdd: Value, difficulty:Difficulty) extends Workable
