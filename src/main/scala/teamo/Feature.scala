package teamo
import scala.concurrent.duration._
import probability_monad.Distribution

case class Difficulty(points: Int, realExpectedDuration: Duration = Difficulty.defaultDistribution.sample(1).head)

object Difficulty{
  private val k = 2.0
  private val theta = 1.0
  val defaultDistribution = Distribution.gamma(k, theta).map(x=> (x * 1.day) + 1.hour).map{
    x=> if(x>7.days) 7.days else x
  }
  def defaultDifficulty = defaultDistribution.sample(1).head
}

/* reference equality is important here */
class Problem(val difficulty: Difficulty,
  /* percentage of functionality killed */
  val impact: Double){
  override def toString = { s"Problem{$impact)"}
}

case class Feature(valueAdd: Value, difficulty:Difficulty)
