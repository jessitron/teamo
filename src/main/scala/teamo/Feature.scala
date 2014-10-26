package teamo
import scala.concurrent.duration._
import probability_monad.Distribution

trait Task {
   val difficulty: Difficulty
   val impact: Value
}

case class Difficulty(points: Int, realExpectedDuration: Duration = Difficulty.defaultDistribution.sample(1).head) {

  override def toString():String =
    s"Difficulty($points points, takes ${DurationPrinter.format(realExpectedDuration)}"
}

object DurationPrinter {
  def format(d: Duration): String =
    if(d.isFinite) printTuple(tupleDuration(d)) else "forever!!"

  private val printTuple: Tuple2[Double,TimeUnit] => String  =
  { case (d:Double, u: TimeUnit) => "%.1f %s".format(d,u.toString.toLowerCase) }

  private def tupleDuration(d: Duration) = {
    usefulUnits.map(unit => d.toUnit(unit) -> unit).filter { case (k,v) => k >=  1 }.head
  }

  val usefulUnits = Seq(DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS, NANOSECONDS)

}

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
  val impact: Value) extends Task {
  override def toString = { s"Problem{$impact)"}
}

case class Feature(valueAdd: Value, difficulty:Difficulty) extends Task {
  val impact = valueAdd
}
