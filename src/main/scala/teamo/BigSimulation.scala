package teamo

import scala.concurrent.duration._

object BigSimulation {
  def main(args: Array[String]) {
    for (s <- 0.0.to(3.0, 0.2)) {
      val r = new scala.util.Random(3148006269L)
      val slackString = "%1.1f" format s
      val results = Simulation.run(TeamNature(Culture(Slack(s)),
        5,
        () => () => {
          val usefulness = r.nextDouble * 4; // might be 0. Politics
          Feature(valueAdd = usefulness, Difficulty(1, RealDifficultyGenerator()))
        }
      ), 180.days, s"results-$slackString.csv")
      println("--------------------------")
      println(results)
      println("-----------YAY------------")
      println(s"That was with slack of $s")
    }
  }
}
