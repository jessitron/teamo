package teamo

import probability_monad.Distribution
import scala.concurrent.duration._
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalatest.{Matchers, FunSuite}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ModelSpec extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  import FeatureDurationGuesser._
  import teamo.Implicits._

  /* freakin A, how do I provide this conversion as evidence? */
  implicit def weLikeMilliseconds(d: Duration):Double = d.toMillis.toDouble

  test("a task never takes forever") {
     forAll() {
       (d: Difficulty,
       skillSet: SkillSet, code: CodeQuality, slack: Slack) =>

          val some = howLongWillThisTake( d, skillSet, code, slack)
          assert(some.isFinite())
          assert(some > 0.seconds)
     }
  }

  /*test("expected duration and variance increase with task difficulty") {
    forAll() {
       (p1: Difficulty, p2: Difficulty,
       skillSet: SkillSet, code: CodeQuality, slack: Slack) =>
         /* this can fail intermittently for values of p1 and p2 very near each other */
        whenever(p1.points > p2.points) {
           val higherDifficulty = p1
           val lowerDifficulty = p2

          val higherDifficultyDistribution: Distribution[Double] =
             howLongWillThisTake( higherDifficulty,
                skillSet, code, slack)


          val lowerDifficultyDistribution: Distribution[Double] =
             howLongWillThisTake( lowerDifficulty,
                skillSet, code, slack)

          assert(higherDifficultyDistribution.ev > lowerDifficultyDistribution.ev)
          assert(higherDifficultyDistribution.variance > lowerDifficultyDistribution.variance)

         }

    }
  }*/

}
