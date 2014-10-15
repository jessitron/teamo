package teamo;

import probability_monad.Distribution
import scala.concurrent.duration._
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalatest.{Matchers, FunSuite}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ModelSpec extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  import FeatureDurationGuesser._

  implicit def weLikeMilliseconds(d: Duration):Double = d.toMillis

  test("expected duration and variance increase with task difficulty") {
    forAll() {
       (p1: Difficulty, p2: Difficulty,
       skillSet: SkillSet, code: CodeQuality, slack: Slack) =>
         /* this can fail intermittently for values of p1 and p2 very near each other */
        (p1.points > p2.points) ==> {
           val higherDifficulty = p1
           val lowerDifficulty = p2

          val higherDifficultyDistribution: Distribution[Duration] =
             howLongWillThisTake( higherDifficulty,
                skillSet, code, slack)


          val lowerDifficultyDistribution: Distribution[Duration] =
             howLongWillThisTake( lowerDifficulty,
                skillSet, code, slack)

          assert(higherDifficultyDistribution.ev > lowerDifficultyDistribution.ev)
          assert(higherDifficultyDistribution.variance > lowerDifficultyDistribution.variance)

         }

    }
  }

}
