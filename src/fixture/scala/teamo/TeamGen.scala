package teamo

import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary


object TeamNatureGen{
  def apply(): Gen[TeamNature] = {
    for{d<-Gen.choose(1,5)
        c<-CultureGen()
        bf <- ButtFactoryGen()}
     yield TeamNature(c,d,bf)
  }
}

object ButtFactoryGen {
  import TeamNature.ButtFactory
  def apply(): Gen[ButtFactory] = {
    for { features <- nonEmptyContainerOf[Seq,Feature](FeatureGen())}
    yield new Function0[Function0[Feature]] {
      override def toString = "implementing: " + features
      def apply() = new Function0[Feature] {
      val iter = Stream.continually(features).flatten.iterator
      def apply : Feature = iter.next()
    }
    }
  }
}

object SlackGen {
  def apply(): Gen[Slack] = {
    for (d <- Gen.choose(0.0, 3.0)) yield Slack(d)
  }
}

object CultureGen{
  def apply(): Gen[Culture] = {
    for(d<-SlackGen()) yield Culture(d)
  }
}
object DifficultyGen{
  def apply(): Gen[Difficulty] = {
    for(d<-Gen.choose(0.01,2.0)) yield Difficulty(d)
  }
}

object FeatureGen {
  def apply():Gen[Feature] = {
    for(v<-Gen.choose(1.0,100.0);
        d<-DifficultyGen()) yield Feature(v,d)
  }
}

object FeaturesGen{
  def apply():Gen[Set[Feature]] = {
   Gen.containerOf[Set,Feature](FeatureGen())
  }
}

object SkillSetGen {
  def apply() : Gen[SkillSet] = {
     Gen.choose(0.0,1.0) map (SkillSet(_))
  }
}

object Implicits {

  implicit val arbDiff: Arbitrary[Difficulty] = Arbitrary(DifficultyGen())
  implicit val b: Arbitrary[SkillSet] = Arbitrary(SkillSetGen())
  implicit val s: Arbitrary[Slack] = Arbitrary(SlackGen())
}
