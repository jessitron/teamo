package teamo

import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary


object TeamNatureGen{
  def apply(): Gen[TeamNature] = {
    for(d<-Gen.choose(1,5);
       c<-CultureGen()) yield TeamNature(c,d,()=>FeatureGen().sample.get)
  }
}

object CultureGen{
  def apply(): Gen[Culture] = {
    for(d<-Gen.choose(0.0,2.0)) yield Culture(d)
  }
}
object DifficultyGen{
  def apply(): Gen[Difficulty] = {
    for(d<-Gen.choose(0.0,10.0)) yield Difficulty(d)
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
}
