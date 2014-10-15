package teamo

import org.scalacheck.Gen
import org.scalacheck.Gen._

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