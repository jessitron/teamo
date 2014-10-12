package teamo;

import org.scalacheck.Gen
import org.scalacheck.Gen._

object TeamGen {

  def apply(): Gen[Team] = {
    const(new Team)
  }
}

object CultureGen{
  def apply(): Gen[Culture] = {
    for(d<-Gen.choose(0.0,2.0)) yield Culture(d)
  }
}

object FeatureGen {
  def apply():Gen[Feature] = {
    for(d<-Gen.choose(1.0,100.0)) yield Feature(d)
  }
}

object FeaturesGen{
  def apply():Gen[Set[Feature]] = {
   Gen.containerOf[Set,Feature](FeatureGen())
  }
}