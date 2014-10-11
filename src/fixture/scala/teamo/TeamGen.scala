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