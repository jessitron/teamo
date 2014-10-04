package teamo;

import org.scalacheck.Gen
import org.scalacheck.Gen._

object TeamGen {

  def apply(): Gen[Team] = {
    const(new Team)
  }
}
