package teamo;

import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop._

class SomeSpec extends FunSuite with GeneratorDrivenPropertyChecks with ShouldMatchers {

  test("something") {
    1 should be(1)
  }

}

