package jcc

import org.scalatest._

class ProblemsTest extends FunSpec with MustMatchers {
  it("should run a test") {
    1 must be(1)
  }
}
