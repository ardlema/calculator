package calculator

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, _}

@RunWith(classOf[JUnitRunner])
class PolinomialSuite extends FunSuite with ShouldMatchers {

  test("the computedelta function should return the proper value") {
    val a = 1
    val b = 2
    val c = 2
    val expectedValue = -4

    val result = Polynomial.computeDelta(Var(a), Var(b), Var(c))
    assert(result() == expectedValue)
  }
}
