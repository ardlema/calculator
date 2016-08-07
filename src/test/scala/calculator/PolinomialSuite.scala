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

  test("the computesolutions function should return 0 when delta is negative") {
    val a = 2
    val b = 2
    val c = 1
    val delta = -4

    val result = Polynomial.computeSolutions(Var(a), Var(b), Var(c), Var(delta))
    assert(result().size == 0)
  }

  test("the computesolutions function should return two results when delta is greater than 0") {
    val a = 1
    val b = 2
    val c = 1
    val delta = 9

    val result = Polynomial.computeSolutions(Var(a), Var(b), Var(c), Var(delta))
    assert(result().size == 2)
    assert(result().contains(0.5))
    assert(result().contains(-2.5))
  }

}
