package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  test("eval simple Plus expression") {
    val a = Literal(2)
    val b = Literal(3)
    val sum = Plus(a, b)

    assert(Calculator.eval(sum, Map()) == 5)
  }

  test("eval Plus expression with another expression") {
    val a = Literal(2)
    val b = Literal(3)
    val c = Literal(4)
    val expr1 = Plus(a, b)
    val sum = Plus(expr1, c)

    assert(Calculator.eval(sum, Map()) == 9)
  }
}
