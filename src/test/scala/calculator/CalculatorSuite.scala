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

    val a2 = Literal(2)
    val b2 = Literal(3)
    val c2 = Literal(4)
    val expr2 = Plus(b2, c2)
    val sum2 = Plus(a2, expr2)

    assert(Calculator.eval(sum2, Map()) == 9)

    val expr3 = Plus(sum, sum2)
    assert(Calculator.eval(expr3, Map()) == 18)
  }

  test("eval Plus expression with references") {
    val refB = Ref("b")
    val a = Literal(1)
    val expr = Plus(a, refB)
    val signal = Signal[Expr](Literal(4))
    val references = Map(("b", signal))

    assert(Calculator.eval(expr, references) == 5)

    val expr2 = Plus(refB, a)

    assert(Calculator.eval(expr2, references) == 5)

    val refC = Ref("c")
    val signal2 = Signal[Expr](Literal(3))
    val references2 = Map(("b", signal), ("c", signal2))
    val expr3 = Plus(refB, refC)
    assert(Calculator.eval(expr3, references2) == 7)
  }

  test("compute simple values") {
    val signalExpression = Signal[Expr] { Literal(1) }
    val expressions = Map(("a", signalExpression))
    val signalDouble = Signal[Double](1)
    val expectedMap = Map(("a", signalDouble))
    assert(Calculator.computeValues(expressions) == expectedMap)
  }
}
