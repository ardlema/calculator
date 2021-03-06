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

  test("eval simple Minus expression") {
    val a = Literal(5)
    val b = Literal(3)
    val minus = Minus(a, b)

    assert(Calculator.eval(minus, Map()) == 2)
  }

  test("eval simple Times expression") {
    val a = Literal(2)
    val b = Literal(3)
    val times = Times(a, b)

    assert(Calculator.eval(times, Map()) == 6)
  }

  test("eval simple Divide expression") {
    val a = Literal(9)
    val b = Literal(3)
    val divide = Divide(a, b)

    assert(Calculator.eval(divide, Map()) == 3)
  }

  test("eval simple Ref expression") {
    val ref = Ref("a")
    val signal = Signal[Expr](Literal(4))
    val references = Map(("a", signal))

    assert(Calculator.eval(ref, references) == 4)
  }

  test("eval simple cyclic dependencies") {
    val ref = Ref("a")
    val signal = Signal[Expr](Ref("a"))
    val references = Map(("a", signal))

    assert(Calculator.eval(ref, references).isNaN)
  }

  test("compute simple values") {
    val signalExpression = Signal[Expr] { Literal(1) }
    val expressions = Map(("a", signalExpression))
    val resultMap = Calculator.computeValues(expressions)

    assert(resultMap.size == 1)
    assert(resultMap.get("a").get() == 1)
  }

  test("compute values with references values") {
    val literal1 = Literal(1)
    val literal2 = Literal(2)
    val aExpression = Signal[Expr] { literal1 }
    val bExpression = Signal[Expr] { literal2 }
    val cExpression = Signal[Expr] { Plus(literal1, literal2) }
    val expressions = Map(("a", aExpression), ("b", bExpression), ("c", cExpression))
    val resultMap = Calculator.computeValues(expressions)

    assert(resultMap.size == 3)
    assert(resultMap.get("a").get() == 1)
    assert(resultMap.get("b").get() == 2)
    assert(resultMap.get("c").get() == 3)
  }
}
