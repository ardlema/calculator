package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    for (expression <- namedExpressions) yield (expression._1, Signal {
      eval(expression._2(), namedExpressions)
    })


  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(l) => l
      case Plus(a, b) => {
        (a, b) match {
          case (Literal(n1), Literal(n2)) => n1 + n2
          case (Literal(n1), Ref(variable)) => n1 + eval(getReferenceExpr(variable, references), references)
          case (Ref(variable), Literal(n1)) => eval(getReferenceExpr(variable, references), references) + n1
          case (Ref(variable1), Ref(variable2)) =>
            eval(getReferenceExpr(variable1, references), references) +
              eval(getReferenceExpr(variable2, references), references)
          case (Literal(n1), expr2) => n1 + eval(expr2, references)
          case (expr1, Literal(n2)) => eval(expr1, references) + n2
          case (expr1, expr2) => eval(expr1, references) + eval(expr2, references)
        }
      }
      case _ => 0
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
