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
    namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    for {
      (k, v) <- namedExpressions
    } yield {
      (k, Signal(eval(getReferenceExpr(k, namedExpressions), namedExpressions, Set())))
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]], seen: Set[Expr]): Double = {
    if (seen.contains(expr)) Double.NaN
    else
      expr match {
        case Literal(v) => v
        case Ref(name) => eval(getReferenceExpr(name, references), references, seen + expr)
        case Plus(a, b) => eval(a, references, seen + expr) + eval(b, references, seen + expr)
        case Minus(a, b) => eval(a, references, seen + expr) - eval(b, references, seen + expr)
        case Times(a, b) => eval(a, references, seen + expr) * eval(b, references, seen + expr)
        case Divide(a, b) => eval(a, references, seen + expr) / eval(b, references, seen + expr)
      }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
