package PatternMatching

import scala.util.Random

object PatternMatchingBasics extends App {


  /*
  patternMatching uses PM
  takes

  sum(number(1), number(2)) => 1+2
   */
  sealed trait Expr

  case class Number(n: Int) extends Expr

  case class Sum(e1: Expr, e2: Expr) extends Expr

  case class Prod(e1: Expr, e2: Expr) extends Expr

  def printExpression(expr: Expr): Unit = {
    expr match {
      case Number(something) => print(something)
      case Sum(e1, e2) =>
        print("(")
        printExpression(e1)
        print("+")
        printExpression(e2)
        print(")")
      case Prod(e1, e2) =>
        printExpression(e1)
        print("*")
        printExpression(e2)
    }
  }

  def betterShow(expr: Expr): String = {
    expr match {
      case Number(n) => n.toString
      case Sum(e, f) => betterShow(e) + "+" + betterShow(f)
      case Prod(e, f) =>
        def maybeParenthesis(exp: Expr): String = {
          exp match {
            case Number(n) => n.toString
            case Prod(g, h) => betterShow(g) + "*" + betterShow(h)
            case Sum(g, h) =>
              "(" + betterShow(g) + "+" + betterShow(h) + ")"
          }
        }

        maybeParenthesis(e) + "*" + maybeParenthesis(f)

    }
  }

  println(betterShow(Prod(Sum(Number(1), Number(2)), Sum(Number(1), Number(2)))))

}
