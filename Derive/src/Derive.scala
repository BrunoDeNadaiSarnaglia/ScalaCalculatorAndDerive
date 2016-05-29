

/**
  * Created by Bruno on 5/29/2016.
  */
object Deriver {

  def main(args: Array[String]) {
    var exp: Tree = Sum(Sum(Var("x"),Var("x")),Sum(Const(7),Var("y")))
    println(Derive(exp, "x"))
    println(Derive(exp, "y"))
    exp = Sum(Mul(Var("x"),Var("x")),Sum(Const(7),Var("y")))
    println(Derive(exp, "x"))
    exp = Pow(Var("x"),5)
    println(Derive(exp, "x"))
    exp = Exp(Var("x"))
    println(Derive(exp, "x"))
    exp = Exp(Pow(Var("x"),2))
    println(Derive(exp, "x"))
    exp = Div(Var("x"), Var("x"))
    println(Derive(exp, "x"))
    exp = Div(Pow(Var("x"), 2), Var("x"))
    println(Derive(exp, "x"))
  }

  def Derive(t: Tree, n: String): Tree = {
    t match{
      case Diff(l, r) => Diff(Derive(l, n), Derive(r, n))
      case Sum(l, r)  => Sum(Derive(l, n), Derive(r, n))
      case Mul(l, r)  => Sum(Mul(Derive(l, n), r),Mul(l, Derive(r, n)))
      case Div(l, r)  => Div(Diff(Mul(Derive(l, n), r),Mul(l, Derive(r, n))), Pow(r, 2))
      case Pow(l, exp)  => Mul(Mul(Const(exp - 1), l), Derive(l, n))
      case Exp(l)  =>   Mul(Derive(l, n), Exp(l))
      case Var(v) if (v == n) => Const(1)
      case _ => Const(0)
    }
  }
}
