import Environment.Environment

/**
  * Created by Bruno on 5/29/2016.
  */
object Calculator {

  def main(args: Array[String]) {
    var exp: Tree = Sum(Sum(Var("x"),Var("x")),Sum(Const(7),Var("y")))
    var env: Environment = { case "x" => 5 case "y" => 7 }
    println(eval(exp, env));
    exp = Sum(Sum(Const(3),Var("x")),Diff(Const(7),Var("y")))
    env = { case "x" => 5 case "y" => 5 }
    println(eval(exp, env));
    exp = Sum(Mul(Const(3),Var("x")),Diff(Const(7),Var("y")))
    env = { case "x" => 2 case "y" => 5 }
    println(eval(exp, env));
    exp = Sum(Div(Const(3),Var("x")),Diff(Const(7),Var("y")))
    env = { case "x" => 2 case "y" => 5 }
    println(eval(exp, env));
    exp = Sum(Div(Const(3),Var("x")),Diff(Const(7),Var("y")))
    env = { case "x" => 0 case "y" => 5 }
    try{
      println(eval(exp, env))
    } catch {
      case e : Exception => println(e)
    }
  }

  def eval(t: Tree, env: Environment): Int = {
    t match {
      case Sum(l, r)  =>  eval(l, env) + eval(r, env)
      case Diff(l, r) =>  eval(l, env) - eval(r, env)
      case Mul(l, r) =>  eval(l, env) * eval(r, env)
      case Div(l, r) =>   val den = eval(r, env)
                          if(den == 0) sys.error("Divide by zero")
                          eval(l, env) / eval(r, env)
      case Var(n) => env(n)
      case Const(v) => v
      case _        =>   sys.error("Unknown expression")
    }
  }
}
