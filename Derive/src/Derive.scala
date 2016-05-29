

/**
  * Created by Bruno on 5/29/2016.
  */
object Deriver {

  def Derive(t: Tree, n: String): Tree = {
    t match{
      case Diff(l, r) => Diff(Derive(l, n), Derive(r, n))
      case Sum(l, r)  => Sum(Derive(l, n), Derive(r, n))
      case Mul(l, r)  => Sum(Mul(Derive(l, n), r),Mul(l, Derive(r, n)))
      case Div(l, r)  => Div(Diff(Mul(Derive(l, n), r),Mul(l, Derive(r, n))), Pow(r, 2))
      case Pow(l, exp)  => Mul(Mul(Const(exp - 1), l), Derive(l, n))
      case Var(v) if (v == n) => Const(1)
      case _ => Const(0)
    }
  }
}
