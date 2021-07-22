object Initial:
  enum Exp:
    case Lit(n: Int)
    case Neg(e: Exp)
    case Add(e1: Exp, e2: Exp)

  import Exp._

  val tf1: Exp = Add(Lit(8), Neg(Add(Lit(1), Lit(2))))

  def eval(e: Exp): Int = e match
    case Lit(n) => n
    case Neg(e) => -eval(e)
    case Add(e1, e2) => eval(e1) + eval(e2)

  def view(e: Exp): String = e match
    case Lit(n) => n.toString
    case Neg(e) => "-" + view(e)
    case Add(e1, e2) => s"(${view(e1)} + ${view(e2)})"

  def test: Unit =
    println(view(tf1) + " = " + eval(tf1))


object Final:
  type Repr = Int

  val lit: Int => Repr = n => n

  val neg: Repr => Repr = -_

  val add: (Repr, Repr) => Repr = _ + _

  val tf1: Int = add(lit(8), neg(add(lit(1), lit(2)))) // = 5