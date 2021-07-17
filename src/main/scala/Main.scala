import ExpSYM._
import MulSYM._
import Exp._

import scala.language.implicitConversions

object Main {

  def main(args: Array[String]): Unit = {
    val tf1 = add(lit(8), neg(add(lit(1), lit(2))))
    val tfm1 = add(lit(7), neg(mul(lit(1), lit(2))))
//    val foo: List[String] = MyObj.foobar("Foo")
//    println(foo.mkString(", "))
//    AppleTree.apple[String]
    println(tf1(Seq[Interpreter[_]](ViewLit, ViewNeg, ViewAdd)) /*+ " = " + push_neg(tf1)[String] */+ " = " + tf1(Seq(EvalLit, EvalNeg, EvalAdd)).toString)
    println(tfm1(Seq(ViewLit, ViewNeg, ViewAdd, MulSYM.View)) /*+ " = " + push_neg(tfm1)[String]*/ + " = " + tfm1(Seq(EvalLit, EvalNeg, EvalAdd, MulSYM.Eval)).toString)
//    println(tf1[String])
  }

}