import ExpSYM._
import MulSYM._
import Exp._

import scala.language.experimental.macros
import scala.language.implicitConversions

object Main {

  def main(args: Array[String]): Unit = {
    val tf1 = add(lit(8), neg(add(lit(1), lit(2))))
    val tfm1 = add(lit(7), neg(mul(lit(1), lit(2))))
//    val foo: List[String] = MyObj.foobar("Foo")
//    println(foo.mkString(", "))
//    AppleTree.apple[String]
    println(tf1[String] /*+ " = " + push_neg(tf1)[String] */+ " = " + tf1[Int].toString)
    println(tfm1[String] /*+ " = " + push_neg(tfm1)[String]*/ + " = " + tfm1[Int].toString)
//    println(tf1[String])
  }

}