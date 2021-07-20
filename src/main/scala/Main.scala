import ExpSYM._
import MulSYM._
import Serialization._
import Duplicate._

object Main:

  val eval: Int => Int = identity
  val view: String => String = identity
  val toTree: Tree => Tree = identity

  def main(args: Array[String]): Unit = {

//    val tf1View = view(add(lit(8), neg(add(lit(1), lit(2)))))
//    val tf1Eval = eval(add(lit(8), neg(add(lit(1), lit(2)))))
//
//    val tfm1View: String = add(lit(7), neg(mul(lit(1), lit(2))))
//    val tfm1Eval: Int = add(lit(7), neg(mul(lit(1), lit(2))))
//
//    val tfm2View = view(mul(lit(7), tfm1View))
//    val tfm2Eval = eval(mul(lit(7), tfm1Eval))
//
//    println(tf1View + " = " + tf1Eval)
//    println(tfm1View + " = " + tfm1Eval)
//    println(tfm2View + " = " + tfm2Eval)
//    val serialized: Tree = toTree(add(lit(8), neg(add(lit(1), lit(2)))))
//    println(serialized.show)
//    val tf1_eval = fromTree[(String, Int)](serialized) match {
//      case Left(e) => println("Error: " + e)
//      case Right(x) => println(x._1 + " = " + x._2)
//    }

    Serialization.main(args)
  }
