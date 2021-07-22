import ExpSYM._
import MulSYM._
import Serialization.{Tree, ErrorMsg, xpS}

object Duplicate:

  implicit def dupl[repr, repr2](
    implicit i1: ExpSYM[repr], i2: ExpSYM[repr2]
  ): ExpSYM[(repr, repr2)] = new ExpSYM[(repr, repr2)]:
    override def lit(n: Int): (repr, repr2) =
      (i1.lit(n), i2.lit(n))
    override def neg(e: (repr, repr2)): (repr, repr2) =
      (i1.neg(e._1), i2.neg(e._2))
    override def add(e1: (repr, repr2), e2: (repr, repr2)): (repr, repr2) =
      (i1.add(e1._1, e2._1), i2.add(e1._2, e2._2))

  def test =
    def check_consume[A, B](f: A => B)(e: Either[ErrorMsg, A]) = e match
      case Left(err) => println(err)
      case Right(x) => f(x)

    def thrice(e: (Int, (String, Tree))): Int =
      println(e._1)
      println(e._2._1)
      println(e._2._2)
      e._1

    val tf1_tree: Tree = add(lit(8), neg(add(lit(1), lit(2))))
    val fromTree = Serialization.expSymDeserializer[(Int, (String, Tree))]
    val tf13 = check_consume(thrice)(fromTree(tf1_tree))