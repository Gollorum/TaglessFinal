import ftt.ExpSYM._
import ftt.MulSYM._
import ftt.Exp._
import ftt.{Interpreter1, LitI, NegI, PushNeg}
import ftt.PushNeg._
import ftt._

import scala.collection.immutable.LinearSeq

object Main:

  trait Foo:
    type Baar
    def value: Baar

  object Foo1 extends Foo:
    override type Baar = String
    override def value: String = "???"

  object Foo2 extends Foo:
    override type Baar = Int
    override def value: Int = 0

  def get(x: Foo): List[Any => x.Baar] = (_ => x.value) :: Nil
  def getget(x: Foo): LinearSeq[Any => x.Baar] = get(x)

  def main(args: Array[String]): Unit = {
//      val tf1 = add(lit(8), neg(add(lit(1), lit(2))))
//      val tfm1 = add(lit(7), neg(mul(lit(1), lit(2))))
//    val foo: ftt.NegI[ftt.ExpSYM.Sign.Neg.type => ftt.ExpSYM.neg[LitI]] = bar2[[R] =>> Interpreter1[NegI[R], LitI]](Neg[LitI](Lit))
//    val foo = implicitly[ftt.NegI[ftt.ExpSYM.Sign.Neg.type => pushNeg.PosResult]](bar2(pushNeg))
    val pn: Neg[LitI] = Neg
    val tf1Pushed: pn.NegResult = explPN(neg(neg(lit(8))))(pn, implicitly)
//    val tf1Pushed = explPN(neg(lit(8)))
    println(tf1Pushed[String](ViewLit))
//      println(tf1[String] /*+ " = " + push_neg(tf1)[String] */ + " = " + tf1[Int].toString)
//      println(tfm1[String] /*+ " = " + push_neg(tfm1)[String]*/ + " = " + tfm1[Int].toString)
  }
