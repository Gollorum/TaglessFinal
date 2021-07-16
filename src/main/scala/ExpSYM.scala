import Exp._

trait LitI[R] {
  def lit(n: Int): R
}

trait NegI[R] {
  def neg(e: R): R
}

trait AddI[R] {
  def add(e1: R, e2: R): R
}

object ExpSYM {

  case class lit(a: Int) extends Exp[LitI] {
    override def apply[R](implicit inst: LitI[R]): R = inst.lit(a)
  }
  case class neg[I[_]](e: Exp[I]) extends Exp[IP[NegI, I]] {
    override def apply[R](implicit inst: IP[NegI, I][R]): R = inst.a.neg(e[R](inst.b))
  }
  case class add[I1[_], I2[_]](e1: Exp[I1], e2: Exp[I2]) extends Exp[IT[AddI, I1, I2]] {
    override def apply[R](implicit inst: IT[AddI, I1, I2][R]): R = inst.a.add(e1(inst.b), e2(inst.c))
  }

  implicit object EvalLit extends LitI[Int] {
    override def lit(n: Int): Int = n
  }

  implicit object EvalNeg extends NegI[Int] {
    override def neg(e: Int): Int = -e
  }

  implicit object EvalAdd extends AddI[Int] {
    override def add(e1: Int, e2: Int): Int = e1 + e2
  }

  implicit object ViewLit extends LitI[String] {
    override def lit(n: Int): String = n.toString
  }

  implicit object ViewNeg extends NegI[String] {
    override def neg(e: String): String = s"-$e"
  }

  implicit object ViewAdd extends AddI[String] {
    override def add(e1: String, e2: String): String = s"($e1 + $e2)"
  }

  sealed trait Sign
  object Sign {
    object Pos extends Sign
    object Neg extends Sign
  }

  type Opposite[S <: Sign] = S match {
    case Sign.Pos.type => Sign.Neg.type
    case Sign.Neg.type => Sign.Pos.type
  }

//  type PushNeg[S <: Sign, E <: ExpRaw] = E match {
//    case lit => S match {
//      case Sign.Pos.type => lit
//      case Sign.Neg.type => neg[ExpSYM]
//    }
//    case neg[] =>
//  }
//
//  class push_neg[I[_]](e: Exp[I]) extends ExpSYM[Sign => ExpRaw] {
//
//  }






//
//    override def lit(n: Int): Sign => ExpRaw = _ match {
//      case _: Pos.type => ExpSYM.lit(n)
//      case _: Neg.type => ExpSYM.neg(ExpSYM.lit(n))
//    }
//
//    override def neg(e: Sign => ExpRaw): Sign => ExpRaw = _ match {
//      case _: Pos.type => e(Neg)
//      case _: Neg.type => e(Pos)
//    }
//
//    override def add(e1: Sign => ExpRaw, e2: Sign => ExpRaw): Sign => ExpRaw = sign => sign match {
//        case Pos => ExpSYM.add(explicitly(e1(Pos)), e2(Pos))
//        case Neg => ExpSYM.add(e1(Neg), e2(Neg))
//    }
//  }

}