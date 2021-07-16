import Exp._

trait ExpSYM[repr] extends Interpreter[repr] {
  def lit(n: Int): repr
  def neg(e: repr): repr
  def add(e1: repr, e2: repr): repr
}

object ExpSYM {

  case class lit(n: Int) extends Exp[ExpSYM] {
    override def apply[R](implicit inst: ExpSYM[R]): R = inst.lit(n)
  }
  case class neg[I[_]](e: Exp[I]) extends Exp[[R] =>> InterpreterPair[ExpSYM, I, R]] {
    override def apply[R](implicit inst: InterpreterPair[ExpSYM, I, R]): R = inst.a.neg(e(inst.b))
  }
  case class add[I1[_], I2[_]](e1: Exp[I1], e2: Exp[I2]) extends Exp[[R] =>> InterpreterTriple[ExpSYM, I1, I2, R]] {
    override def apply[R](implicit inst: InterpreterTriple[ExpSYM, I1, I2, R]): R = inst.a.add(e1(inst.b), e2(inst.c))
  }
//
//  case class lit(a: Int) extends Exp[ExpSYM] {
//    override def apply[R](implicit inst: ExpSYM[R]): R = inst.lit(a)
//  }
//  case class neg[I[_]](e: Exp[I]) extends Exp[IP[ExpSYM, I]] {
//    override def apply[R](implicit inst: IP[ExpSYM, I][R]): R = inst.a.neg(e[R](inst.b))
//  }
//  case class add[I1[_], I2[_]](e1: Exp[I1], e2: Exp[I2]) extends Exp[IT[ExpSYM, I1, I2]] {
//    override def apply[R](implicit inst: IT[ExpSYM, I1, I2][R]): R = inst.a.add(e1(inst.b), e2(inst.c))
//  }

  def push_neg[A[_]](exp: Exp[A]): Exp[_] = exp match {
    case lit(n) => exp
    case neg(lit(n)) => exp
    case neg(neg(e)) => push_neg(e)
    case neg(add(e1, e2)) => add(push_neg(neg(e1)), push_neg(neg(e2)))
    case add(e1, e2) => add(push_neg(e1), push_neg(e2))
  }

  implicit object Eval extends ExpSYM[Int] {
    override def lit(n: Int): Int = n
    override def neg(e: Int): Int = -e
    override def add(e1: Int, e2: Int): Int = e1 + e2
  }

  implicit object View extends ExpSYM[String] {
    override def lit(n: Int): String = n.toString
    override def neg(e: String): String = "-" + e
    override def add(e1: String, e2: String): String = "(" + e1 + " + " + e2 + ")"
  }

//  sealed trait Sign
//  object Pos extends Sign
//  object Neg extends Sign
//
//  class push_neg extends ExpSYM[Sign => ExpRaw] {
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