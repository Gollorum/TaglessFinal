trait MulSYM[repr] extends Interpreter[repr] {
  def mul(e1: repr, e2: repr): repr
}

object MulSYM {

  case class mul[I1[_], I2[_]](e1: Exp[I1], e2: Exp[I2]) extends Exp[[R] =>> InterpreterTriple[MulSYM, I1, I2, R]] {
    override def apply[R](implicit inst: InterpreterTriple[MulSYM, I1, I2, R]): R = inst.a.mul(e1(inst.b), e2(inst.c))
  }
//
//  def mul[I1[_], I2[_]](e1: Exp[I1], e2: Exp[I2]): Exp[[R] =>> InterpreterTriple[I1, I2, MulSYM, R]] = new Exp {
//    override def apply[A](implicit inst: InterpreterTriple[I1, I2, MulSYM, A]): A = inst.c.mul(e1(inst.a), e2(inst.b))
//  }

  implicit object Eval extends MulSYM[Int] {
    override def mul(e1: Int, e2: Int): Int = e1 * e2
  }

  implicit object View extends MulSYM[String] {
    override def mul(e1: String, e2: String): String = "(" + e1 + " * " + e2 + ")"
  }

}