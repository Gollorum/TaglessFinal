package ftt:

  trait MulSYM[R] extends Gimme2:
    def mul(e1: Input1, e2: Input2): R

  object MulSYM:

    case class mul[I1[_], I2[_]](e1: Exp[I1], e2: Exp[I2]) extends Exp[[R] =>> Interpreter2[MulSYM[R], I1, I2]]:
      override def apply[R](implicit inst: Interpreter2[MulSYM[R], I1, I2]): R = inst.a.mul(e1(inst.b), e2(inst.c))

    implicit object Eval extends MulSYM[Int]:
      override type Input1 = Int
      override type Input2 = Int
      override def mul(e1: Int, e2: Int): Int = e1 * e2

    implicit object View extends MulSYM[String]:
      override type Input1 = String
      override type Input2 = String
      override def mul(e1: String, e2: String): String = "(" + e1 + " * " + e2 + ")"