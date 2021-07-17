//import ExpSYM.{Pos, Sign}
//
//trait MulSYM[repr] extends Interpreter[repr] {
//  def mul(e1: repr, e2: repr): repr
//}
//
//object MulSYM {
//
//  case class mul(e1: ExpUntyped, e2: ExpUntyped) extends Exp[MulSYM] {
//    override def apply[R](i: MulSYM[R], iOther: Seq[Interpreter[_]]): Either[InterpretationError, R] =
//      e1(iOther).flatMap { r1 => e2(iOther).map{ r2 => i.mul(r1, r2)}}
//  }
//
//  implicit object Eval extends MulSYM[Int] {
//    override def mul(e1: Int, e2: Int): Int = e1 * e2
//  }
//
//  implicit object View extends MulSYM[String] {
//    override def mul(e1: String, e2: String): String = "(" + e1 + " * " + e2 + ")"
//  }
//
//  implicit object PushNeg extends MulSYM[Sign => ExpUntyped] {
//    override def mul(e1: Sign => ExpUntyped, e2: Sign => ExpUntyped): Sign => ExpUntyped =
//      s => MulSYM.mul(e1(s), e2(Pos))
//  }
//
//}