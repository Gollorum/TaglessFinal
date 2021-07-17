import Interpreter.List.With

trait LitI[R] extends Interpreter[R] {
  def lit(n: Int): R
}

trait NegI[R] extends Interpreter[R] {
  def neg(e: R): R
}

trait AddI[R] extends Interpreter[R] {
  def add(e1: R, e2: R): R
}

object ExpSYM {

  case class lit(n: Int) extends Exp.Leaf[LitI] {
    override def apply[Repr](i: LitI[Repr]): Repr = i.lit(n)
  }
  case class neg[T[_] <: Interpreter.List[_]](e: Exp[T]) extends Exp.Node[NegI, T] {
    override def apply[Repr](interpreters: Interpreter.List.With[Repr, NegI[Repr], T[Repr]]): Repr =
      Interpreter.List.extra[Repr, NegI[Repr], T[Repr]](interpreters).neg(e( interpreters))
  }
  case class add[T1[_] <: Interpreter.List[_], T2[_] <: Interpreter.List[_]](e1: Exp[T1], e2: Exp[T2]) extends Exp.Node[AddI, [R] =>> Interpreter.List.Concat[R, T1, T2]] {
    override def apply[Repr](interpreters: Interpreter.List.With[Repr, NegI[Repr], Interpreter.List.Concat[Repr, T1[Repr], T2[Repr]]]): Repr =
      Interpreter.List.extra[Repr, NegI[Repr], Interpreter.List.Concat[Repr, T1[Repr], T2[Repr]]](interpreters).add(e1(interpreters), e2(interpreters))
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

//  sealed trait Sign
//  object Pos extends Sign
//  object Neg extends Sign
//
//  implicit object PushNegLit extends LitI[Sign => ExpUntyped] {
//    override def lit(n: Int): Sign => ExpUntyped = {
//      case Pos => ExpSYM.lit(n)
//      case Neg => ExpSYM.neg(ExpSYM.lit(n))
//    }
//  }
//
//  implicit object PushNegNeg extends NegI[Sign => ExpUntyped] {
//    override def neg(e: Sign => ExpUntyped): Sign => ExpUntyped = {
//      case Pos => e(Neg)
//      case Neg => e(Pos)
//    }
//  }
//
//  implicit object PushNegAdd extends AddI[Sign => ExpUntyped] {
//    override def add(e1: Sign => ExpUntyped, e2: Sign => ExpUntyped): Sign => ExpUntyped = s =>
//      ExpSYM.add(e1(s), e2(s))
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