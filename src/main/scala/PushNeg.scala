import ftt.ExpSYM.{Sign, Opposite}
import ftt.Exp._

package ftt:

  trait PushNeg[I[_]]:

    type PosResultInterpreter[R]
    type NegResultInterpreter[R]

    type PosResult <: Exp[PosResultInterpreter]
    type NegResult <: Exp[NegResultInterpreter]

    type ResultInterpreter[S <: Sign] = [R] =>> S match
      case Sign.Pos.type => PosResultInterpreter[R]
      case Sign.Neg.type => NegResultInterpreter[R]

    type Result[S <: Sign] = S match
      case Sign.Pos.type => PosResult
      case Sign.Neg.type => NegResult

    override def equals(obj: Any): Boolean = obj.getClass == this.getClass


  object PushNeg:

    implicit object Lit extends PushNeg[LitI]:
      override type PosResultInterpreter[R] = LitI[R]
      override type NegResultInterpreter[R] = Interpreter1[NegI[R], LitI]

      override type PosResult = ExpSYM.lit
      override type NegResult = ExpSYM.neg[LitI]


    class PushLit[S <: Sign] extends LitI[S => Lit.Result[S]]:
      override def lit(n: Int): S => Lit.Result[S] = _ match
        case _: Sign.Pos.type => ExpSYM.lit(n)
        case _: Sign.Neg.type => ExpSYM.neg(ExpSYM.lit(n))

    implicit def litP: LitI[Sign.Pos.type => Lit.PosResult] = PushLit[Sign.Pos.type]
    implicit def litN: LitI[Sign.Neg.type => Lit.NegResult] = PushLit[Sign.Neg.type]

    implicit class Neg[I[_]](implicit val pushNeg: PushNeg[I]) extends PushNeg[[R] =>> Interpreter1[NegI[R], I]]:
      override type PosResultInterpreter[R] = pushNeg.NegResultInterpreter[R]
      override type NegResultInterpreter[R] = pushNeg.PosResultInterpreter[R]

      override type PosResult = pushNeg.NegResult
      override type NegResult = pushNeg.PosResult

    implicit def foo[I[_]](implicit pn: PushNeg[I]): PushNegNegPos[pn.NegResult] = new PushNegNegPos[pn.NegResult]
//    implicit def foo2[I[_]](implicit pn: PushNeg[I]): NegI[Sign.Pos.type => pn.NegResult] = foo
    implicit def bar[I[_]](implicit pn: PushNeg[I]): PushNegNegNeg[pn.PosResult] = new PushNegNegNeg[pn.PosResult]
//    implicit def bar2[I[_]](implicit pn: PushNeg[I]): NegI[Sign.Neg.type => pn.PosResult] = bar
//    implicit def baz[S <: Sign, I[_]](implicit pn: PushNeg[I]): PushNegNeg[S, pn.Result[Opposite[S]]] = PushNegNeg[S, pn.Result[Opposite[S]]]
//    implicit def baz2[S <: Sign, I[_]](implicit pn: PushNeg[I]): NegI[S => pn.Result[Opposite[S]]] = baz
    class PushNegNegPos[Result] extends NegI[Sign.Pos.type => Result]:
      override type Input = Sign.Neg.type => Result
      override def neg(e: Input): Sign.Pos.type => Result = s => e(s.opposite)

    class PushNegNegNeg[Result] extends NegI[Sign.Neg.type => Result]:
      override type Input = Sign.Pos.type => Result
      override def neg(e: Input): Sign.Neg.type => Result = s => e(s.opposite)

//
//    class Add[I1[_], I2[_]](val push1: PushNeg[I1], val push2: PushNeg[I2]) extends PushNeg[AddI] {
//      override type PosResultInterpreter[R] = Interpreter2[AddI[R], push1.PosResultInterpreter[S], push2.PosResultInterpreter[S]]
//      override type NegResultInterpreter[R] = Interpreter2[AddI[R], push1.NegResultInterpreter[S], push2.NegResultInterpreter[S]]
//
//      override type PosResult = ExpSYM.add[push1.PosResult, push2.PosResult]
//      override type NegResult = ExpSYM.add[push1.NegResult, push2.NegResult]
//    }
//
//    implicit def addN[I1[_], I2[_]](implicit push1: PushNeg[I1], push2: PushNeg[I2]): Add[I1, I2] = Add[I1, I2](push1, push2)
//
//    class PushAdd[S <: Sign, I1[_], I2[_]](val push1: PushNeg[I1], val push2: PushNeg[I2]) extends AddI[S => ExpSYM.add[push1.ResultInterpreter[S], push2.ResultInterpreter[S]]] {
//      val n: Add[I1, I2] = PushNeg.addN(push1, push2)
//      override type Input1 = S => push1.Result[S]
//      override type Input2 = S => push2.Result[S]
//
//      override def add(e1: Input1, e2: Input2): S => n.Result[S] = s => ExpSYM.add(e1(s), e2(s))
//    }
//
//    implicit def pushAdd[S <: Sign, I1[_], I2[_]](implicit push1: PushNeg[I1], push2: PushNeg[I2]): PushAdd[S, I1, I2] = PushAdd(push1, push2)

    extension (e: ExpSYM.lit)
      def pushNeg: ExpSYM.lit = e
//    extension[I[_]](e: ExpSYM.neg[I]) {
//      def pushNeg(implicit
//                  pn: Neg[I],
//                  pushNeg: PushNeg[I],
//                  i2: I[Sign.Pos.type => pn.PosResult]
//                 ): Sign.Pos.type => pn.PosResult
//      = e[Sign.Pos.type => pn.PosResult](Interpreter1[NegI[Sign.Pos.type => pn.PosResult], I](negN[Sign.Pos.type , I], i2))
//    }

    def explPN[I[_]](e: ExpSYM.neg[I])(implicit
                                       pushNeg: PushNeg[I],
                                       i2: I[Sign.Neg.type => pushNeg.NegResult]
    ): pushNeg.NegResult
    = e(pair[PushNegNegPos[pushNeg.NegResult], I])(Sign.Pos)
    //  extension[I1[_], I2[_]](e: ExpSYM.add[I1, I2]) {
    //    def pushNeg(implicit pn: Add[I1, I2], push1: PushNeg[I1], push2: PushNeg[I2]): pn.Result[Sign.Pos.type] = e/*(pushAdd[Sign.Pos.type, I1, I2])*/(Sign.Pos)
    //  }
    //
    //  def pushNegAddExpl[I1[_], I2[_]](e: ExpSYM.add[I1, I2])(implicit pn: Add[I1, I2], push1: PushNeg[I1], push2: PushNeg[I2]): pn.Result[Sign.Pos.type] = e/*(pushAdd[Sign.Pos.type, I1, I2])*/(Sign.Pos)
