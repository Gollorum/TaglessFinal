package ftt:

  trait LitI[+R]:
    def lit(n: Int): R

  trait NegI[+R] extends Gimme1:
    def neg(e: Input): R

  trait AddI[+R] extends Gimme2:
    def add(e1: Input1, e2: Input2): R

  object ExpSYM:

    case class lit(a: Int) extends Exp[LitI]:
      override def apply[R](implicit inst: LitI[R]): R = inst.lit(a)

    case class neg[I[_]](e: Exp[I]) extends Exp[[R] =>> Interpreter1[NegI[R], I]]:
      override def apply[R](implicit inst: Interpreter1[NegI[R], I]): R = inst.a.neg(e(inst.b))

    case class add[I1[_], I2[_]](e1: Exp[I1], e2: Exp[I2]) extends Exp[[R] =>> Interpreter2[AddI[R], I1, I2]]:
      override def apply[R](implicit inst: Interpreter2[AddI[R], I1, I2]): R = inst.a.add(e1(inst.b), e2(inst.c))

    implicit object EvalLit extends LitI[Int]:
      override def lit(n: Int): Int = n

    implicit object EvalNeg extends NegI[Int]:
      override type Input = Int
      override def neg(e: Int): Int = -e

    implicit object EvalAdd extends AddI[Int]:
      override type Input1 = Int
      override type Input2 = Int
      override def add(e1: Int, e2: Int): Int = e1 + e2

    implicit object ViewLit extends LitI[String]:
      override def lit(n: Int): String = n.toString

    implicit object ViewNeg extends NegI[String]:
      override type Input = String
      override def neg(e: String): String = s"-$e"

    implicit object ViewAdd extends AddI[String]:
      override type Input1 = String
      override type Input2 = String
      override def add(e1: String, e2: String): String = s"($e1 + $e2)"

    sealed trait Sign

    object Sign:
      object Pos extends Sign
      object Neg extends Sign

    type Opposite[S <: Sign] <: Sign = S match
      case Sign.Pos.type => Sign.Neg.type
      case Sign.Neg.type => Sign.Pos.type

    extension[S <: Sign] (s: S)
      def opposite: Opposite[S] = s match
        case _: Sign.Pos.type => Sign.Neg
        case _: Sign.Neg.type => Sign.Pos