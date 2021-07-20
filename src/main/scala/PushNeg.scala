object PushNeg:

  enum Ctx:
    case Pos, Neg
    def opposite = this match
      case Pos => Neg
      case Neg => Pos
  end Ctx


  implicit def pushNeg[repr](implicit i: ExpSYM[repr]): ExpSYM[Ctx => repr] = new ExpSYM[Ctx => repr]:
    override def lit(n: Int): Ctx => repr = _ match
      case Ctx.Pos => i.lit(n)
      case Ctx.Neg => i.neg(i.lit(n))
    override def neg(e: Ctx => repr): Ctx => repr = ctx => e(ctx.opposite)
    override def add(e1: Ctx => repr, e2: Ctx => repr): Ctx => repr = ctx => i.add(e1(ctx), e2(ctx))

  import ExpSYM._
  import MulSYM._
  import Serialization.{ErrorMsg, xpS}
  import Duplicate._

  val view: String => String = identity
  implicit def pushNeg[A](f: A => A)(implicit g: Ctx => A): A = f(g(Ctx.Pos))

  def main(args: Array[String]): Unit =
    println(pushNeg(view)(lit(8)))
    println(pushNeg(view)(neg(lit(8))))
    println(pushNeg(view)(neg(neg(lit(8)))))

    val tf1 = view(add(lit(8), neg(add(lit(1), lit(2)))))
    val tf1_pushed = pushNeg(view)(add(lit(8), neg(add(lit(1), lit(2)))))
    println(s"$tf1 = $tf1_pushed")