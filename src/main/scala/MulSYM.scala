trait MulSYM[repr]:
  def mul(e1: repr, e2: repr): repr

object MulSYM:
  def mul[repr](e1: repr, e2: repr)(implicit mulSYM: MulSYM[repr]): repr = mulSYM.mul(e1, e2)

  given MulSYM[String] with
    override def mul(e1: String, e2: String): String = s"($e1 * $e2)"

  given MulSYM[Int] with
    override def mul(e1: Int, e2: Int): Int = e1 * e2

  def test: Unit =
    import ExpSYM._
    def tfm1[repr](using ExpSYM[repr], MulSYM[repr]) = add(lit(7), neg(mul(lit(1), lit(2))))
    def tfm2[repr](using ExpSYM[repr], MulSYM[repr]) = mul(lit(7), tfm1)
    println(tfm1[String] + " = " + tfm1[Int])
    println(tfm2[String] + " = " + tfm2[Int])




  // Serialization
  import Serialization.{Deserializer, ErrorMsg, Leaf, Node, OnInvalid, Tree}

  given MulSYM[Tree] with
    override def mul(e1: Tree, e2: Tree): Tree = Node("Mul", e1, e2)

  class fromTree[A](implicit interpreter: MulSYM[A]) extends Deserializer[A]:
    override def apply(tree: Tree, onInvalid: OnInvalid[A], self: Deserializer[A]): Either[ErrorMsg, A] = tree match
      case Node("Mul", e1, e2) => for {
        e1a <- self(e1, onInvalid)
        e2a <- self(e2, onInvalid)
      } yield interpreter.mul(e1a, e2a)
      case e => onInvalid(e)




  // PushNeg
  import PushNeg.Ctx

  implicit def pushNeg[repr](implicit i: MulSYM[repr]): MulSYM[Ctx => repr] = new MulSYM[Ctx => repr]:
    override def mul(e1: Ctx => repr, e2: Ctx => repr): Ctx => repr = 
      ctx => i.mul(e1(ctx), e2(Ctx.Pos))