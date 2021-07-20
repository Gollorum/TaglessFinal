import PushNeg.Ctx
import Serialization.{Deserializer, ErrorMsg, Leaf, Node, OnInvalid, Tree}

trait MulSYM[repr]:
  def mul(e1: repr, e2: repr): repr

object MulSYM:
  def mul[repr](e1: repr, e2: repr)(implicit mulSYM: MulSYM[repr]): repr = mulSYM.mul(e1, e2)

  given MulSYM[String] with
    override def mul(e1: String, e2: String): String = s"($e1 * $e2)"

  given MulSYM[Int] with
    override def mul(e1: Int, e2: Int): Int = e1 * e2

  given MulSYM[Tree] with
    override def mul(e1: Tree, e2: Tree): Tree = Node("Mul", e1, e2)

  class fromTree[A](implicit interpreter: MulSYM[A]) extends Deserializer[A]:
    override def apply(tree: Tree, onInvalid: OnInvalid[A], self: Deserializer[A]): Either[ErrorMsg, A] = tree match
      case Node("Mul", e1, e2) => for {
        e1a <- self(e1, onInvalid)
        e2a <- self(e2, onInvalid)
      } yield interpreter.mul(e1a, e2a)
      case e => onInvalid(e)

  implicit def pushNeg[repr](implicit i: MulSYM[repr]): MulSYM[Ctx => repr] = new MulSYM[Ctx => repr]:
    override def mul(e1: Ctx => repr, e2: Ctx => repr): Ctx => repr = ctx => i.mul(e1(ctx), e2(Ctx.Pos))