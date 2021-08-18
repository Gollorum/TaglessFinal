import Serialization.Error

trait MulSYM[repr]:
  def mul(e1: repr, e2: repr): repr

object MulSYM:
  def mul[repr](e1: repr, e2: repr)(implicit mulSYM: MulSYM[repr]): repr = mulSYM.mul(e1, e2)

  given MulSYM[String] with
    override def mul(e1: String, e2: String): String = s"($e1 * $e2)"

  given MulSYM[Int] with
    override def mul(e1: Int, e2: Int): Int = e1 * e2

  val isResultEven: Boolean => Boolean = identity
  val view: String => String = identity

  given ExpSYM[Boolean] with
    def lit(n: Int): Boolean = (n & 1) == 0
    def neg(e: Boolean): Boolean = e
    def add(e1: Boolean, e2: Boolean): Boolean = e1 == e2

  given MulSYM[Boolean] with
    def mul(e1: Boolean, e2: Boolean): Boolean = e1 || e2

  def test: Unit =
    import ExpSYM._
//    def tfm1[repr](using ExpSYM[repr], MulSYM[repr]) = add(lit(7), neg(mul(lit(1), lit(2))))
//    def tfm2[repr](using ExpSYM[repr], MulSYM[repr]) = mul(lit(7), tfm1)
//    println(tfm2[String] + " = " + tfm2[Int])

//    case class Interpreters[A[_], B[_], repr](a: A[repr], b: B[repr])
//
//    implicit def combine[A[_], B[_], repr](using a: A[repr], b: B[repr]): Interpreters[A, B, repr] = Interpreters(a, b)
//
//    def eval[F[_]](using interpreter: F[Int]): ([repr] => F[repr] => repr) => Int = exp => exp(interpreter)
//    def view[F[_]](using interpreter: F[String]): ([repr] => F[repr] => repr) => String = exp => exp(interpreter)
//
//    val tfm1 = [repr] => (i: Interpreters[ExpSYM, MulSYM, repr]) =>
//      given ExpSYM[repr] = i.a
//      given MulSYM[repr] = i.b
//      add(lit(7), neg(mul(lit(1), lit(2))))
//
//    println(view(tfm1) + " = " + eval(tfm1))




  // Serialization
  import Serialization.{Deserializer, Error, Tree}

  given MulSYM[Tree] with
    override def mul(e1: Tree, e2: Tree): Tree = Tree("Mul", e1, e2)

  given fromTree: Deserializer[MulSYM] with
    def apply[B[_]](
      tree: Tree,
      self: Deserializer[B],
      f: [repr] => B[repr] => MulSYM[repr]
    ): Either[Error, [repr] => B[repr] => repr] = tree match
      case Tree("Mul", e1, e2) => for {
        e1a <- self(e1)
        e2a <- self(e2)
      } yield [repr] => (b: B[repr]) => f(b).mul(e1a(b), e2a(b))
      case e => Left(Error.Unknown(e))




  // PushNeg
  import PushNeg.Ctx

  implicit def pushNeg[repr](implicit i: MulSYM[repr]): MulSYM[Ctx => repr] = new MulSYM[Ctx => repr]:
    override def mul(e1: Ctx => repr, e2: Ctx => repr): Ctx => repr = 
      ctx => i.mul(e1(ctx), e2(Ctx.Pos))