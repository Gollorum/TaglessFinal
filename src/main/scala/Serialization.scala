import scala.util.Try

object Serialization:

  case class Tree(s: String, t: Tree*)

  given xpS: ExpSYM[Tree] with
    override def lit(n: Int): Tree = Tree("Lit", Tree(n.toString))
    override def neg(e: Tree): Tree = Tree("Neg", e)
    override def add(e1: Tree, e2: Tree): Tree = Tree("Add", e1, e2)

  enum Error:
    case Unknown(t: Tree)
    case Invalid(msg: String)

  private def readInt(s: String): Either[Error, Int] = Try(s.toInt).toEither.left.map(e => Error.Invalid("Read error: " + e.toString))

  trait Deserializer[A[_]]:

    def apply(
      tree: Tree,
      d: Deserializer[A] = this
    ): Either[Error, [repr] => A[repr] => repr] = this.apply[A](tree, d, [repr] => (a: A[repr]) => a)

    def apply[B[_]](
      tree: Tree,
      d: Deserializer[B],
      f: [repr] => B[repr] => A[repr]
    ): Either[Error, [repr] => B[repr] => repr]

  extension [A[_]](left: Deserializer[A])
    def ::[B[_]](right: Deserializer[B]): Deserializer[[repr] =>> (A[repr], B[repr])] = new Deserializer[[repr] =>> (A[repr], B[repr])]:
      type C[repr] = (A[repr], B[repr])
      override def apply[D[_]](
        tree: Tree,
        d: Deserializer[D],
        f: [repr] => D[repr] => C[repr]
      ): Either[Error, [repr] => D[repr] => repr] =
        left.apply(tree, d, [repr] => (d: D[repr]) => f(d)._1).left.flatMap(_ match
          case Error.Unknown(t) => right.apply(tree, d, [repr] => (d: D[repr]) => f(d)._2)
          case x => Left(x)
        )

  given expSymDeserializer: Deserializer[ExpSYM] with
    def apply[B[_]](
      tree: Tree,
      self: Deserializer[B],
      f: [repr] => B[repr] => ExpSYM[repr]
    ): Either[Error, [repr] => B[repr] => repr] = tree match
      case Tree("Lit", Tree(n)) => readInt(n).map(i => [repr] => (b: B[repr]) => f(b).lit(i))
      case Tree("Neg", e) => self(e).map(exp => [repr] => (b: B[repr]) => f(b).neg(exp(b)))
      case Tree("Add", e1, e2) => for {
        e1a <- self(e1)
        e2a <- self(e2)
      } yield [repr] => (b: B[repr]) => f(b).add(e1a(b), e2a(b))
      case e => Left(Error.Unknown(e))



  def test =
    import ExpSYM.{add, lit, neg}

    val toTree: Tree => Tree = identity
    val view: String => String = identity

    val tf1Tree = toTree(add(lit(8), neg(add(lit(1), lit(2)))))
    val fromTree = expSymDeserializer
    val tf1 = fromTree(tf1Tree)
    println(tf1Tree)
    println(tf1 match {
      case Left(e) => e
      case Right(e) => e(ExpSYM.given_ExpSYM_String)
    })

    import MulSYM.mul

    val tfm1Tree = toTree(add(lit(1), neg(mul(lit(1), lit(2)))))
    println(fromTree(tfm1Tree) match {
      case Left(e) => e
      case Right(e) => e(ExpSYM.given_ExpSYM_String)
    })
    val msD = MulSYM.fromTree
    println((msD :: fromTree)(tfm1Tree) match {
      case Left(e) => e
      case Right(e) => e((MulSYM.given_MulSYM_String, ExpSYM.given_ExpSYM_String))
    })
