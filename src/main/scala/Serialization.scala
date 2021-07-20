import scala.util.Try

object Serialization:

  sealed trait Tree:
    def toString: String
  case class Leaf(s: String) extends Tree:
    override def toString: String = s"Leaf $s"
  case class Node(s: String, t: Tree*) extends Tree:
    override def toString: String = s"Node \"$s\" [${t.map(_.toString).mkString(", ")}]"

  given xpS: ExpSYM[Tree] with
    override def lit(n: Int): Tree = Node("Lit", Leaf(n.toString))
    override def neg(e: Tree): Tree = Node("Neg", e)
    override def add(e1: Tree, e2: Tree): Tree = Node("Add", e1, e2)

  type ErrorMsg = String
  private def readInt(s: String): Either[ErrorMsg, Int] = Try(s.toInt).toEither.left.map("Read error: " + _.toString)

  trait Deserializer[A]:
    def apply(
     tree: Tree,
     onInvalid: OnInvalid[A] = tree => Left("Invalid tree: " + tree.toString),
     d: Deserializer[A] = this
    ): Either[ErrorMsg, A]

  extension [A](left: Deserializer[A])
    def ::(right: Deserializer[A]): Deserializer[A] = new Deserializer[A]:
      override def apply(tree: Tree, onInvalid: OnInvalid[A], self: Deserializer[A]): Either[ErrorMsg, A] =
        left.apply(tree, t => right.apply(t, onInvalid, self), self)

  type OnInvalid[A] = Tree => Either[ErrorMsg, A]

  class expSymDeserializer[A](using interpreter: ExpSYM[A]) extends Deserializer[A]:
    override def apply(tree: Tree, onInvalid: OnInvalid[A], self: Deserializer[A]): Either[ErrorMsg, A] = tree match
      case Node("Lit", Leaf(n)) => readInt(n).map(interpreter.lit)
      case Node("Neg", e) => self(e, onInvalid).map(interpreter.neg)
      case Node("Add", e1, e2) => for {
        e1a <- self(e1, onInvalid)
        e2a <- self(e2, onInvalid)
      } yield interpreter.add(e1a, e2a)
      case e => onInvalid(e)

  def main(args: Array[String]): Unit =
    import ExpSYM.{add, lit, neg}
    import MulSYM.mul

    val toTree: Tree => Tree = identity
    val view: String => String = identity

    val tf1Tree = toTree(add(lit(8), neg(add(lit(1), lit(2)))))
    val fromTree = expSymDeserializer[String]
    val tf1 = fromTree(tf1Tree)
    println(tf1Tree)
    println(tf1 match {
      case Left(e) => e
      case Right(e) => e
    })

    val tfm1Tree = toTree(add(lit(1), neg(mul(lit(1), lit(2)))))
    println(tfm1Tree)
    println(fromTree(tfm1Tree) match {
      case Left(e) => e
      case Right(e) => e
    })
    val msD = MulSYM.fromTree[String]
    println((msD :: fromTree)(tfm1Tree) match {
      case Left(e) => e
      case Right(e) => e
    })

    import PushNeg._
    println((msD :: fromTree)(pushNeg(toTree)(add(lit(1), neg(mul(lit(1), lit(2)))))) match {
      case Left(e) => e
      case Right(e) => e
    })