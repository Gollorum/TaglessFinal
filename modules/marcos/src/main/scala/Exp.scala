import scala.quoted
import scala.reflect
import scala.Tuple._
import scala.reflect.ClassTag

trait Exp[A[_]] {
  def apply[R](implicit inst: A[R]): R
}

trait Interpreter[A]

case class InterpreterPair[A[_], B[_], R] (a: A[R], b: B[R])
case class InterpreterTriple[A[_], B[_], C[_], R] (a: A[R], b: B[R], c: C[R])

type IP[A[_], B[_]] = [R] =>> InterpreterPair[A, B, R]
type IT[A[_], B[_], C[_]] = [R] =>> InterpreterTriple[A, B, C, R]

object Exp {

  type And[A, T <: Tuple] = T match {
    case EmptyTuple => Tuple1[A]
    case A *: _ => T
    case h *: t => h *: And[A, t]
  }

  type Merge[T1 <: Tuple, T2 <: Tuple] = T1 match {
    case EmptyTuple => T2
    case h *: t => Merge[t, And[h, T2]]
  }

  type +[A[_], T[_] <: Tuple] = [Repr] =>> And[A[Repr], T[Repr]]
  type ++[T1[_] <: Tuple, T2[_] <: Tuple] = [Repr] =>> Merge[T1[Repr], T2[Repr]]

//  def constructInterpreterTree[A[_], B](interpreters: Seq[Expr[Interpreter[B]]])(using target: Type[A[B]], at: Type[A], q: Quotes): Expr[A[B]] = target match {
//    case t: Type[InterpreterPair[a, b, B]] => '{ ${makePair[A, b, B](interpreters)}.asInstanceOf }
//    case t: Type[InterpreterTriple[a, b, c, B]] => '{ triple[a, b, c, B](${constructInterpreterTree[a, B](interpreters)}, ${constructInterpreterTree[b, B](interpreters)}, ${constructInterpreterTree[c, B](interpreters)}).asInstanceOf }
//    case _ => interpreters.find(x => isInstanceOf[A[B]](x)) match {
//      case Some(x) => '{x.asInstanceOf}
//      case None => scala.sys.error(s"No interpreter of type ${target} was found.")
//    }
//  }
//
//  def makePair[A[_], B[_], Repr](interpreters: Seq[Expr[Interpreter[Repr]]])(using Type[A], Type[B], Type[Repr], Quotes): Expr[InterpreterPair[A, B, Repr]] = '{
//    pair[A, B, Repr](
//      ${ constructInterpreterTree[A, Repr](interpreters) },
//      ${ constructInterpreterTree[B, Repr](interpreters) }
//    )
//  }
//
//  inline def isInstanceOf[B](expr: Expr[Any]): Boolean = expr.isInstanceOf[Expr[B]]

  implicit def pair[A[_], B[_], R](implicit a: A[R], b: B[R]): InterpreterPair[A, B, R] = InterpreterPair(a, b)
  implicit def triple[A[_], B[_], C[_], R](implicit a: A[R], b: B[R], c: C[R]): InterpreterTriple[A, B, C, R] = InterpreterTriple(a, b, c)

}