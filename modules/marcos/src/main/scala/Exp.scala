import scala.quoted
import scala.reflect
import scala.Tuple._
import scala.reflect.ClassTag

trait Exp[A[_]] {
//  def apply[B](implicit inst: A[B]): B
  protected def apply[B, T <: Tuple](inst: A[B], allInterpreters: T): B

  private type InterpreterAndSomething[B] = [X <: Tuple] =>> A[B] *: X

//  type ResultWith[B, T <: Tuple] = T match {
//    case EmptyTuple => Nothing
//    case InterpreterAndSomething[B][_] => B
//    case _ *: t => ResultWith[B, t]
//  }

  private def applySearch[B, T <: Tuple, AT <: Tuple](interpreters: T, allInterpreters: AT)(implicit classTag: ClassTag[A[B]]): B = interpreters match {
    case _: EmptyTuple => throw new IllegalArgumentException(s"Interpreter of type ${classTag} was not provided.")
    case b *: _: InterpreterAndSomething[B][_] => {
      //val inst: A[B] = b.asInstanceOf[A[B]]
      apply[B, AT](b.asInstanceOf, allInterpreters).asInstanceOf
    }
    case _ *: t => applySearch(t.asInstanceOf, allInterpreters.asInstanceOf).asInstanceOf
  }

  def apply[B, T <: Tuple](interpreters: T)(implicit classTag: ClassTag[A[B]]): B = applySearch[B, T, T](interpreters, interpreters)


  //  inline def apply[B](interpreters: Interpreter[B]*): B = ${ '{apply[B](inst = ${resolveInterpreters('interpreters)})} }

//  private def resolveInterpreters[B](expr: Expr[Seq[Interpreter[B]]])(using target: Type[A[B]], q: Quotes): Expr[A[B]] =
//    expr match {
//      case Varargs(interpreterExprs) => Exp.constructInterpreterTree[A, B](interpreterExprs)
//      case _ => scala.sys.error(s"Explicit arguments exprected. Got: $expr")
//    }

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