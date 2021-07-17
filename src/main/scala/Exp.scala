import scala.reflect.ClassTag

sealed trait Exp[T[_] <: Interpreter.List[_]] {
  def apply[Repr](interpreters: T[Repr]): Repr
}

object Exp {

  trait Leaf[I[_] <: Interpreter[_]] extends Exp[I] {}

  trait Node[I[_] <: Interpreter[_], T[_] <: Interpreter.List[_]] extends Exp[[Repr] =>> Interpreter.List.With[Repr, I[Repr], T[Repr]]] {}

}