sealed trait InterpretationError
class InterpreterNotProvided[A]() extends InterpretationError

trait Interpreter[+R]

object Interpreter {

  sealed trait List[+R]

  object List {

    object Nil extends Interpreter.List[Any]

    case class *:[R, H <: Interpreter[R], T <: Interpreter.List[R]](head: H, tail: T) extends Interpreter.List[R]

    type With[R, H <: Interpreter[R], T <: Interpreter.List[R]] = T match {
      case *:[R, H, t] => *:[R, H, t]
      case *:[R, h, t] => *:[R, h, With[R, H, t]]
      case Nil.type => *:[R, H, Nil.type]
    }

//    extension [R, H <: Interpreter[R], T <: Interpreter.List[R]](interpreters: With[R, H, T])
//      def extract: H = interpreters match {
//        case e: *:[R, H, _] => e.head
//        case e: *:[R, _, With[R, H, t]] => e.tail.extract[R, H, t]
//      }

    def extra[R, H <: Interpreter[R], T <: Interpreter.List[R]](interpreters: With[R, H, T]): H = interpreters match {
      case e: *:[R, H, _] => e.head
      case e: *:[R, _, With[R, H, t]] => extra[R, H, t](e.tail)
    }

    type Concat[R, T1 <: Interpreter.List[R], T2 <: Interpreter.List[R]] <: T1 with T2 = T1 match {
      case *:[R, h, t] => Concat[R, t, With[R, h, T2]]
      case Nil.type => T2
    }

  }
}