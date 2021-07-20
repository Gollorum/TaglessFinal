package ftt:

  sealed trait ExpRaw:
    type Interpreter[R]

  trait Exp[-A[_]] extends ExpRaw:
    def apply[R](implicit inst: A[R]): R

  trait Gimme1:
    type Input

  trait Gimme2:
    type Input1
    type Input2

  class Interpreter1[+A <: Gimme1, +B[_]](val a: A, val b: B[a.Input])

  class Interpreter2[+A <: Gimme2, +B[_], +C[_]](val a: A, val b: B[a.Input1], val c: C[a.Input2])

  object Exp:
    implicit def pair[A <: Gimme1, B[_]](implicit a: A, b: B[a.Input]): Interpreter1[A, B] = Interpreter1(a, b)
    implicit def pairG[A[_] <: Gimme1, B[_], R](implicit a: A[R], b: B[a.Input]): Interpreter1[A[R], B] = Interpreter1(a, b)
    implicit def triple[A <: Gimme2, B[_], C[_]](implicit a: A, b: B[a.Input1], c: C[a.Input2]): Interpreter2[A, B, C] = Interpreter2(a, b, c)
    implicit def tripleG[A[_] <: Gimme2, B[_], C[_], R](implicit a: A[R], b: B[a.Input1], c: C[a.Input2]): Interpreter2[A[R], B, C] = Interpreter2(a, b, c)
