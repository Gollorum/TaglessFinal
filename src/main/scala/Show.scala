trait Show[A] {
  def show(a: A): String
}

object Show {

  implicit def show[A](a: A)(implicit sh: Show[A]): String = sh.show(a)
  implicit def apply[A](a: A)(implicit sh: Show[A]): String = show(a)

  implicit val showInt: Show[Int] = (a: Int) => s"$a"

}