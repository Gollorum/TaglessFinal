trait ExpSYM[repr]:
  def lit(n: Int): repr
  def neg(e: repr): repr
  def add(e1: repr, e2: repr): repr

object ExpSYM:
  def lit[repr](n: Int)(using expSYM: ExpSYM[repr]): repr = expSYM.lit(n)
  def neg[repr](e: repr)(using expSYM: ExpSYM[repr]): repr = expSYM.neg(e)
  def add[repr](e1: repr, e2: repr)(using expSYM: ExpSYM[repr]): repr = expSYM.add(e1, e2)

  given ExpSYM[String] with
    override def lit(n: Int): String = n.toString
    override def neg(e: String): String = s"-$e"
    override def add(e1: String, e2: String): String = s"($e1 + $e2)"

  given ExpSYM[Int] with
    override def lit(n: Int): Int = n
    override def neg(e: Int): Int = -e
    override def add(e1: Int, e2: Int): Int = e1 + e2

  def test: Unit =

    def eval[F[_]](using interpreter: F[Int]): ([repr] => F[repr] => repr) => Int = exp => exp(interpreter)
    def view[F[_]](using interpreter: F[String]): ([repr] => F[repr] => repr) => String = exp => exp(interpreter)

    val tf1 = [repr] => (expSYM: ExpSYM[repr]) =>
      given ExpSYM[repr] = expSYM
      add(lit(8), neg(add(lit(1), lit(2))))

    println(view(tf1) + " = " + eval(tf1))