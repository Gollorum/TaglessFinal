trait ExpSYM[repr]:
  def lit(n: Int): repr
  def neg(e: repr): repr
  def add(e1: repr, e2: repr): repr

object ExpSYM:
  def lit[repr](n: Int)(implicit expSYM: ExpSYM[repr]): repr = expSYM.lit(n)
  def neg[repr](e: repr)(implicit expSYM: ExpSYM[repr]): repr = expSYM.neg(e)
  def add[repr](e1: repr, e2: repr)(implicit expSYM: ExpSYM[repr]): repr = expSYM.add(e1, e2)

  given ExpSYM[String] with
    override def lit(n: Int): String = n.toString
    override def neg(e: String): String = s"-$e"
    override def add(e1: String, e2: String): String = s"($e1 + $e2)"

  given ExpSYM[Int] with
    override def lit(n: Int): Int = n
    override def neg(e: Int): Int = -e
    override def add(e1: Int, e2: Int): Int = e1 + e2
