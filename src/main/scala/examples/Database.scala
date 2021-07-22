package examples
import scala.concurrent.Future

object Database {

  trait DatabaseOps[A]:
    def fetch(tableID: String): A

  trait ListOps[F[_], In, Out]:
    def select(list: F[In])(f: In => Out): F[Out]

  trait ListFilter[F[_], A]:
    def where(list: F[A])(filter: A => Boolean): F[A]

  val myDatabase = Map(
    "Topics" -> List("Functional IO", "Generische Programmierung / Shapeless", "Final Tagless Interpreters", "Freie Monaden", "Monad Transformer", "Persistent Data Structures, Zippers and Optics", "Algebraic Effects"),
    "RandomWords" -> List("Ausarbeitung", "Form", "Kurs", "Semester", "Titel", "Vergabe"),
    "Reasons JavaScript is better than scala" -> Nil
  )

  def fetch[A](tableID: String)(using i: DatabaseOps[A]): A = i.fetch(tableID)
  extension [F[_], A](list: F[A])
    def select[B](f: A => B)(using i: ListOps[F, A, B]): F[B] = i.select(list)(f)
    def where(filter: A => Boolean)(using i: ListFilter[F, A]): F[A] = i.where(list)(filter)

  def test =

    given DatabaseOps[List[String]] with
      override def fetch(tableID: String): List[String] = myDatabase(tableID)

    given ListFilter[List, String] with
      override def where(list: List[String])(filter: String => Boolean): List[String] = list.filter(filter)

    val allMyTopics = fetch[List[String]]("Topics").where(_ == "Final Tagless Interpreters")
    println("My topics: " + allMyTopics.mkString(", "))




    given DatabaseOps[Future[List[String]]] with
      override def fetch(tableID: String): Future[List[String]] = Future.successful(myDatabase(tableID))

    given ListOps[[A] =>> Future[List[A]], String, Int] with
      given scala.concurrent.ExecutionContext with
        override def execute(runnable: Runnable): Unit = runnable.run()
        override def reportFailure(cause: Throwable): Unit = ???
      override def select(list: Future[List[String]])(f: String => Int): Future[List[Int]] = list.map(_.map(f))

    type FuturisticList[A] = Future[List[A]]
    val wordLengths: Future[List[Int]] = fetch[FuturisticList[String]]("RandomWords").select[Int](_.length)
    println("Random numbers: " + wordLengths.value.get.get.mkString(", "))




    given DatabaseOps[String] with
      override def fetch(tableID: String): String = "Fetch " + tableID

    type ID[A] = A
    given ListOps[ID, String, String] with
      override def select(list: String)(f: String => String): String = f(list + " and select")

    val studentInstruction: String = fetch[ID[String]]("Topics").select[String](_ + " your favorite")
    println(studentInstruction)

}
