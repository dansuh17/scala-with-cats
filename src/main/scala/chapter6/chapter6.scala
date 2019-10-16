package chapter6

object chapter6

// Semigroupal = combining contexts

// 6.1.1
object JoiningTwoContexts {
  import cats.Semigroupal
  import cats.instances.option._
  import cats.syntax.apply.catsSyntaxTuple3Semigroupal

  Semigroupal[Option].product(Some(123), Some("abc")) // Option[(Int, String)]

  Semigroupal.tuple3(Option(1), Option(2), Option(3))

  Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _) // Some(6)

  // implicitly uses catsSyntaxTuple3Semigroupal
  (Option(123), Option("abc"), Option(true)).tupled

  case class Cat(name: String, born: Int, color: String)

  // uses Semigroupal to extract values from Options
  // and uses Functor to apply the values to function using mapN
  (Option("Garfield"), Option(1978), Option("Orange & Black")).mapN(Cat.apply)
}

object FancyFunctors {
  import cats.Monoid
  import cats.instances.int._
  import cats.instances.invariant._
  import cats.instances.list._
  import cats.instances.string._
  import cats.syntax.apply._

  case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])

  val tupleToCat: (String, Int, List[String]) => Cat = Cat.apply
  val catToTuple: Cat => (String, Int, List[String]) = cat =>
    (cat.name, cat.yearOfBirth, cat.favoriteFoods)

  implicit val catMonoid: Monoid[Cat] =
    (Monoid[String], Monoid[Int], Monoid[List[String]])
      .imapN(tupleToCat)(catToTuple)
}

// 6.3
object SemigroupalToDifferentTypes {
  import cats.Semigroupal
  import cats.instances.future._
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.language.higherKinds

  val futurePair: Future[(String, Int)] =
    Semigroupal[Future].product(Future("Hello"), Future(123))

  val res1: (String, Int) = Await.result(futurePair, 1.second)

  import cats.syntax.apply._

  case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])

  val futureCat: Future[Cat] =
    (Future("Garfield"), Future(1978), Future(List("Lasagne"))).mapN(Cat.apply)

  Await.result(futureCat, 1.second)

  import cats.Semigroupal
  import cats.instances.list._

  // product of list semigroupals result in "cartesian product" of elements
  Semigroupal[List]
    .product(List(1, 2), List(3, 4)) // List((1, 3), (1, 4), (2, 3), (2, 4))

  import cats.instances.either._

  type ErrorOr[A] = Either[Vector[String], A]

  // fail-fast behaviour
  Semigroupal[ErrorOr].product(Left(Vector("Error 1")), Left(Vector("Error 2")))

  val a = Future("Future 1")
  val b = Future("Future 2")

  for {
    x <- a
    y <- b
  } yield (x, y)

  import cats.Monad
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  // product of Monads
  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    x.flatMap(a => y.map(b => (a, b)))

  import cats.instances.list._

  // results in Cartesian Product due to behavior defined by flatMap
  product(List(1, 2), List(3, 4))

  product[ErrorOr, Int, Int](Left(Vector("Error 1")), Left(Vector("Error 2")))
}
