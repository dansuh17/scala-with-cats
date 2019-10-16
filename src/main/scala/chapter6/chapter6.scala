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

// 6.4 Validated
object V {
  import cats.Semigroupal
  import cats.data.Validated
  import cats.instances.list._

  type AllErrorsOr[A] = Validated[List[String], A]

  val inv: AllErrorsOr[(Nothing, Nothing)] = Semigroupal[AllErrorsOr].product(
    Validated.invalid(List("Error 1")),
    Validated.invalid(List("Error 2"))
  )

  val v: Validated.Valid[Int] = Validated.Valid(123)
  val i: Validated.Invalid[List[String]] = Validated.Invalid(List("Badness"))

  import cats.syntax.validated._ // interface syntax
  123.valid[List[String]] // Valid(123)

  List("Badness").invalid[Int] // Invalid(List("Badness"))
}

// 6.4.2 Combining instances of validated
object CombiningValidated {
  import cats.Semigroupal
  import cats.data.Validated
  import cats.instances.string._
  import cats.syntax.apply._
  import cats.syntax.validated._
  import cats.instances.vector._

  type AllErrorsOr[A] = Validated[String, A]

  // accumulating errors
  ("Error1".invalid[Int], "Error2".invalid[Int]).tupled // Invalid("Error1Error2")

  (Vector(404).invalid[Int], Vector(500).invalid[Int]).tupled // Invalid(Vector(404, 500))

  // andThen has same signatures as a monad's flatMap, but does not follow the monad laws
  32.valid.andThen { a =>
    10.valid.map { b =>
      a + b
    }
  }
}

// 6.4.4 Exercise: Form Validation
object FormValidation {
  import cats.data.Validated
  import scala.util.Try
  import cats.syntax.either._
  import cats.Semigroupal
  import cats.syntax.apply._

  case class User(name: String, age: Int)

  type Data = Map[String, String]

  type FailFast[A] = Either[List[String], A]

  def getValue(field: String)(data: Data): FailFast[String] =
    data.get(field).toRight(List(s"invalid field name $field"))

  def parseInt(s: String): FailFast[Int] =
    Either
      .catchOnly[NumberFormatException](s.toInt)
      .leftMap(_ => List(s"$s is not an integer"))

  def nonBlank(s: String): FailFast[String] =
    Right(s).ensure(List("string is blank"))(_.nonEmpty)

  def nonNegative(i: Int): FailFast[Int] =
    Right(i).ensure(List(s"$i is negative"))(_ > 0)

  def readName(data: Data): FailFast[String] =
    for {
      v <- getValue("name")(data)
      k <- nonBlank(v)
    } yield k

  def readAge(data: Data): FailFast[Int] =
    for {
      v <- getValue("age")(data)
      vNonBlank <- nonBlank(v)
      vInt <- parseInt(vNonBlank)
      vNonNeg <- nonNegative(vInt)
    } yield vNonNeg

  type AllErrorOr[A] = Validated[List[String], A]

  // read the user from user-provided form data
  def readUser(data: Data): AllErrorOr[User] =
    (readName(data).toValidated, readAge(formData).toValidated).mapN(User.apply)

  // final usage
  val formData: Data = Map("name" -> "Dan", "age" -> "28")
  val user: Validated[List[String], User] = readUser(formData)
}
