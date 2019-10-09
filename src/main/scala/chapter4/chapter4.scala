package chapter4

object chapter4

// 4.1
object MyMonad {
  import scala.language.higherKinds

  trait Monad[F[_]] {
    def pure[A](value: A): F[A] // also called 'empty'
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

    // proof: all monads are functors
    def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)(func andThen pure)
  }

  // left identity
  // pure(a).flatMap(func) == func(a)

  // right identity
  // m.flatMap(pure) == m

  // associativity
  // m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
}

// 4.2
object MonadsInCats {
  import cats.Monad
  import cats.instances.option.catsStdInstancesForOption
  import cats.instances.list.catsStdInstancesForList

  val opt1 = Monad[Option].pure(3)
  // implicitly provided Monad instance for Option
  val opt1Explicit = Monad[Option](catsStdInstancesForOption).pure(3)

  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
  val opt2Explicit =
    Monad[Option](catsStdInstancesForOption).flatMap(opt1)(a => Some(a + 2))

  val opt3 = Monad[Option].map(opt2)(a => 100 * a)

  val list1 = Monad[List].pure(3)
  // implicitly provided Monad instance for List
  val list1Explicit = Monad[List](catsStdInstancesForList).pure(3)

  val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))

  val list3 = Monad[List].map(list2)(a => a + 123)

  import cats.instances.future.catsStdInstancesForFuture
  import scala.concurrent.{Future, Await}
  import scala.concurrent.duration.DurationInt
  import scala.concurrent.ExecutionContext.Implicits.global

  val fm = Monad[Future]
  val fmExplicit = Monad[Future](catsStdInstancesForFuture(global)) // explicit version

  val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))
  Await.result(future, 1.second)

  val durationExplicit = DurationInt(3) // also can be written as : 3.second
}

// 4.2.3
object MonadSyntax {
  import cats.instances.option.catsStdInstancesForOption
  import cats.instances.list.catsStdInstancesForList
  import cats.syntax.applicative.catsSyntaxApplicativeId

  // equivalent
  1.pure[Option]
  catsSyntaxApplicativeId(1).pure(catsStdInstancesForOption)

  // equivalent
  1.pure[List]
  catsSyntaxApplicativeId(1).pure(catsStdInstancesForList)

  // avoiding name conflicts for map and flatMap
  import cats.Monad
  import cats.syntax.functor.toFunctorOps
  import cats.syntax.flatMap.toFlatMapOps
  import scala.language.higherKinds

  // F[_]: Monad syntax?
  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    a.flatMap(x => b.map(y => x * x + y * y))

  sumSquare(Option(3), Option(4))
  sumSquare(Option(3), Option(4))(catsStdInstancesForOption) // explicit

  sumSquare(List(1, 2, 3), List(4, 5)) // 17, 26, 20, 29, 25, 34
  sumSquare(List(1, 2, 3), List(4, 5))(catsStdInstancesForList) // explicit

  // different way to define sumSquare
  def sumSquare2[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y
}

// 4.3
object IdentityMonad {
  import cats.{Id, Monad}
  import MonadSyntax.sumSquare

  sumSquare(3: Id[Int], 4: Id[Int])

  // 4.3.1 Implement Id Monad instance
  val idInstance: Monad[Id] = new Monad[Id] {
    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
    override def pure[A](x: A): Id[A] = x
    override def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)

    // ???
    override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = ???
  }
}

// 4.4 Either Monad
object EitherMonad {
  val either1: Either[String, Int] = Right(10)
  val either2: Either[String, Int] = Right(32)

  val eitherSum: Either[String, Int] = for {
    a <- either1
    b <- either2
  } yield a + b

  import cats.syntax.either._
  val a: Either[String, Int] = 3.asRight[String]
  val aExplicit: Either[String, Int] = catsSyntaxEitherId(3).asRight[String]

  val b: Either[String, Int] = 4.asRight[String]

  val c: Either[String, Int] = for {
    x <- a
    y <- b
  } yield x * x + y * y

  def countPositive(nums: List[Int]): Either[String, Int] =
    nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
      // using smart constructor
      if (num > 0) {
        accumulator.map(_ + 1)
      } else {
        Left("Negative, Stopping!")
      }
    }

  countPositive(List(1, 2, 3))
  countPositive(List(1, -2, 3))

  // Either helper functions
  Either.catchOnly[NumberFormatException]("foo".toInt)
  Either.catchNonFatal(sys.error("Badnass"))
  Either.fromTry(scala.util.Try("foo".toInt))
  Either.fromOption[String, Int](None, ifNone = "Badness")

  import cats.syntax.either.catsSyntaxEitherId
  "Error".asLeft[Int].getOrElse(0)
  catsSyntaxEitherId("Error").asLeft[Int].getOrElse(0) // explicitly

  "Error".asLeft[Int].orElse(2.asRight[String])

  // ensure method
  (-1).asRight[String].ensure("Must be non-negative!")(_ > 0)

  // recover
  "error".asLeft[Int].recover {
    case str: String => -1
  }

  // recoverWith
  "error".asLeft[Int].recoverWith {
    case str: String => Right(-1)
  }

  "foo".asLeft[Int].leftMap(_.reverse)

  for {
    a <- 1.asRight[String]
    b <- 0.asRight[String]
    c <- if (b == 0) "DIV0".asLeft[Int]
    else (a / b).asRight[String]
  } yield c * 100
}

// 4.5
object AsideErrorHandlingAndMonadError {
  import cats.MonadError
  import cats.instances.either.catsStdInstancesForEither

  type ErrorOr[A] = Either[String, A]

  // MonadError[F, E], F = Monad, E = error type contained within F
  val monadError = MonadError[ErrorOr, String]
  val success = monadError.pure(42) // Right(42)
  val failure = monadError.raiseError("Badness") // Left("Badness")

  monadError.handleError(failure) {
    case "Badness" =>
      monadError.pure("It's ok")
    case other =>
      monadError.raiseError("It's not OK")
  }

  import cats.syntax.either._
  monadError.ensure(success)("Number too low!")(_ > 1000)

  import cats.syntax.applicative._
  import cats.syntax.applicativeError._
  import cats.syntax.monadError._

  val success2 = 42.pure[ErrorOr]
  val failure2 = "Badness".raiseError[ErrorOr, Int]
  success.ensure("Number too low!")(_ > 100)
}

// 4.5.3 instances of monaderror
object MonadErrorInstances {
  import scala.util.Try
  import cats.instances.try_._
  import cats.syntax.applicativeError._

  val exn: Throwable = new RuntimeException("Int's all gone wrong")
  exn.raiseError[Try, Int]
}
