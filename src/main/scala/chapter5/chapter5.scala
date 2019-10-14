package chapter5

object chapter5

// 5.2
object TransformativeExample {
  import cats.data.OptionT
  type ListOption[A] = OptionT[List, A]

  val result1: ListOption[Int] = OptionT(List(Option(10)))

  import cats.Monad
  import cats.instances.list._
  import cats.syntax.applicative._
  val result2: ListOption[Int] = 32.pure[ListOption]

  result1.flatMap { x: Int =>
    result2.map { y: Int =>
      {
        x + y
      }
    }
  }
  // OptionT(List(Some(42)))
}

// 5.3.2
object BuildingMonadStacks {
  import cats.data.OptionT
  import cats.instances.either._
  import cats.syntax.applicative._

  type ErrorOr[A] = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  val a = 10.pure[ErrorOrOption]
  val b = 32.pure[ErrorOrOption]
  val c = a.flatMap(x => b.map(y => x + y))

  import cats.data.{EitherT, OptionT}
  import scala.concurrent.Future

  // fixes the outer monad and error types
  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  import cats.instances.future._
  import scala.concurrent.Await
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val futureEitherOr: FutureEitherOption[Int] =
    for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b

  // constructing monad stacks
  val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
  val errorStack2 = 32.pure[ErrorOrOption]
}
