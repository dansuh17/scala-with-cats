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

// 5.3.5
object UsagePatterns {
  import cats.data.Writer

  type Logged[A] = Writer[List[String], A]

  // method returning untransformed monad stack
  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None      => Writer(List(s"Failed on $str"), None)
    }

  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    import cats.data.OptionT

    // use monad transformers for easy composition (and use 'for')
    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c

    result.value
  }

  // user doesn't see OptionT
  val result1 = addAll("1", "2", "3") // WriterT((List(Read 1, Read 2, Read 3), Some(6)))
  val result2 = addAll("1", "a", "3")
}

// 5.4 Exercise
object TransformAndRollOut {
  import scala.concurrent.{Future, Await}
  import cats.data.EitherT
  import scala.concurrent.ExecutionContext.Implicits.global
  import cats.instances.future._
  import scala.concurrent.duration.DurationInt

  // future of error-prone message of type 'A'
  // type Response[A] = Future[Either[String, A]] // non-transformed type
  type Response[A] = EitherT[Future, String, A]

  val powerLevels: Map[String, Int] =
    Map("Jazz" -> 6, "Bumblebee" -> 8, "Hot Rod" -> 10)

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(powerLevel) => EitherT.right(Future(powerLevel))
      case None =>
        EitherT.left(Future(s"$autobot has no power level information"))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      p1 <- getPowerLevel(ally1)
      p2 <- getPowerLevel(ally2)
    } yield (p1 + p2) > 15

  def tacticalReport(ally1: String, ally2: String): String =
    Await.result(canSpecialMove(ally1, ally2).value, 1.second) match {
      case Left(error) => s"Comms error: $error"
      case Right(specialMove) =>
        if (specialMove) s"$ally1 and $ally2 are ready to roll out!"
        else s"$ally1 and $ally2 need a recharge"
    }
}
