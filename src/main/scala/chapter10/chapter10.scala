package chapter10

import cats.Semigroup
import cats.data.{NonEmptyList, Validated, Kleisli}
import cats.data.Validated.{Valid, Invalid}
import cats.instances.list._
import cats.syntax.semigroup._ // for |+|
import cats.syntax.validated._
import cats.syntax.apply._
import cats.syntax.either._
import cats.instances.either._

object chapter10

object DataValidation {
  import Check._

  type Errors = NonEmptyList[String]

  def error(s: String): NonEmptyList[String] = NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(error(s"Must be longer than $n characters"), _.length > n)

  def alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      _.forall(_.isLetterOrDigit)
    )

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(error(s"Must contain character $char"), _.contains(char))

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      _.filter(c => c == char).size == 1
    )

  val usernameCheck: Check[Errors, String, String] = PurePredicate(
    longerThan(4) and alphanumeric
  )

  val splitEmail: Check[Errors, String, (String, String)] =
    Check(_.split('@') match {
      case Array(name, domain) => (name, domain).validNel[String]
      case _ =>
        "Must contain a single @ character".invalidNel[(String, String)]
    })

  val checkUsername: Check[Errors, String, String] = Check(longerThan(0))
  val checkDomain: Check[Errors, String, String] = Check(
    longerThan(3) and contains('.')
  )

  val joinEmail: Check[Errors, (String, String), String] = Check {
    case (username, domain) =>
      (checkUsername(username), checkDomain(domain)).mapN(_ + "@" + _)
  }

  val checkEmail: Check[Errors, String, String] = splitEmail andThen joinEmail

  final case class User(username: String, email: String)

  // Create a user while checking through all requirements
  def createUser(username: String, email: String): Validated[Errors, User] =
    (checkUsername(username), checkEmail(email)).mapN(User)
}

object DataValidationWithKleisli {
  type Errors = NonEmptyList[String]

  def error(s: String): NonEmptyList[String] = NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(error(s"Must be longer than $n characters"), _.length > n)

  def alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      _.forall(_.isLetterOrDigit)
    )

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(error(s"Must contain character $char"), _.contains(char))

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      _.filter(c => c == char).size == 1
    )

  type Res[A] = Either[Errors, A]
  type Check[A, B] = Kleisli[Res, A, B]

  def check[A, B](func: A => Res[B]): Check[A, B] = Kleisli(func)

  def checkFromPred[A](pred: Predicate[Errors, A]): Check[A, A] =
    check(pred.run)

  val usernameCheck: Check[String, String] = checkFromPred(
    longerThan(4) and alphanumeric
  )

  val splitEmail: Check[String, (String, String)] = check {
    _.split('@') match {
      case Array(username, domain) => Right((username, domain))
      case _                       => Left(error("Must contain single @ character"))
    }
  }

  val checkUsername: Check[String, String] = checkFromPred(longerThan(0))
  val checkDomain: Check[String, String] = checkFromPred(
    longerThan(3) and contains('.')
  )

  val joinEmail: Check[(String, String), String] = check {
    case (user, domain) =>
      (checkUsername(user), checkDomain(domain)).mapN(_ + "@" + _)
  }

  val checkEmail: Check[String, String] = splitEmail andThen joinEmail

  final case class User(username: String, email: String)

  def createUser(username: String, email: String): Either[Errors, User] =
    for {
      username <- usernameCheck(username)
      email <- checkEmail(email)
    } yield User(username, email)
}

// Models Predicate as ADT
sealed trait Predicate[E, A] {
  import Predicate._

  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case Pure(func) => func(a)
      // combine the results of the two Semigroups
      case And(left, right) =>
        (left(a), right(a)).mapN((_, _) => a)
      case Or(left, right) =>
        left(a) match {
          case Valid(x) => Valid(x)
          case Invalid(e1) =>
            right(a) match {
              case Valid(y)    => Valid(y)
              case Invalid(e2) => Invalid(e1 |+| e2)
            }
        }
    }

  def run(implicit s: Semigroup[E]): A => Either[E, A] =
    (a: A) => apply(a).toEither
}

object Predicate {
  final case class Pure[E, A](func: A => Validated[E, A])
      extends Predicate[E, A]

  final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A])
      extends Predicate[E, A]

  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A])
      extends Predicate[E, A]

  def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] =
    Pure(a => if (fn(a)) a.valid else err.invalid)
}

sealed trait Check[E, A, B] {
  import Check._

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](func: B => C): Check[E, A, C] = Map[E, A, B, C](this, func)

  def flatMap[C](func: B => Check[E, A, C]): Check[E, A, C] =
    FlatMap(this, func)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] = AndThen(this, that)
}

object Check {
  final case class AndThen[E, A, B, C](check: Check[E, A, B],
                                       c2: Check[E, B, C])
      extends Check[E, A, C] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => c2(b).toEither))
  }

  final case class FlatMap[E, A, B, C](check: Check[E, A, B],
                                       func: B => Check[E, A, C])
      extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => func(b)(a).toEither))
  }

  final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C)
      extends Check[E, A, C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(in).map(func)
  }

  final case class Pure[E, A, B](func: A => Validated[E, B])
      extends Check[E, A, B] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B] = func(a)
  }

  final case class PurePredicate[E, A](pred: Predicate[E, A])
      extends Check[E, A, A] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, A] = pred(in)
  }

  def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] = PurePredicate(pred)

  def apply[E, A, B](func: A => Validated[E, B]): Check[E, A, B] = Pure(func)
}

object CheckF {
  final case class CheckF[E, A](func: A => Either[E, A]) {
    def apply(a: A): Either[E, A] = func(a)

    def and(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] =
      CheckF { a =>
        (this(a), that(a)) match {
          case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
          case (Left(e), Right(_))  => e.asLeft
          case (Right(_), Left(e))  => e.asLeft
          case (Right(_), Right(_)) => a.asRight
        }
      }
  }

  // checkF usage
  val a: CheckF[List[String], Int] = CheckF { v =>
    if (v > 2) v.asRight
    else List("Must be > 2").asLeft
  }

  val b: CheckF[List[String], Int] = CheckF { v =>
    if (v < -2) v.asRight
    else List("Must be < -2").asLeft
  }

  // a check that can never succeed
  val check: CheckF[List[String], Int] = a and b

  check(5) // Left of List(Must be < -2)
  check(0) // Left of List(Must be > 2, Must be < -2)
}
