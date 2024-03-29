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

// 4.6
object EvalMonad {
  import cats.Eval

  // eval once and memoize
  val now: Eval[Double] = Eval.now(math.random + 1000)

  // lazy eval
  val later: Eval[Double] = Eval.later(math.random + 2000)

  // always evaluate every time
  val always: Eval[Double] = Eval.always(math.random + 3000)

  // accessing
  now.value
  later.value
  always.value

  val greeting = Eval
    .always { println("Step1"); "Hello" }
    .map { str =>
      println("Step2"); s"$str world"
    }

  greeting.value

  val ans: Eval[Int] = for {
    a <- Eval.now { println("Calc A"); 40 }
    b <- Eval.always { println("Calc B"); 2 }
  } yield {
    // this 'map' area always called lazily on demand
    println("Adding A and B")
    a + b
  }
  // Calc A

  ans.value
  // Calc B
  // Adding A and B

  ans.value
  // Adding A and B

  val saying = Eval
    .always { println("Step 1"); "the cat" }
    .map { str =>
      println("Step 2"); s"$str sat on"
    }
    .memoize // value will be cached up to this point
    .map { str =>
      println("Step 3"); s"$str the mat"
    }
}

object Trampolining {
  // Stack Overflow Example
  // def factorial(n: BigInt): BigInt =
  //   if (n == 1) n else n * factorial(n - 1)

  // factorial(50000)

  import cats.Eval
  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.now(n)
    } else {
      Eval.defer(factorial(n - 1).map(_ * n))
    }

  val v = factorial(50000).value
}

// 4.6.5 Exercise: Safer Folding
object SaferFolding {
  import cats.Eval

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {
    def frEval[A, B](as: List[A],
                     acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
      as match {
        case head :: tail =>
          Eval.defer(fn(head, frEval(tail, acc)(fn)))
        case Nil =>
          acc
      }

    frEval(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }.value
  }
}

// 4.7 Writer Monad
object WriterMonad {
  import cats.data.Writer
  import cats.instances.vector.catsKernelStdMonoidForVector

  Writer(Vector("It was the best of times", "it was the worst of times"), 1859)

  // type Writer[W, A] = WriterT[Id, W, A]

  import cats.syntax.applicative._
  type Logged[A] = Writer[Vector[String], A]
  // we require the Vector to be an instance of Monoid so that writer knows how to 'append' the log
  // using the binary operator
  123.pure[Logged]
  // catsSyntaxApplicativeId(123).pure[Logged](catsDataMonadForWriterTId(catsKernelStdMonoidForVector))

  import cats.syntax.writer._
  Vector("msg1", "msg2", "msg3").tell
  catsSyntaxWriterId(Vector("msg1", "msg2", "msg3")).tell

  val a = Writer(Vector("msg1", "msg2", "msg3"), 123)
  val b = 123.writer(Vector("msg1", "msg2", "msg3"))
  val bExplicit = catsSyntaxWriterId(123).writer(Vector("m1", "m2", "m3"))

  val aResult: Int = a.value // result
  val aLog: Vector[String] = a.written // log
  val (log, result) = b.run

  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z")) // appends the log as it goes by
  } yield a + b // adds the results (values)

  // transform only the log using map
  val writer2 = writer1.mapWritten(_.map(_.toUpperCase)) // A B C X Y Z, 42

  // transform both the log and the result simultaneously
  val writer3 = writer1.bimap(log => log.map(_.toUpperCase), res => res * 100)

  // accepts a single function with two parameters
  val writer4 = writer1.mapBoth { (log, res) =>
    val log2 = log.map(_ + "!")
    val res2 = res * 1000
    (log2, res2)
  }

  // clear the log
  val writer5 = writer1.reset

  // swap the log and result
  val writer6 = writer1.swap
}

// 4.7.3 Exercise
object ShowYourWorking {
  import cats.data.Writer
  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import cats.syntax.writer._
  import cats.syntax.applicative._
  import cats.instances.vector.catsKernelStdMonoidForVector

  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A): A =
    try body
    finally Thread.sleep(100)

  def factorial(n: Int): Logged[Int] = {
    for {
      ans <- slowly({
        if (n == 0) 1.pure[Logged]
        else {
          factorial(n - 1).map(_ * n)
        }
      })
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

  val Vector((logA, ansA), (logB, ansB)) = Await.result(
    Future.sequence(Vector(Future(factorial(3).run), Future(factorial(3).run))),
    5.seconds
  )
}

object ReaderMonad {
  import cats.data.Reader

  case class Cat(name: String, favoriteFood: String)

  val catName: Reader[Cat, String] = Reader(_.name)
  catName.run(Cat("Garfield", "lasagne")) // cats.Id[String] = Garfield

  val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello ${name}")
  val feedKitty: Reader[Cat, String] = Reader(
    cat => s"Have a nice bowl of ${cat.favoriteFood}"
  )

  // usinf flatMap to combine readers
  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed <- feedKitty
    } yield s"$greet. $feed."
}

// 4.8.3 Exercise
object HackingOnReaders {
  import cats.data.Reader
  import cats.syntax.applicative._

  case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  // compose two readers to render new value
  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    findUsername(userId).flatMap {
      case None     => false.pure[DbReader]
      case Some(un) => checkPassword(un, password)
    }
  // for {
  //   userNameOp <- findUsername(userId)
  //   passwordOk <- userNameOp
  //     .map { uname =>
  //       checkPassword(uname, password)
  //     }
  //     .getOrElse {
  //       false.pure[DbReader]
  //     }
  // } yield passwordOk
}

// 4.9 StateMonad
object StateMonad {
  import cats.data.State
  val a = State[Int, String] { state =>
    (state, s"The state is $state")
  }

  val (state, result) = a.run(10).value
  val stateOnly = a.runS(10).value
  val resultOnly = a.runA(10).value

  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }

  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)

  val (state2, result2) = both.run(20).value
  // state2 = 42, result2 = ("Result of step1: 21", "Result of step2: 42")
}

object StateFunctions {
  import cats.data.State
  import cats.data.State._

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1) // modify the state with a function
    c <- inspect[Int, Int](_ * 1000) // get the state after applying the function
  } yield (a, b, c)
}

// 4.9.3 Exercise
object PostOrderCalculator {
  // Instructions:
  // 1 2 + 3 * // see 1, push onto stack
  // 2 + 3 *   // see 2, push onto stack
  // + 3 *     // see +, pop 1 and 2 off of stack,
  //           //        push (1 + 2) = 3 in their place
  // 3 3 *     // see 3, push onto stack
  // 3 *       // see 3, push onto stack
  // *         // see *, pop 3 and 3 off of stack,
  //           //        push (3 * 3) = 9 in their place
  import cats.data.State
  import cats.syntax.applicative._

  type CalcState[A] = State[List[Int], A]

  // parses a single symbol into an instance of State
  def evalOne(sym: String): CalcState[Int] =
    if (sym.forall(Character.isDigit)) {
      // in case a single number...
      addNum(sym.toInt)
    } else {
      sym match {
        // in case an operator
        case "+" => evalPlus
        case "*" => evalMult
        case "-" => evalMinus
        case "/" => evalDiv
        case _   => throw RuntimeException
      }
    }

  def addNum(n: Int): CalcState[Int] = State[List[Int], Int] { stack =>
    (n :: stack, n)
  }

  def binaryOp(op: (Int, Int) => Int): CalcState[Int] = State[List[Int], Int] {
    case n1 :: n2 :: rest => {
      val res = op(n1, n2)
      (res :: rest, res)
    }
    case _ => throw RuntimeException
  }

  val evalPlus: CalcState[Int] = binaryOp(_ + _)
  val evalMult: CalcState[Int] = binaryOp(_ * _)
  val evalMinus: CalcState[Int] = binaryOp(_ - _)
  val evalDiv: CalcState[Int] = binaryOp(_ / _)

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (state, elem) =>
      state.flatMap(_ => evalOne(elem))
    }

  // example of composing results from different programs
  val program = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans

  program.runA(Nil).value

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value
}

// 4.10
object CustomMonads {
  import cats.Monad
  import scala.annotation.tailrec
  import cats.syntax.monad._
  import cats.syntax.applicative._

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
        case Leaf(value)         => f(value)
      }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(f(a)) {
        case Left(v)  => tailRecM(v)(f) // NOT in tailrec position
        case Right(v) => Leaf(v)
      }

    override def pure[A](x: A): Tree[A] = Leaf(x)
  }
}
