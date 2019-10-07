package chapter3

object chapter3

object FutureFunctor {
  import scala.concurrent.{Future, Await}
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration.DurationInt

  val future: Future[String] = Future(123)
    .map(n => n + 1)
    .map(n => n * 2)
    .map(n => n + "!")

  Await.result(future, 1.second)
}

object FunctionFunctor {
  import cats.implicits.catsStdMonadForFunction1
  import cats.syntax.functor.toFunctorOps

  val func1: Int => Double = (x: Int) => x.toDouble
  val func2: Double => Double = (y: Double) => y * 2

  // requires "-Ypartial-unification" enabled
  (func1 map func2)(1)

  (func1 andThen func2)(1)

  func2(func1(1))

  // requires "-Ypartial-unification" enabled
  val func = ((x: Int) => x.toDouble)
    .map(x => x + 1)
    .map(x => x * 2)
    .map(x => x + "!")

  val funcExplicit =
    toFunctorOps((x: Int) => x.toDouble)(catsStdMonadForFunction1)
      .map(x => x + 1)
      .map(x => x * 2)
      .map(x => x + "!")

  func(123) // String "248.0!"
}

object MyFunctorDef {
  // enable advanced language feature - or add "-language:higherKinds" to scalac options
  import scala.language.higherKinds

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }
}

// 3.5
object FunctorsInCats {
  import scala.language.higherKinds
  import cats.Functor
  import cats.instances.list.catsStdInstancesForList
  import cats.instances.option.catsStdInstancesForOption

  val list1 = List(1, 2, 3)
  // Functor.apply accepts a type constructor accepting one type
  // implicitly accepts functor instance of List
  val list2 = Functor[List].map(list1)(_ * 2)

  val option1 = Option(123)

  // implicitly accepts functor instance of Option
  val option2 = Functor[Option].map(option1)(_.toString)

  // lift = converts A => B to F[A] => F[B]
  val func = (x: Int) => x + 1
  val liftedFunc = Functor[Option].lift(func)

  liftedFunc(Option(123))
}

object FunctorSyntax {
  import scala.language.higherKinds
  import cats.Functor
  import cats.instances.function.catsStdMonadForFunction1
  import cats.syntax.functor.toFunctorOps

  val func1 = (a: Int) => a + 1
  val func2 = (a: Int) => a * 2
  val func3 = (a: Int) => a + "!"
  val func4 = func1.map(func2).map(func3)
  func4(123) // String "248!"

  // defining a doMath for
  def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] =
    start.map(n => n + 1 * 2)

  import cats.instances.option.catsStdInstancesForOption
  doMath(Option(20))

  import cats.instances.list.catsStdInstancesForList
  doMath(List(1, 2, 3))
}

// 3.5.4 Exercise: Branching out with functors
object BranchingOutWithFunctors {
  import cats.Functor

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  // functor type class instance for Tree
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(value)  => Leaf(f(value))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  import cats.syntax.functor.toFunctorOps
  val t1: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)) // note the type is declared Tree[Int]
  val t2: Tree[String] = t1.map((x: Int) => x.toString)

  // The code below causes compilation error because functor is defined for Tree, not Leaf or Branch
  // val t3: Branch[Int] = Branch(Leaf(1), Leaf(2)).map((x: Int) => x.toString)

  // Use smart constructors to compensate this issue

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
    def leaf[A](value: A): Tree[A] = Leaf(value)
  }

  val t4 = Tree.branch(Tree.leaf(1), Tree.leaf(2)).map((x: Int) => x.toString)
}

// Exercise 3.6.1.1 Showing off with Contramap
object ContravariantFunctor {
  trait Printable[A] { self =>
    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] =
      (value: B) => self.format(func(value))
  }

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  implicit val stringPrintable: Printable[String] = (value: String) =>
    "\"" + value + "\""

  implicit val booleanPrintable: Printable[Boolean] = (value: Boolean) =>
    if (value) "yes" else "no"

  format("Hello") // "Hello"
  format(true) // yes

  final case class Box[A](value: A)

  // printable instance for Box using contramap
  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap[Box[A]](_.value)

  // you can do this for any types where Printable instances are implicitly defined
  format(Box("hello world"))
  format(Box(true))

  // this will fail:
  // format(Box(123))
}
