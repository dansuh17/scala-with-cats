package chapter7

object chapter7

// Foldable = foldLeft, foldRight
// Traverse = uses Applicatives

// 7.1.2 Exercise
object ReflectingOnFolds {
  // reverse
  List(1, 2, 3, 4, 5).foldLeft(List.empty[Int])((acc, elem) => elem :: acc)

  List(1, 2, 3, 4, 5).foldRight(List.empty[Int])((elem, acc) => elem :: acc)
}

// 7.1.3 Exercise
object ScaffoldingOtherMethods {
  // for List
  def map[A, B](l: List[A])(f: A => B): List[B] =
    l.foldLeft(List.empty[B])((acc, elem) => f(elem) :: acc)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    l.foldLeft(List.empty[B])((acc, elem) => f(elem) ++ acc)

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    l.foldLeft(List.empty[A])((acc, elem) => if (f(elem)) elem :: acc else acc)

  // to implement 'sum', you require a binary 'plus' operator, which can be abstracted with Monoid
  import cats.Monoid
  def sum[A](l: List[A])(implicit monoid: Monoid[A]): A =
    l.foldRight(monoid.empty)(monoid.combine)
}

// folding right
object FoldableInCats {
  import cats.Foldable
  import cats.instances.list._

  val ints = List(1, 2, 3)
  Foldable[List].foldLeft(ints, 0)(_ + _)

  import cats.instances.option._
  val maybeInt = Option(123)
  Foldable[Option].foldLeft(maybeInt, 10)(_ * _)

  import cats.instances.stream._
  import cats.Eval
  val bigData = (1 to 100000).toStream
  val eval: Eval[Long] = Foldable[Stream].foldRight(bigData, Eval.now(0L)) {
    (num, eval) =>
      eval.map(_ + num)
  }

  eval.value // stack safe
}

// foldable + monoids
object FoldableWithMonoids {
  import cats.Foldable
  import cats.instances.list.catsStdInstancesForList
  import cats.instances.int.catsKernelStdGroupForInt

  Foldable[List].combineAll(List(1, 2, 3))
  Foldable[List](catsStdInstancesForList)
    .combineAll(List(1, 2, 3))(catsKernelStdGroupForInt)

  import cats.instances.string._ // Monoid

  Foldable[List].foldMap(List(1, 2, 3))(_.toString)

  import cats.instances.vector._

  val inst = List(Vector(1, 2, 3), Vector(4, 5, 6))

  // composing foldables
  (Foldable[List] compose Foldable[Vector]).combineAll(inst)
}

// 7.1.4.3 Syntax for Foldable
object FoldableSyntax {
  import cats.instances.list._
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.foldable._

  // below two lines are the same thing
  List(1, 2, 3).combineAll
  toFoldableOps(List(1, 2, 3))(catsStdInstancesForList)
    .combineAll(catsKernelStdGroupForInt)

  List(1, 2, 3).foldMap(_.toString)
}

// 7.2.1
object TraverseWithFutures {
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val hostnames = List("a.com", "b.com", "c.com")

  def getUptime(hostname: String): Future[Int] = Future(hostname.length * 60)

  val allUptimes: Future[List[Int]] =
    hostnames.foldLeft(Future(List.empty[Int])) { (acc, host) =>
      val uptime = getUptime(host) // create a Future
      for {
        acc <- acc
        uptime <- uptime
      } yield acc :+ uptime // create a combined Future[List[Int]]
    }

  Await.result(allUptimes, 1.second)

  val allUptimesTraverse: Future[List[Int]] =
    Future.traverse(hostnames)(getUptime)
  Await.result(allUptimes, 1.second)

  // traverse
  // start with List[A]
  // provide A => Future[B]
  // end up with Future[List[B]]

  // sequence
  // start with List[Future[A]]
  // end up with Future[List[A]]
}

// 7.2.2
object TraversingWithApplicatives {
  import scala.concurrent.Future
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  import cats.Applicative
  import cats.instances.future._
  import cats.syntax.applicative._
  import cats.syntax.apply._

  val a: Future[List[Int]] = List.empty[Int].pure[Future]

  def getUptime(hostname: String): Future[Int] = Future(hostname.length * 60)

  def newCombine(acc: Future[List[Int]], host: String): Future[List[Int]] =
    (acc, getUptime(host)).mapN(_ :+ _)

  import scala.language.higherKinds

  def listTraverse[F[_]: Applicative, A, B](
    list: List[A]
  )(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (acc, item) =>
      (acc, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)
}

// 7.2.2.1 Exercise
object TraverseWithVectors {
  import cats.instances.vector._
  import TraversingWithApplicatives._

  listSequence(List(Vector(1, 2), Vector(3, 4))) // (1, 3), (1, 4), (2, 3), (2, 4)
}

// 7.2.2.2 Exercise
object TraverseWithOptions {
  import cats.instances.option._
  import TraversingWithApplicatives._

  def process(inputs: List[Int]): Option[List[Int]] =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)
}
