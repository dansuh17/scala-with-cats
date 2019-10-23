import cats.Monoid
import cats.syntax.semigroup.catsSyntaxSemigroup // |+|

// Case Study: Map-Reduce
def foldMap[A, B: Monoid](seq: Vector[A])(f: A => B): B =
  seq.foldLeft(Monoid[B].empty)(_ |+| f(_))

import cats.instances.int._
foldMap(Vector(1, 2, 3))(identity)

import cats.instances.string._

foldMap(Vector(1, 2, 3))(_.toString + "! ")

foldMap("Hello world!".toVector)(_.toString.toUpperCase)

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

val future1 = Future {
  (1 to 100).toList.foldLeft(0)(_ + _)
}

val future2 = Future {
  (100 to 200).toList.sum
}

val future3 = future1.map(_.toString)

val future4 = for {
  a <- future1
  b <- future2
} yield a + b

Future.sequence(List(Future(1), Future(2), Future(3)))

Runtime.getRuntime.availableProcessors()

(1 to 10).toList.grouped(3).toList
