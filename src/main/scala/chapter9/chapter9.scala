package chapter9

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import cats.Monoid
import cats.Traverse
import cats.Foldable
import cats.syntax.semigroup.catsSyntaxSemigroup // |+|
import cats.instances.int._
import cats.instances.vector._
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.foldable._

// Case Study: Map-Reduce
object chapter9 {
  // def foldMap[A, B: Monoid](seq: Vector[A])(f: A => B): B =
  //   seq.map(f).fold(Monoid[B].empty)(Monoid[B].combine)

  // Case Study: Map-Reduce
  def foldMap[A, B: Monoid](seq: Vector[A])(f: A => B): B =
    seq.foldLeft(Monoid[B].empty)(_ |+| f(_))

  import cats.instances.int._
  foldMap(Vector(1, 2, 3))(identity)

  import cats.instances.string._

  foldMap(Vector(1, 2, 3))(_.toString + "! ")

  foldMap("Hello world!".toVector)(_.toString.toUpperCase)
}

object MapReduce {
  // Case Study: Map-Reduce
  def foldMap[A, B: Monoid](seq: Vector[A])(f: A => B): B =
    seq.foldLeft(Monoid[B].empty)(_ |+| f(_))

  def parallelFoldMap[A, B: Monoid](
    values: Vector[A]
  )(func: A => B): Future[B] = {
    val numCores: Int = Runtime.getRuntime.availableProcessors()

    val calcGroup: List[List[A]] =
      values.toList.grouped(values.length / numCores).toList

    val futures: List[Future[B]] =
      calcGroup.map(calcs => Future(foldMap(calcs.toVector)(func)))

    futures.sequence.map { iter =>
      iter.foldLeft(Monoid[B].empty)(_ |+| _)
    }
  }
}

// 9.3.4
object ParallelFoldMap {
  def parallelFoldMap[A, B: Monoid](
    values: Vector[A]
  )(func: A => B): Future[B] = {
    val numCores: Int = Runtime.getRuntime.availableProcessors()
    val groupSize: Int = (values.length / numCores).ceil.toInt

    values
      .grouped(groupSize)
      .toVector
      .traverse(group => Future(group.foldMap(func)))
      .map(_.combineAll)
  }
}
