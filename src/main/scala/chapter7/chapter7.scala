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
