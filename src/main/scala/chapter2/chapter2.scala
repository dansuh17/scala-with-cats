package chapter2

object chapter2 {}

trait MySemigroup[A] {
  def combine(x: A, y: A): A
}

trait MyMonoid[A] extends MySemigroup[A] {
  def empty: A
}

object MyMonoid {
  def apply[A](implicit monoid: MyMonoid[A]): MyMonoid[A] = monoid

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: MyMonoid[A]): Boolean =
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)

  def identityLaw[A](x: A)(implicit m: MyMonoid[A]): Boolean =
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
}

// 2.3 Exercise: Truth about monoids
object BooleanMonoids {
  implicit val booleanOr: MyMonoid[Boolean] =
    new MyMonoid[Boolean] {
      override def empty: Boolean = false
      override def combine(x: Boolean, y: Boolean): Boolean = x || y
    }

  implicit val booleanAnd: MyMonoid[Boolean] =
    new MyMonoid[Boolean] {
      override def empty: Boolean = true
      override def combine(x: Boolean, y: Boolean): Boolean = x && y
    }

  implicit val booleanXor: MyMonoid[Boolean] =
    new MyMonoid[Boolean] {
      override def empty: Boolean = false
      override def combine(x: Boolean, y: Boolean): Boolean =
        (x && !y) || (!x && y)
    }

  implicit val booleanXnor: MyMonoid[Boolean] =
    new MyMonoid[Boolean] {
      override def empty: Boolean = true
      override def combine(x: Boolean, y: Boolean): Boolean =
        (!x || y) && (x || !y)
    }
}

// 2.4 Exercise
object SetMonoids {
  implicit def setUnion[A](): MyMonoid[Set[A]] =
    new MyMonoid[Set[A]] {
      override def empty: Set[A] = Set.empty
      override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
    }

  // NOT a monoid, no identity for set intersection
  implicit def setIntersection[A](): MySemigroup[Set[A]] =
    new MySemigroup[Set[A]] {
      override def combine(x: Set[A], y: Set[A]): Set[A] = x intersect y
    }

  // cannot define monoid for diff and complement - not associative
}

// 2.5
object CatsMonoid {
  // monoid instances
  import cats.Monoid
  import cats.instances.int.catsKernelStdGroupForInt // monoid addition instance for int
  import cats.instances.option.catsKernelStdMonoidForOption

  val intCombined: Int = Monoid[Int].combine(32, 10)
  val optionCombined: Option[Int] =
    Monoid[Option[Int]].combine(Option(22), Option(20))

  // for semigroup syntax (why no syntax for monoid? - no method to express specially for monoid)
  import cats.instances.string.catsKernelStdMonoidForString
  import cats.syntax.semigroup.catsSyntaxSemigroup

  val stringResult: String = "Hi" |+| "there" |+| Monoid[String].empty
  // explicit version of above
  val stringResExplicit: String =
    catsSyntaxSemigroup("Hi")(catsKernelStdMonoidForString).|+|("there")
  val intResult: Int = 1 |+| 2 |+| Monoid[Int].empty
}

// 2.5.4 Exercise: Adding all the things
object SuperAdder {
  import cats.Monoid
  import cats.syntax.semigroup.catsSyntaxSemigroup

  // def add(item: List[Int]): Int = item.foldLeft(Monoid[Int].empty)(_ |+| _)

  def add[A](item: List[A])(implicit m: Monoid[A]): A =
    item.foldLeft(m.empty)(_ |+| _)

  case class Order(totalCost: Double, quantity: Double)

  // define a typeclass instance for Order so that we can reuse add()
  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    override def empty: Order = Order(0, 0)
    override def combine(x: Order, y: Order): Order =
      Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

  val addedOrder: Order = add(List(Order(1, 2), Order(1, 5), Order(6, 6.0)))
}
