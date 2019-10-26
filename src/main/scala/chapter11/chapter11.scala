package chapter11

object chapter11
import scala.language.higherKinds

// eventual consistency

import cats.kernel.CommutativeMonoid

trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
  def combine(a1: A, a2: A): A
  def empty: A
}

object BoundedSemiLattice {
  implicit val intBoundedSemiLattice: BoundedSemiLattice[Int] =
    new BoundedSemiLattice[Int] {
      override def combine(a1: Int, a2: Int): Int = a1 max a2

      override def empty: Int = 0
    }

  implicit def setBoundedSemiLattice[A]: BoundedSemiLattice[Set[A]] =
    new BoundedSemiLattice[Set[A]] {
      override def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2

      override def empty: Set[A] = Set.empty[A]
    }
}

// increment -> monoid
// total -> commutative monoid
// merge -> idempotent commutative monoid (bounded semilattice)

trait GCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(
    implicit m: CommutativeMonoid[V]
  ): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V])(
    implicit b: BoundedSemiLattice[V]
  ): F[K, V]

  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}

object GCounter {
  import cats.instances.list._
  import cats.instances.map._
  import cats.syntax.monoid._
  import cats.syntax.foldable._

  implicit def mapGCounterInstance[K, V]: GCounter[Map, K, V] =
    new GCounter[Map[K, V], K, V] {
      override def increment(
        f: Map[K, V]
      )(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K, V] = {
        val prev = f.getOrElse(k, m.empty)
        val newVal = prev |+| v
        f + (k -> newVal)
      }

      override def merge(f1: Map[K, V], f2: Map[K, V])(
        implicit b: BoundedSemiLattice[V]
      ): Map[K, V] = f1 |+| f2

      override def total(f: Map[K, V])(implicit m: CommutativeMonoid[V]): V =
        f.values.toList.combineAll
    }

  def apply[F[_, _], K, V](
    implicit counter: GCounter[F, K, V]
  ): GCounter[F, K, V] = counter

  import KeyValueStore._

  // Create instance of GCounter if KeyValueStore instance and
  // CommutativeMonoid instances are implicitly defined
  implicit def gcounterInstance[F[_, _], K, V](
    implicit kvs: KeyValueStore[F],
    km: CommutativeMonoid[F[K, V]]
  ): GCounter[F, K, V] =
    new GCounter[F, K, V] {

      override def increment(
        f: F[K, V]
      )(key: K, value: V)(implicit m: CommutativeMonoid[V]): F[K, V] = {
        // using type interface syntax
        val total = f.getOrElse(key, m.empty) |+| value
        f.put(key, total)
      }

      override def merge(f1: F[K, V], f2: F[K, V])(
        implicit b: BoundedSemiLattice[V]
      ): F[K, V] = f1 |+| f2

      override def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V =
        f.values.combineAll
    }
}

trait KeyValueStore[F[_, _]] {
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

  def get[K, V](f: F[K, V])(k: K): Option[V]

  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
    get(f)(k).getOrElse(default)

  def values[K, V](f: F[K, V]): List[V]
}

object KeyValueStore {
  implicit val mapInst: KeyValueStore[Map] = new KeyValueStore[Map] {
    override def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] =
      f + (k -> v)

    override def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)

    override def values[K, V](f: Map[K, V]): List[V] = f.values.toList
  }

  // type interface syntax
  implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
    def put(key: K, value: V)(implicit kvs: KeyValueStore[F]): F[K, V] =
      kvs.put(f)(key, value)

    def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] =
      kvs.get(f)(key)

    def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F]): V =
      get(key).getOrElse(default)

    def values(implicit kvs: KeyValueStore[F]): List[V] =
      kvs.values(f)
  }
}
