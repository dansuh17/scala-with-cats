package chapter1

import cats.Show

sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
case object JsNull extends Json

object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)

  // example
  import JsonWriterInstances._
  val personJson
    : Json = toJson(Person("dansuh", "dan.suh@msmsm.com")) // JsonWriter[Person] implicitly added
}

// extension methods
object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json = w.write(value)
  }

  // extends method for Person class
  import JsonWriterInstances._
  val personJson: Json = Person("dansuh", "dan.suh@jmsdfsdf.com").toJson
}

// type class
trait JsonWriter[A] {
  def write(value: A): Json
}

final case class Person(name: String, email: String)

// type class instances
object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] = new JsonWriter[String] {
    def write(value: String): Json = JsString(value)
  }

  implicit val personWriter: JsonWriter[Person] = new JsonWriter[Person] {
    def write(p: Person): Json =
      JsObject(Map("name" -> JsString(p.name), "email" -> JsString(p.email)))
  }
}

/**
  * Exercise 1.3 - Printable Library
  */
// type class
trait Printable[A] {
  def format(value: A): String
}

// type class instances
object PrintableInstances {
  implicit val printableString: Printable[String] = (value: String) => value
  implicit val printableInt: Printable[Int] = (value: Int) => value.toString
}

// interface syntax
object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String = p.format(value)
    def print(implicit p: Printable[A]): Unit = println(value.format)
  }
}

// define interface objects
object Printable {
  def format[A](value: A)(implicit pable: Printable[A]): String =
    pable.format(value)

  def print[A](value: A)(implicit pable: Printable[A]): Unit =
    println(format(value))
}

// example using Printable[A]
object Catlib {
  final case class Cat(name: String, age: Int, color: String)

  implicit val printableCat: Printable[Cat] = (cat: Cat) =>
    s"${cat.name} is a ${cat.age} year-old ${cat.color} cat"

  val cat = Cat("ha", 2, "gray")

  // using interface object
  println(Printable.format(cat)) // implicitly uses printableCat
  Printable.print(cat)

  // using interface syntax
  import PrintableSyntax._
  println(cat.format)
  cat.print
}

object show {
  import cats.instances.int.catsStdShowForInt
  import cats.instances.string.catsStdShowForString

  // uses implicit Show instances
  val showInt: Show[Int] = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]

  val intAsString: String = showInt.show(1912)
  val stringAsString: String = showString.show("abc")
}

object CatShow {
  final case class Cat(name: String, age: Int, color: String)

  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.show.toShow

  implicit val showCat: Show[Cat] = Show.show[Cat] { cat =>
    s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat"
  }

  // interface object
  val myCat = Cat("mycat", 3, "red")
  showCat.show(myCat)

  // interface syntax
  myCat.show
}

// 1.5.2
object ComparingInts {
  import cats.Eq
  // IntOrder extends Order which extends PartialOrder which extends Eq
  import cats.instances.int.catsKernelStdOrderForInt

  // type class instance accessing implicit catsKernelStdOrderForInt
  val eqInt: Eq[Int] = Eq[Int]
  eqInt.eqv(123, 123)

  import cats.syntax.eq.catsSyntaxEq // for === and =!=
  123 === 123
  123 =!= 234
  // 123 === "123" - compiler error
}

object ComparingOption {
  import cats.instances.int.catsKernelStdOrderForInt
  import cats.instances.option.catsKernelStdOrderForOption
  import cats.syntax.eq.catsSyntaxEq

  // Some(1) === None
  (Some(1): Option[Int]) === (None: Option[Int])
  Option(1) === Option.empty[Int] // == none

  import cats.syntax.option._
  1.some === none[Int] // TODO: try to expand this
}
