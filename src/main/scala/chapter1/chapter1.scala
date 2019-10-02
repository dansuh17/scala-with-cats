package chapter1

sealed trait Json

object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)

  // example
  import JsonWriterInstances._
  val personJson = toJson(Person("dansuh", "dan.suh@msmsm.com")) // JsonWriter[Person] implicitly added
}

final case class JsObject(get: Map[String, Json]) extends Json {
  override def equals(that: Any): Boolean = super.equals(that)
}

// extension methods
object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json = w.write(value)
  }

  // extends method for Person class
  import JsonWriterInstances._
  val personJson = Person("dansuh", "dan.suh@msdfsdf.com").toJson
}

final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
case object JsNull extends Json

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
