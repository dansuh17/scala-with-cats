package chapter8

import scala.language.higherKinds
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import cats.{Id, Applicative}
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.functor._

object chapter8

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}

// hosts: provide dummy host data
// class TestUptimeClient(hosts: Map[String, Int]) extends TestUptimeClient {
//   override def getUptime(hostname: String): Int =
//     Future.successful(hosts.getOrElse(hostname, 0))
//
//   def testTotalUptime(): Unit = {
//     val host = Map("host1" -> 10, "host2" -> 6)
//     val client = new TestUptimeClient(hosts)
//     val service = new UptimeService(client)
//     val actual = service.getTotalUptime(hosts.keys.toList)
//     val expected = hosts.values.sum
//     // assert(actual == expected) // !! comparing unrelated types
//   }
// }

trait RealUptimeClient extends UptimeClient[Future] {
  override def getUptime(hostname: String): Future[Int]
}

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  override def getUptime(hostname: String): Int = hosts.getOrElse(hostname, 0)

  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

  testTotalUptime()
}
