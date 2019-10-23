package chapter8

import scala.language.higherKinds
import scala.concurrent.Future
import cats.{Id, Applicative}
import cats.instances.list.catsStdInstancesForList // Traverse instance
import cats.syntax.traverse.toTraverseOps
import cats.syntax.functor.toFunctorOps // map

object chapter8

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  // List[String] -> F[List[Int]] -> F[Int]
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}

trait RealUptimeClient extends UptimeClient[Future] {
  override def getUptime(hostname: String): Future[Int]
}

// Test client that doesn't use Future but instead uses Id
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
