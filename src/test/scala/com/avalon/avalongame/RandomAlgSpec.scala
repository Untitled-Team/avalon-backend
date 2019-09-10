package com.avalon.avalongame

import cats.effect.{ContextShift, IO}
import com.avalon.avalongame.common._
import com.mrdziuban.ScalacheckMagnolia._
import org.scalatest.{FunSuite, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class RandomAlgSpec extends FunSuite with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {

  implicit val cs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)

  val randomAlg: RandomAlg[IO] = RandomAlg.build[IO]

  test("Cycle through users, circling back to start when through the list") {
    val user1 = Nickname("Taylor")
    val user2 = Nickname("Nick")
    val user3 = Nickname("Chris")
    val user4 = Nickname("Carter")
    val user5 = Nickname("Austin")

    val users = List(user1, user2, user3, user4, user5)

    randomAlg.clockwise(user1, users).unsafeRunSync() should be(user2)
    randomAlg.clockwise(user2, users).unsafeRunSync() should be(user3)
    randomAlg.clockwise(user3, users).unsafeRunSync() should be(user4)
    randomAlg.clockwise(user4, users).unsafeRunSync() should be(user5)
    randomAlg.clockwise(user5, users).unsafeRunSync() should be(user1)
  }
}