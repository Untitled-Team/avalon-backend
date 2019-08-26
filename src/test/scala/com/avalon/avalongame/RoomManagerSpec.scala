package com.avalon.avalongame

import cats.effect.{ContextShift, IO}
import org.scalatest.{FunSuite, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class RoomManagerSpec extends FunSuite with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {
  import com.mrdziuban.ScalacheckMagnolia._

  implicit val cs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)

  test("Create and update the room manager, alongside some room tests") {
    forAll { (chatId: RoomId, user: User, config: GameConfig) =>

      val roomManager = RoomManager.build[IO].unsafeRunSync()

      roomManager.create(chatId, config).unsafeRunSync()

      val room = roomManager.get(chatId).unsafeRunSync()

      room.users.unsafeRunSync() should be(Nil)
      room.addUser(user).unsafeRunSync()
      room.users.unsafeRunSync() should be(List(user))
    }
  }

}