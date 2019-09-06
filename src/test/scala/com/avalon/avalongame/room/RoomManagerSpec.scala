package com.avalon.avalongame.room

import cats.effect.{ContextShift, IO}
import com.avalon.avalongame.common._
import com.avalon.avalongame.RandomAlg
import com.mrdziuban.ScalacheckMagnolia._
import org.scalatest.{FunSuite, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class RoomManagerSpec extends FunSuite with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {

  implicit val cs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)

  val mockRandomAlg: RandomAlg[IO] = new RandomAlg[IO] {
    override def shuffle[A](l: List[A]): IO[List[A]] = IO.pure(l)

    override def randomGet[A](l: List[A]): IO[A] = IO(l.head) //oops
  }

  test("Create and update the room manager, alongside some room tests") {
    forAll { (chatId: RoomId, nickname: Nickname, config: GameConfig) =>

      val roomManager = RoomManager.build[IO](mockRandomAlg).unsafeRunSync()

      roomManager.create(chatId).unsafeRunSync()

      val room = roomManager.get(chatId).unsafeRunSync()

      room.players.unsafeRunSync() should be(Nil)
      room.addUser(nickname).unsafeRunSync()
      room.players.unsafeRunSync() should be(List(nickname))
    }
  }
}