package com.avalon.avalongame.room

import cats.Eq
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
    override def clockwise[A: Eq](previous: A, l: List[A]): IO[A] = IO(l.head)
  }

  test("Create and update the room manager, alongside some room tests") {
    forAll { (roomId: RoomId, nickname: Nickname, config: GameConfig) =>

      val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
        override def generate: IO[RoomId] = IO.pure(roomId)
      }

      val roomManager = RoomManager.build[IO](mockRandomAlg, mockRoomIdGenerator).unsafeRunSync()

      roomManager.create.unsafeRunSync()

      val room = roomManager.get(roomId).unsafeRunSync()

      room.players.unsafeRunSync() should be(Nil)
      room.addUser(nickname).unsafeRunSync()
      room.players.unsafeRunSync() should be(List(nickname))
    }
  }

  test("Fail if the room already exists") {
    forAll { roomId: RoomId =>

      val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
        override def generate: IO[RoomId] = IO.pure(roomId)
      }

      val roomManager = RoomManager.build[IO](mockRandomAlg, mockRoomIdGenerator).unsafeRunSync()

      roomManager.create.unsafeRunSync()
      roomManager.create.attempt.unsafeRunSync() should be(Left(RoomAlreadyExists(roomId)))
    }
  }
}