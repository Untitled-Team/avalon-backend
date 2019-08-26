package com.avalon.avalongame

import cats.effect.concurrent.{MVar, Ref}
import cats.effect.{ContextShift, IO, Timer}
import com.avalon.avalongame.events._
import fs2._
import fs2.concurrent.Queue
import org.scalatest.{FunSuite, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration._

class EventManagerSpec extends FunSuite with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {
  import com.mrdziuban.ScalacheckMagnolia._

  implicit val cs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)
  implicit val t: Timer[IO] = IO.timer(scala.concurrent.ExecutionContext.Implicits.global)

  test("Get `GameCreated` in response to `CreateGame` message") {
    forAll { createGame: CreateGame =>
      new context {
        val eventManager: EventManager[IO] = EventManager.build[IO](roomManager, mockRoomIdGenerator).unsafeRunSync()

        val queue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

        eventManager.interpret(queue, Stream.eval(IO.pure(createGame))).unsafeRunSync()

        val room = roomManager.get(mockRoomIdGenerator.generate.unsafeRunSync()).unsafeRunSync()
        room.users.unsafeRunSync() should be(List(User(createGame.nickname)))

        queue.dequeue1.unsafeRunSync() should be(GameCreated(mockRoomIdGenerator.generate.unsafeRunSync()))
      }
    }
  }

  test("Get `UserJoined` for all Sockets in response to `JoinGame` message") {
    forAll { (roomId: RoomId, gameConfig: GameConfig) =>
      new context {

        override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
          override def generate: IO[RoomId] = IO.pure(roomId)
        }
        val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
        val nickname2 = Nickname(java.util.UUID.randomUUID().toString)
        val nickname3 = Nickname(java.util.UUID.randomUUID().toString)

        val eventManager: EventManager[IO] = EventManager.build[IO](roomManager, mockRoomIdGenerator).unsafeRunSync()

        val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
        val userQueue2 = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
        val userQueue3 = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

        //================
        eventManager.interpret(userQueue, Stream.eval(IO.pure(CreateGame(nickname1, gameConfig)))).unsafeRunSync()

        val room = roomManager.get(roomId).unsafeRunSync()
        room.users.unsafeRunSync() should be(List(User(nickname1)))
        userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(GameCreated(roomId))
        //================

        //================
        eventManager.interpret(userQueue2, Stream.eval(IO.pure(JoinGame(nickname2, roomId)))).unsafeRunSync()

        room.users.unsafeRunSync() should contain allOf (User(nickname1), User(nickname2))
        userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(UserJoined(nickname2))
        userQueue2.dequeue1.timeout(1 second).unsafeRunSync() should be(JoinedRoom(room.info.unsafeRunSync()))
        //================

        //================
        eventManager.interpret(userQueue3, Stream.eval(IO.pure(JoinGame(nickname3, roomId)))).unsafeRunSync()

        room.users.unsafeRunSync() should contain allOf (User(nickname1), User(nickname2), User(nickname3))
        userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(UserJoined(nickname3))
        userQueue2.dequeue1.timeout(1 second).unsafeRunSync() should be(UserJoined(nickname3))
        userQueue3.dequeue1.timeout(1 second).unsafeRunSync() should be(JoinedRoom(room.info.unsafeRunSync()))
        //================
      }
    }
  }

  trait context {
    val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
      override def generate: IO[RoomId] = IO.pure(RoomId("blah"))
    }

    val roomManager: RoomManager[IO] = RoomManager.build[IO].unsafeRunSync()
  }
}

