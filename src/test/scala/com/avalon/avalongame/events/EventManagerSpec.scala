package com.avalon.avalongame.events

import cats.effect.{ContextShift, IO, Timer}
import com.avalon.avalongame.RandomAlg
import com.avalon.avalongame.common._
import com.avalon.avalongame.room._
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
        val players = room.players.unsafeRunSync()

        players should be(List(createGame.nickname))

        queue.dequeue1.unsafeRunSync() should be(MoveToLobby(mockRoomIdGenerator.generate.unsafeRunSync(), players))
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
        room.players.unsafeRunSync() should be(List(nickname1))
        userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(MoveToLobby(roomId, room.players.unsafeRunSync()))
        //================

        //================
        eventManager.interpret(userQueue2, Stream.eval(IO.pure(JoinGame(nickname2, roomId)))).unsafeRunSync()

        room.players.unsafeRunSync() should contain allOf (nickname1, nickname2)
        userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(ChangeInLobby(room.players.unsafeRunSync()))
        userQueue2.dequeue1.timeout(1 second).unsafeRunSync() should be(MoveToLobby(roomId, room.players.unsafeRunSync()))
        //================

        //================
        eventManager.interpret(userQueue3, Stream.eval(IO.pure(JoinGame(nickname3, roomId)))).unsafeRunSync()

        room.players.unsafeRunSync() should contain allOf (nickname1, nickname2, nickname3)
        userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(ChangeInLobby(room.players.unsafeRunSync()))
        userQueue2.dequeue1.timeout(1 second).unsafeRunSync() should be(ChangeInLobby(room.players.unsafeRunSync()))
        userQueue3.dequeue1.timeout(1 second).unsafeRunSync() should be(MoveToLobby(roomId, room.players.unsafeRunSync()))
        //================
      }
    }
  }

//  test("Get `GameStarted` for all Sockets in response to `StartGame` message") {
//    forAll { (roomId: RoomId, gameConfig: GameConfig) =>
//      new context {
//
//        override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
//          override def generate: IO[RoomId] = IO.pure(roomId)
//        }
//        val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
//        val nickname2 = Nickname(java.util.UUID.randomUUID().toString)
//        val nickname3 = Nickname(java.util.UUID.randomUUID().toString)
//        val nickname4 = Nickname(java.util.UUID.randomUUID().toString)
//        val nickname5 = Nickname(java.util.UUID.randomUUID().toString)
//
//
//
//        val eventManager: EventManager[IO] = EventManager.build[IO](roomManager, mockRoomIdGenerator).unsafeRunSync()
//
//        val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
//        val userQueue2 = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
//        val userQueue3 = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
//        val userQueue4 = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
//        val userQueue5 = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
//
//        //================
//        eventManager.interpret(userQueue, Stream.eval(IO.pure(CreateGame(nickname1, gameConfig)))).unsafeRunSync()
//
//        val room = roomManager.get(roomId).unsafeRunSync()
//        room.players.unsafeRunSync() should be(List(User(nickname1)))
//        userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(MoveToLobby(roomId))
//        //================
//
//        //================
//        eventManager.interpret(userQueue2, Stream.eval(IO.pure(JoinGame(nickname2, roomId)))).unsafeRunSync()
//
//        room.players.unsafeRunSync() should contain allOf (User(nickname1), User(nickname2))
//        userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(UserJoined(nickname2))
//        userQueue2.dequeue1.timeout(1 second).unsafeRunSync() should be(JoinedRoom(room.info.unsafeRunSync()))
//        //================
//
//        //================
//        eventManager.interpret(userQueue3, Stream.eval(IO.pure(JoinGame(nickname3, roomId)))).unsafeRunSync()
//
//        room.players.unsafeRunSync() should contain allOf (User(nickname1), User(nickname2), User(nickname3))
//        userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(UserJoined(nickname3))
//        userQueue2.dequeue1.timeout(1 second).unsafeRunSync() should be(UserJoined(nickname3))
//        userQueue3.dequeue1.timeout(1 second).unsafeRunSync() should be(JoinedRoom(room.info.unsafeRunSync()))
//        //================
//
//        //================
//        eventManager.interpret(userQueue4, Stream.eval(IO.pure(JoinGame(nickname4, roomId)))).unsafeRunSync()
//
//        room.players.unsafeRunSync() should contain allOf (User(nickname1), User(nickname2), User(nickname3), User(nickname4))
//        userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(UserJoined(nickname4))
//        userQueue2.dequeue1.timeout(1 second).unsafeRunSync() should be(UserJoined(nickname4))
//        userQueue3.dequeue1.timeout(1 second).unsafeRunSync() should be(UserJoined(nickname4))
//        userQueue4.dequeue1.timeout(1 second).unsafeRunSync() should be(JoinedRoom(room.info.unsafeRunSync()))
//        //================
//
//        //================
//        eventManager.interpret(
//          userQueue5,
//          Stream.eval(IO.pure(JoinGame(nickname5, roomId))) ++ Stream.eval(IO.pure(StartGame))).unsafeRunSync()
//
//        room.players.unsafeRunSync() should contain allOf (User(nickname1), User(nickname2), User(nickname3), User(nickname4), User(nickname5))
//        userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(UserJoined(nickname5))
//        userQueue2.dequeue1.timeout(1 second).unsafeRunSync() should be(UserJoined(nickname5))
//        userQueue3.dequeue1.timeout(1 second).unsafeRunSync() should be(UserJoined(nickname5))
//        userQueue4.dequeue1.timeout(1 second).unsafeRunSync() should be(UserJoined(nickname5))
//        userQueue5.dequeue1.timeout(1 second).unsafeRunSync() should be(JoinedRoom(room.info.unsafeRunSync()))
//        //================
//
//        val resultMissions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()
//        val users = List(User(nickname1), User(nickname2), User(nickname3), User(nickname4), User(nickname5))
//        def gameStarted(role: Role) = {
//          val charRole = CharacterRole.fromRole(role, List(nickname1, nickname2))
//          GameStarted(MissionProposing(1, User(nickname1)), resultMissions, charRole, users)
//        }
//
//        userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(gameStarted(Assassin))
//        userQueue2.dequeue1.timeout(1 second).unsafeRunSync() should be(gameStarted(NormalBadGuy))
//        userQueue3.dequeue1.timeout(1 second).unsafeRunSync() should be(gameStarted(Merlin))
//        userQueue4.dequeue1.timeout(1 second).unsafeRunSync() should be(gameStarted(NormalGoodGuy))
//        userQueue5.dequeue1.timeout(1 second).unsafeRunSync() should be(gameStarted(NormalGoodGuy))
//      }
//    }
//  }

  trait context {
    val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
      override def generate: IO[RoomId] = IO.pure(RoomId("blah"))
    }

    val mockRandomAlg: RandomAlg[IO] = new RandomAlg[IO] {
      override def shuffle[A](l: List[A]): IO[List[A]] = IO.pure(l)

      override def randomGet[A](l: List[A]): IO[A] = IO(l.head) //oops
    }

    val roomManager: RoomManager[IO] = RoomManager.build[IO](mockRandomAlg).unsafeRunSync()

//    val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
//      def create(roomId: RoomId, config: GameConfig): IO[Unit] = IO.unit
//      def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
//        new Room[IO] {
//          override def info: IO[RoomInfo] = ???
//          override def users: IO[List[User]] = ???
//          override def addUser(user: User): IO[Unit] = IO.unit
//          override def startGame: IO[GameRepresentation] = ???
//        }
//      }
//    }
  }
}

