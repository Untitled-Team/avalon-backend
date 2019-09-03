package com.avalon.avalongame

import cats.effect.{ContextShift, IO, Timer}
import com.avalon.avalongame.Room._
import org.scalatest.{FunSuite, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration._

class RoomSpec extends FunSuite with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {
  import com.mrdziuban.ScalacheckMagnolia._

  implicit val cs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)
  implicit val t: Timer[IO] = IO.timer(scala.concurrent.ExecutionContext.Implicits.global)

  val mockRandomAlg: RandomAlg[IO] = new RandomAlg[IO] {
    override def shuffle[A](l: List[A]): IO[List[A]] = IO.pure(l)

    override def randomGet[A](l: List[A]): IO[A] = IO(l.head) //oops
  }

  test("Fail when we try to start a game with fewer than 5 players") {
    forAll { (roomId: RoomId, config: GameConfig) =>
      val room = Room.build(mockRandomAlg, roomId, config).unsafeRunSync()

      room.startGame.attempt.unsafeRunSync() should be(Left(NotEnoughPlayers(0)))
    }
  }

  test("Fail if we try to add a user who has the same nickname as another user in the room") {
    forAll { (roomId: RoomId, config: GameConfig) =>

      val user1 = User(Nickname("Taylor"))
      val user2 = User(Nickname("Taylor"))

      val room = Room.build(mockRandomAlg, roomId, config).unsafeRunSync()

      room.users.unsafeRunSync() should be(Nil)

      room.addUser(user1).unsafeRunSync()
      room.addUser(user2).attempt.unsafeRunSync() should be(Left(NicknameAlreadyInUse(user1.nickname)))

      room.users.unsafeRunSync() should be(List(user1))
    }
  }

  test("Properly add users") {
    forAll { (roomId: RoomId, config: GameConfig) =>

      val user1 = User(Nickname("Taylor"))
      val user2 = User(Nickname("Nick"))
      val user3 = User(Nickname("Chris"))
      val user4 = User(Nickname("Carter"))
      val user5 = User(Nickname("Austin"))


      val room = Room.build(mockRandomAlg, roomId, config).unsafeRunSync()

      room.users.unsafeRunSync() should be(Nil)

      room.addUser(user1).unsafeRunSync()
      room.addUser(user2).unsafeRunSync()
      room.addUser(user3).unsafeRunSync()
      room.addUser(user4).unsafeRunSync()
      room.addUser(user5).unsafeRunSync()

      room.users.unsafeRunSync() should contain allOf(user1, user2, user3, user4, user5)
    }
  }

  test("Fail if we pass in the wrong number of users") {
    forAll { (roomId: RoomId, config: GameConfig) =>

      val user1 = User(Nickname("Taylor"))
      val user2 = User(Nickname("Nick"))
      val user3 = User(Nickname("Chris"))
      val user4 = User(Nickname("Carter"))
      val user5 = User(Nickname("Austin"))

      val users = List(user1, user2, user3, user4, user5)

      val room = Room.build(mockRandomAlg, roomId, config).unsafeRunSync()

      room.addUser(user1).unsafeRunSync()
      room.addUser(user2).unsafeRunSync()
      room.addUser(user3).unsafeRunSync()
      room.addUser(user4).unsafeRunSync()
      room.addUser(user5).unsafeRunSync()

      room.startGame.unsafeRunSync()

      room.proposeMission(user1.nickname, users.take(3)).attempt.unsafeRunSync() should be(Left(InvalidUserCountForMission(3)))
    }
  }

  test("Fail if the wrong user tries to propose a mission") {
    forAll { (roomId: RoomId, config: GameConfig) =>

      val user1 = User(Nickname("Taylor"))
      val user2 = User(Nickname("Nick"))
      val user3 = User(Nickname("Chris"))
      val user4 = User(Nickname("Carter"))
      val user5 = User(Nickname("Austin"))

      val users = List(user1, user2, user3, user4, user5)

      val room = Room.build(mockRandomAlg, roomId, config).unsafeRunSync()

      room.addUser(user1).unsafeRunSync()
      room.addUser(user2).unsafeRunSync()
      room.addUser(user3).unsafeRunSync()
      room.addUser(user4).unsafeRunSync()
      room.addUser(user5).unsafeRunSync()

      room.startGame.unsafeRunSync()

      room.proposeMission(user2.nickname, users.take(2)).attempt.unsafeRunSync() should be(Left(UserNotMissionLeader(user2.nickname)))
    }
  }

  test("Successfully propose the mission if we have valid missionLeader and valid user count") {
    forAll { (roomId: RoomId, config: GameConfig) =>

      val user1 = User(Nickname("Taylor"))
      val user2 = User(Nickname("Nick"))
      val user3 = User(Nickname("Chris"))
      val user4 = User(Nickname("Carter"))
      val user5 = User(Nickname("Austin"))

      val users = List(user1, user2, user3, user4, user5)

      val room = Room.build(mockRandomAlg, roomId, config).unsafeRunSync()

      room.addUser(user1).unsafeRunSync()
      room.addUser(user2).unsafeRunSync()
      room.addUser(user3).unsafeRunSync()
      room.addUser(user4).unsafeRunSync()
      room.addUser(user5).unsafeRunSync()

      room.startGame.unsafeRunSync()

      val proposal = room.proposeMission(user1.nickname, users.take(2)).attempt.unsafeRunSync()

      proposal should be(Right(MissionProposal(1, user1.nickname, users.take(2))))
    }
  }
}