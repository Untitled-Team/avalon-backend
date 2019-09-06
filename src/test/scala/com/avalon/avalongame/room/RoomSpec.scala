package com.avalon.avalongame.room

import cats.effect.{ContextShift, IO, Timer}
import com.avalon.avalongame.common._
import com.avalon.avalongame.RandomAlg
import com.mrdziuban.ScalacheckMagnolia._
import org.scalatest.{FunSuite, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import Room._

class RoomSpec extends FunSuite with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {

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

      val user1 = Nickname("Taylor")
      val user2 = Nickname("Taylor")

      val room = Room.build(mockRandomAlg, roomId, config).unsafeRunSync()

      room.players.unsafeRunSync() should be(Nil)

      room.addUser(user1).unsafeRunSync()
      room.addUser(user2).attempt.unsafeRunSync() should be(Left(NicknameAlreadyInUse(user1)))

      room.players.unsafeRunSync() should be(List(user1))
    }
  }

  test("Properly add users") {
    forAll { (roomId: RoomId, config: GameConfig) =>

      val user1 = Nickname("Taylor")
      val user2 = Nickname("Nick")
      val user3 = Nickname("Chris")
      val user4 = Nickname("Carter")
      val user5 = Nickname("Austin")


      val room = Room.build(mockRandomAlg, roomId, config).unsafeRunSync()

      room.players.unsafeRunSync() should be(Nil)

      room.addUser(user1).unsafeRunSync()
      room.addUser(user2).unsafeRunSync()
      room.addUser(user3).unsafeRunSync()
      room.addUser(user4).unsafeRunSync()
      room.addUser(user5).unsafeRunSync()

      room.players.unsafeRunSync() should contain allOf(user1, user2, user3, user4, user5)
    }
  }

  test("Succeed at Starting the game and returning good guys and bad guys") {
    forAll { (roomId: RoomId, config: GameConfig) =>

      val user1 = Nickname("Taylor")
      val user2 = Nickname("Nick")
      val user3 = Nickname("Chris")
      val user4 = Nickname("Carter")
      val user5 = Nickname("Austin")

      val users = List(user1, user2, user3, user4, user5)

      val room = Room.build(mockRandomAlg, roomId, config).unsafeRunSync()

      room.addUser(user1).unsafeRunSync()
      room.addUser(user2).unsafeRunSync()
      room.addUser(user3).unsafeRunSync()
      room.addUser(user4).unsafeRunSync()
      room.addUser(user5).unsafeRunSync()

      val result = room.startGame.unsafeRunSync()

      result.badGuys should contain allOf(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy))
      result.goodGuys should contain allOf(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy))
    }
  }

  test("Succeed at allowing users to ready up after the game has been started") {
    forAll { (roomId: RoomId, config: GameConfig) =>

      val user1 = Nickname("Taylor")
      val user2 = Nickname("Nick")
      val user3 = Nickname("Chris")
      val user4 = Nickname("Carter")
      val user5 = Nickname("Austin")

      val users = List(user1, user2, user3, user4, user5)

      val room = Room.build(mockRandomAlg, roomId, config).unsafeRunSync()

      room.addUser(user1).unsafeRunSync()
      room.addUser(user2).unsafeRunSync()
      room.addUser(user3).unsafeRunSync()
      room.addUser(user4).unsafeRunSync()
      room.addUser(user5).unsafeRunSync()

      room.startGame.unsafeRunSync()

      val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

      room.playerReady(user1).unsafeRunSync() should be(NotReadyYet(List(user2, user3, user4, user5)))
      room.playerReady(user2).unsafeRunSync() should be(NotReadyYet(List(user3, user4, user5)))
      room.playerReady(user3).unsafeRunSync() should be(NotReadyYet(List(user4, user5)))
      room.playerReady(user4).unsafeRunSync() should be(NotReadyYet(List(user5)))
      room.playerReady(user5).unsafeRunSync() should be(AllReady(1, user1, missions))
    }
  }

  test("Fail if we pass in the wrong number of users") {
    forAll { (roomId: RoomId, config: GameConfig) =>

      val user1 = Nickname("Taylor")
      val user2 = Nickname("Nick")
      val user3 = Nickname("Chris")
      val user4 = Nickname("Carter")
      val user5 = Nickname("Austin")

      val users = List(user1, user2, user3, user4, user5)

      val room = Room.build(mockRandomAlg, roomId, config).unsafeRunSync()

      room.addUser(user1).unsafeRunSync()
      room.addUser(user2).unsafeRunSync()
      room.addUser(user3).unsafeRunSync()
      room.addUser(user4).unsafeRunSync()
      room.addUser(user5).unsafeRunSync()

      room.startGame.unsafeRunSync()

      val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

      room.playerReady(user1).unsafeRunSync() should be(NotReadyYet(List(user2, user3, user4, user5)))
      room.playerReady(user2).unsafeRunSync() should be(NotReadyYet(List(user3, user4, user5)))
      room.playerReady(user3).unsafeRunSync() should be(NotReadyYet(List(user4, user5)))
      room.playerReady(user4).unsafeRunSync() should be(NotReadyYet(List(user5)))
      room.playerReady(user5).unsafeRunSync() should be(AllReady(1, user1, missions))

      room.proposeMission(user1, users.take(3)).attempt.unsafeRunSync() should be(Left(InvalidUserCountForMission(3)))
    }
  }

//  test("Fail if the wrong user tries to propose a mission") {
//    forAll { (roomId: RoomId, config: GameConfig) =>
//
//      val user1 = Nickname("Taylor")
//      val user2 = Nickname("Nick")
//      val user3 = Nickname("Chris")
//      val user4 = Nickname("Carter")
//      val user5 = Nickname("Austin")
//
//      val users = List(user1, user2, user3, user4, user5)
//
//      val room = Room.build(mockRandomAlg, roomId, config).unsafeRunSync()
//
//      room.addUser(user1).unsafeRunSync()
//      room.addUser(user2).unsafeRunSync()
//      room.addUser(user3).unsafeRunSync()
//      room.addUser(user4).unsafeRunSync()
//      room.addUser(user5).unsafeRunSync()
//
//      room.startGame.unsafeRunSync()
//
//      room.proposeMission(user2, users.take(2)).attempt.unsafeRunSync() should be(Left(UserNotMissionLeader(user2)))
//    }
//  }
//
//  test("Successfully propose the mission if we have valid missionLeader and valid user count") {
//    forAll { (roomId: RoomId, config: GameConfig) =>
//
//      val user1 = Nickname("Taylor")
//      val user2 = Nickname("Nick")
//      val user3 = Nickname("Chris")
//      val user4 = Nickname("Carter")
//      val user5 = Nickname("Austin")
//
//      val users = List(user1, user2, user3, user4, user5)
//
//      val room = Room.build(mockRandomAlg, roomId, config).unsafeRunSync()
//
//      room.addUser(user1).unsafeRunSync()
//      room.addUser(user2).unsafeRunSync()
//      room.addUser(user3).unsafeRunSync()
//      room.addUser(user4).unsafeRunSync()
//      room.addUser(user5).unsafeRunSync()
//
//      room.startGame.unsafeRunSync()
//
//      val proposal = room.proposeMission(user1, users.take(2)).attempt.unsafeRunSync()
//
//      proposal should be(Right(MissionProposal(1, user1, users.take(2))))
//    }
//  }
}