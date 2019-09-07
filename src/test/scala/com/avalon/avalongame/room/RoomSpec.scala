package com.avalon.avalongame.room

import cats.effect.{ContextShift, IO, Timer}
import com.avalon.avalongame.common._
import com.avalon.avalongame.RandomAlg
import com.mrdziuban.ScalacheckMagnolia._
import org.scalatest.{FunSuite, Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import Room._

class RoomSpec extends WordSpec with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {

  implicit val cs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)
  implicit val t: Timer[IO] = IO.timer(scala.concurrent.ExecutionContext.Implicits.global)

  val mockRandomAlg: RandomAlg[IO] = new RandomAlg[IO] {
    override def shuffle[A](l: List[A]): IO[List[A]] = IO.pure(l)

    override def randomGet[A](l: List[A]): IO[A] = IO(l.head) //oops
  }

  "addUser" should {
    "Fail if we try to add a user who has the same nickname as another user in the room" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Taylor")

        val room = Room.build(mockRandomAlg, roomId).unsafeRunSync()

        room.players.unsafeRunSync() should be(Nil)

        room.addUser(user1).unsafeRunSync()
        room.addUser(user2).attempt.unsafeRunSync() should be(Left(NicknameAlreadyInUse(user1)))

        room.players.unsafeRunSync() should be(List(user1))
      }
    }

    "Fail to add user when the game has started" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")


        val room = Room.build(mockRandomAlg, roomId).unsafeRunSync()

        room.players.unsafeRunSync() should be(Nil)

        room.addUser(user1).unsafeRunSync()
        room.addUser(user2).unsafeRunSync()
        room.addUser(user3).unsafeRunSync()
        room.addUser(user4).unsafeRunSync()
        room.addUser(user5).unsafeRunSync()

        room.players.unsafeRunSync() should contain allOf(user1, user2, user3, user4, user5)

        room.startGame.unsafeRunSync()

        room.addUser(Nickname("new user")).attempt.unsafeRunSync() should be(Left(GameHasStarted))
      }
    }

    "Properly add users" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")


        val room = Room.build(mockRandomAlg, roomId).unsafeRunSync()

        room.players.unsafeRunSync() should be(Nil)

        room.addUser(user1).unsafeRunSync()
        room.addUser(user2).unsafeRunSync()
        room.addUser(user3).unsafeRunSync()
        room.addUser(user4).unsafeRunSync()
        room.addUser(user5).unsafeRunSync()

        room.players.unsafeRunSync() should contain allOf(user1, user2, user3, user4, user5)
      }
    }
  }

  "startGame" should {
    "Fail when we try to start a game with fewer than 5 players" in {
      forAll { (roomId: RoomId, config: GameConfig) =>
        val room = Room.build(mockRandomAlg, roomId).unsafeRunSync()

        room.startGame.attempt.unsafeRunSync() should be(Left(NotEnoughPlayers(0)))
      }
    }

    "Succeed at Starting the game and returning good guys and bad guys" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val room = Room.build(mockRandomAlg, roomId).unsafeRunSync()

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
  }

  "playerReady" should {
    "Succeed at allowing users to ready up after the game has been started" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val room = Room.build(mockRandomAlg, roomId).unsafeRunSync()

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
  }

  "proposeMission" should {
    "Fail if we pass in the wrong number of users" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val room = Room.build(mockRandomAlg, roomId).unsafeRunSync()

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


    "Fail if the wrong user tries to propose a mission" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val room = Room.build(mockRandomAlg, roomId).unsafeRunSync()

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

        room.proposeMission(user2, users.take(2)).attempt.unsafeRunSync() should be(Left(UserNotMissionLeader(user2)))
      }
    }


    "Fail if try to propose a mission before starting game" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val room = Room.build(mockRandomAlg, roomId).unsafeRunSync()

        room.addUser(user1).unsafeRunSync()
        room.addUser(user2).unsafeRunSync()
        room.addUser(user3).unsafeRunSync()
        room.addUser(user4).unsafeRunSync()
        room.addUser(user5).unsafeRunSync()

        room.proposeMission(user2, users.take(2)).attempt.unsafeRunSync() should be(Left(GameNotStarted))
      }
    }

    "Fail if try to propose the mission from the wrong state" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val room = Room.build(mockRandomAlg, roomId).unsafeRunSync()

        room.addUser(user1).unsafeRunSync()
        room.addUser(user2).unsafeRunSync()
        room.addUser(user3).unsafeRunSync()
        room.addUser(user4).unsafeRunSync()
        room.addUser(user5).unsafeRunSync()

        room.startGame.unsafeRunSync()

        room.proposeMission(user2, users.take(2)).attempt.unsafeRunSync() should be(Left(InvalidStateTransition(PlayersReadingRole(Nil), "proposeMission", user2)))
      }
    }

    "Successfully propose the mission if we have valid missionLeader and valid user count" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val room = Room.build(mockRandomAlg, roomId).unsafeRunSync()

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

        val proposal = room.proposeMission(user1, users.take(2)).attempt.unsafeRunSync()

        proposal should be(Right(MissionProposal(1, user1, users.take(2))))
      }
    }
  }

  "teamVote" should {
    "fail if we call it from the wrong state" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val room = Room.build(mockRandomAlg, roomId).unsafeRunSync()

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

        val proposal = room.teamVote(user1, TeamVote(false)).attempt.unsafeRunSync()

        proposal should be(Left(InvalidStateTransition(MissionProposing(1, user1), "teamVote", user1)))
      }
    }

    "return StillVoting if we're still waiting for others to vote" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

        val gr = GameRepresentation(
          MissionVoting(1, user1, List(user1, user2), Nil),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy)),
          users)

        val room = Room.buildPrivate(mockRandomAlg, roomId, Some(gr), users).unsafeRunSync()

        val proposal = room.teamVote(user1, TeamVote(false)).unsafeRunSync()

        proposal should be(StillVoting)
      }
    }

    "return FailedVote if we're still waiting for others to vote" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

        val gr = GameRepresentation(
          MissionVoting(1, user1, List(user1, user2), Nil),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy)),
          users)

        val room = Room.buildPrivate(mockRandomAlg, roomId, Some(gr), users).unsafeRunSync()

        val result1 = room.teamVote(user1, TeamVote(false)).unsafeRunSync()
        val result2 = room.teamVote(user2, TeamVote(false)).unsafeRunSync()
        val result3 = room.teamVote(user3, TeamVote(false)).unsafeRunSync()
        val result4 = room.teamVote(user4, TeamVote(false)).unsafeRunSync()
        val result5 = room.teamVote(user5, TeamVote(false)).unsafeRunSync()

        result1 should be(StillVoting)
        result2 should be(StillVoting)
        result3 should be(StillVoting)
        result4 should be(StillVoting)
        result5 should be(
          FailedVote(
            user2,
            List(
              PlayerTeamVote(user1, TeamVote(false)),
              PlayerTeamVote(user2, TeamVote(false)),
              PlayerTeamVote(user3, TeamVote(false)),
              PlayerTeamVote(user4, TeamVote(false)),
              PlayerTeamVote(user5, TeamVote(false)),
            )))
      }
    }

    "return SuccessfulVote if we have majority true votes" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

        val gr = GameRepresentation(
          MissionVoting(1, user1, List(user1, user2), Nil),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy)),
          users)

        val room = Room.buildPrivate(mockRandomAlg, roomId, Some(gr), users).unsafeRunSync()

        val result1 = room.teamVote(user1, TeamVote(true)).unsafeRunSync()
        val result2 = room.teamVote(user2, TeamVote(true)).unsafeRunSync()
        val result3 = room.teamVote(user3, TeamVote(true)).unsafeRunSync()
        val result4 = room.teamVote(user4, TeamVote(false)).unsafeRunSync()
        val result5 = room.teamVote(user5, TeamVote(false)).unsafeRunSync()

        result1 should be(StillVoting)
        result2 should be(StillVoting)
        result3 should be(StillVoting)
        result4 should be(StillVoting)
        result5 should be(
          SuccessfulVote(
            List(
              PlayerTeamVote(user1, TeamVote(true)),
              PlayerTeamVote(user2, TeamVote(true)),
              PlayerTeamVote(user3, TeamVote(true)),
              PlayerTeamVote(user4, TeamVote(false)),
              PlayerTeamVote(user5, TeamVote(false)),
            )))
      }
    }

    "fail when there is a tied vote" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")
        val user6 = Nickname("Liran")

        val users = List(user1, user2, user3, user4, user5, user6)

        val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

        val gr = GameRepresentation(
          MissionVoting(1, user1, List(user1, user2), Nil),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy), GoodPlayerRole(user6, NormalGoodGuy)),
          users)

        val room = Room.buildPrivate(mockRandomAlg, roomId, Some(gr), users).unsafeRunSync()

        val result1 = room.teamVote(user1, TeamVote(true)).unsafeRunSync()
        val result2 = room.teamVote(user2, TeamVote(true)).unsafeRunSync()
        val result3 = room.teamVote(user3, TeamVote(true)).unsafeRunSync()
        val result4 = room.teamVote(user4, TeamVote(false)).unsafeRunSync()
        val result5 = room.teamVote(user5, TeamVote(false)).unsafeRunSync()
        val result6 = room.teamVote(user6, TeamVote(false)).unsafeRunSync()

        result1 should be(StillVoting)
        result2 should be(StillVoting)
        result3 should be(StillVoting)
        result4 should be(StillVoting)
        result5 should be(StillVoting)
        result6 should be(
          FailedVote(
            user2,
            List(
              PlayerTeamVote(user1, TeamVote(true)),
              PlayerTeamVote(user2, TeamVote(true)),
              PlayerTeamVote(user3, TeamVote(true)),
              PlayerTeamVote(user4, TeamVote(false)),
              PlayerTeamVote(user5, TeamVote(false)),
              PlayerTeamVote(user6, TeamVote(false)),
            )))
      }
    }

    "be able to propose a mission after a failed vote" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")
        val user6 = Nickname("Liran")

        val users = List(user1, user2, user3, user4, user5, user6)

        val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

        val gr = GameRepresentation(
          MissionVoting(1, user1, List(user1, user2), Nil),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy), GoodPlayerRole(user6, NormalGoodGuy)),
          users)

        val room = Room.buildPrivate(mockRandomAlg, roomId, Some(gr), users).unsafeRunSync()

        val result1 = room.teamVote(user1, TeamVote(true)).unsafeRunSync()
        val result2 = room.teamVote(user2, TeamVote(true)).unsafeRunSync()
        val result3 = room.teamVote(user3, TeamVote(true)).unsafeRunSync()
        val result4 = room.teamVote(user4, TeamVote(false)).unsafeRunSync()
        val result5 = room.teamVote(user5, TeamVote(false)).unsafeRunSync()
        val result6 = room.teamVote(user6, TeamVote(false)).unsafeRunSync()

        result1 should be(StillVoting)
        result2 should be(StillVoting)
        result3 should be(StillVoting)
        result4 should be(StillVoting)
        result5 should be(StillVoting)
        result6 should be(
          FailedVote(
            user2,
            List(
              PlayerTeamVote(user1, TeamVote(true)),
              PlayerTeamVote(user2, TeamVote(true)),
              PlayerTeamVote(user3, TeamVote(true)),
              PlayerTeamVote(user4, TeamVote(false)),
              PlayerTeamVote(user5, TeamVote(false)),
              PlayerTeamVote(user6, TeamVote(false)),
            )))

        val proposal = room.proposeMission(user2, users.take(2)).attempt.unsafeRunSync()

        proposal should be(Right(MissionProposal(1, user2, users.take(2))))
      }
    }
  }
}