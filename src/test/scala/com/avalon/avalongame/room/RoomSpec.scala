package com.avalon.avalongame.room

import cats.effect.{ContextShift, IO, Timer}
import com.avalon.avalongame.Arbitraries._
import com.avalon.avalongame.common._
import com.avalon.avalongame.RandomAlg
import com.mrdziuban.ScalacheckMagnolia._
import org.scalatest.{FunSuite, Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import Room._
import cats.Eq
import cats.effect.concurrent.MVar
import io.chrisdavenport.fuuid.FUUID

class RoomSpec extends WordSpec with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {

  implicit val cs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)
  implicit val t: Timer[IO] = IO.timer(scala.concurrent.ExecutionContext.Implicits.global)

  val mockRandomAlg: RandomAlg[IO] = new RandomAlg[IO] {
    override def shuffle[A](l: List[A]): IO[List[A]] = IO.pure(l)
    override def randomGet[A](l: List[A]): IO[A] = IO(l.head) //oops
    override def clockwise[A: Eq](previous: A, l: List[A]): IO[A] = IO(l.head)
    def fuuid: IO[FUUID] = ???
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

    "Fail if we try to add more than 10 users. " in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")
        val user6 = Nickname("zsdaf")
        val user7 = Nickname("zdsf")
        val user8 = Nickname("sdfs")
        val user9 = Nickname("Carzxcvsdfter")
        val user10 = Nickname("asdfsaf")

        val internalRoom: MVar[IO, InternalRoom] =
          MVar.of[IO, InternalRoom] {
            InternalRoom(
              List(user1, user2, user3, user4, user5, user6, user7, user8, user9, user10), None)
          }.unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, internalRoom)

        room.players.unsafeRunSync().size should be(10)
        room.addUser(Nickname("Failed")).attempt.unsafeRunSync() should be(Left(RoomIsFull(roomId)))
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

  "removePlayer" should {
    "Fail if the player isn't in the room at all" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")

        val room = Room.build(mockRandomAlg, roomId).unsafeRunSync()

        room.players.unsafeRunSync() should be(Nil)

        room.addUser(user1).unsafeRunSync()
        room.players.unsafeRunSync() should be(List(user1))
        room.removePlayer(Nickname("Not in room"))
        room.players.unsafeRunSync() should be(List(user1))
      }
    }

    "Fail to remove user when the game has started" in {
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

        room.removePlayer(user1).attempt.unsafeRunSync() should be(Left(GameHasStarted))
      }
    }

    "Properly remove users" in {
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

        room.removePlayer(user1).unsafeRunSync()
        room.removePlayer(user2).unsafeRunSync()
        room.removePlayer(user3).unsafeRunSync()
        room.removePlayer(user4).unsafeRunSync()
        room.removePlayer(user5).unsafeRunSync()

        room.players.unsafeRunSync() should be(Nil)
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

        val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

        room.addUser(user1).unsafeRunSync()
        room.addUser(user2).unsafeRunSync()
        room.addUser(user3).unsafeRunSync()
        room.addUser(user4).unsafeRunSync()
        room.addUser(user5).unsafeRunSync()

        val result = room.startGame.unsafeRunSync()

        result.roles.badGuys should contain allOf(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy))
        result.roles.goodGuys should contain allOf(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy))
        result.startingState.missionLeader should be(user1)
        result.startingState.missions should be(missions)
        result.startingState.missionNumber should be(1)
      }
    }
  }

  //add test for transition from invalid state
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

        val proposal = room.proposeMission(user1, users.take(2)).attempt.unsafeRunSync()

        proposal should be(Right(MissionProposal(1, user1, users.take(2), 4, user1)))
      }
    }
  }

  "teamVote" should { //make sure the mission is updated with the correct mission leader!!!!
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

        val proposal = room.teamVote(user1, TeamVote(false)).attempt.unsafeRunSync()

        proposal should be(Left(InvalidStateTransition(MissionProposing(1, user1, 5), "teamVote", user1)))
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
          MissionVoting(1, user1, List(user1, user2), Nil, 5),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy)),
          users)

        val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)

        val proposal = room.teamVote(user1, TeamVote(false)).unsafeRunSync()

        proposal should be(Right(TeamPhaseStillVoting))
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
          MissionVoting(1, user1, List(user1, user2), Nil, 5),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy)),
          users)

        val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)

        val result1 = room.teamVote(user1, TeamVote(false)).unsafeRunSync()
        val result2 = room.teamVote(user2, TeamVote(false)).unsafeRunSync()
        val result3 = room.teamVote(user3, TeamVote(false)).unsafeRunSync()
        val result4 = room.teamVote(user4, TeamVote(false)).unsafeRunSync()
        val result5 = room.teamVote(user5, TeamVote(false)).unsafeRunSync()


        val votes = List(
          PlayerTeamVote(user1, TeamVote(false)),
          PlayerTeamVote(user2, TeamVote(false)),
          PlayerTeamVote(user3, TeamVote(false)),
          PlayerTeamVote(user4, TeamVote(false)),
          PlayerTeamVote(user5, TeamVote(false)))

        val missionsAfterVotes = IO.fromEither(Missions.addFinishedTeamVote(missions, 1, user1, votes)).unsafeRunSync()

        result1 should be(Right(TeamPhaseStillVoting))
        result2 should be(Right(TeamPhaseStillVoting))
        result3 should be(Right(TeamPhaseStillVoting))
        result4 should be(Right(TeamPhaseStillVoting))
        result5 should be(Right(FailedVote(user1, 1, votes, missionsAfterVotes, user1, 4)))
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
          MissionVoting(1, user1, List(user1, user2), Nil, 5),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy)),
          users)

        val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)

        val result1 = room.teamVote(user1, TeamVote(true)).unsafeRunSync()
        val result2 = room.teamVote(user2, TeamVote(true)).unsafeRunSync()
        val result3 = room.teamVote(user3, TeamVote(true)).unsafeRunSync()
        val result4 = room.teamVote(user4, TeamVote(false)).unsafeRunSync()
        val result5 = room.teamVote(user5, TeamVote(false)).unsafeRunSync()

        val votes = List(
          PlayerTeamVote(user1, TeamVote(true)),
          PlayerTeamVote(user2, TeamVote(true)),
          PlayerTeamVote(user3, TeamVote(true)),
          PlayerTeamVote(user4, TeamVote(false)),
          PlayerTeamVote(user5, TeamVote(false)))

        result1 should be(Right(TeamPhaseStillVoting))
        result2 should be(Right(TeamPhaseStillVoting))
        result3 should be(Right(TeamPhaseStillVoting))
        result4 should be(Right(TeamPhaseStillVoting))
        result5 should be(Right(SuccessfulVote(votes)))

        val repr = mvar.read.unsafeRunSync().gameRepresentation.get
        repr.missions.one.votes should be(List(FinishedTeamVote(user1, votes)))
        repr.missions.one.players.get should be(List(user1, user2))
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
          MissionVoting(1, user1, List(user1, user2), Nil, 5),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy), GoodPlayerRole(user6, NormalGoodGuy)),
          users)

        val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)

        val result1 = room.teamVote(user1, TeamVote(true)).unsafeRunSync()
        val result2 = room.teamVote(user2, TeamVote(true)).unsafeRunSync()
        val result3 = room.teamVote(user3, TeamVote(true)).unsafeRunSync()
        val result4 = room.teamVote(user4, TeamVote(false)).unsafeRunSync()
        val result5 = room.teamVote(user5, TeamVote(false)).unsafeRunSync()
        val result6 = room.teamVote(user6, TeamVote(false)).unsafeRunSync()

        val votes = List(
          PlayerTeamVote(user1, TeamVote(true)),
          PlayerTeamVote(user2, TeamVote(true)),
          PlayerTeamVote(user3, TeamVote(true)),
          PlayerTeamVote(user4, TeamVote(false)),
          PlayerTeamVote(user5, TeamVote(false)),
          PlayerTeamVote(user6, TeamVote(false)))

        val missionsAfterVotes = IO.fromEither(Missions.addFinishedTeamVote(missions, 1, user1, votes)).unsafeRunSync()

        result1 should be(Right(TeamPhaseStillVoting))
        result2 should be(Right(TeamPhaseStillVoting))
        result3 should be(Right(TeamPhaseStillVoting))
        result4 should be(Right(TeamPhaseStillVoting))
        result5 should be(Right(TeamPhaseStillVoting))
        result6 should be(Right(FailedVote(user1, 1, votes, missionsAfterVotes, user1, 4)))
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
          MissionVoting(1, user1, List(user1, user2), Nil, 5),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy), GoodPlayerRole(user6, NormalGoodGuy)),
          users)

        val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)

        val result1 = room.teamVote(user1, TeamVote(true)).unsafeRunSync()
        val result2 = room.teamVote(user2, TeamVote(true)).unsafeRunSync()
        val result3 = room.teamVote(user3, TeamVote(true)).unsafeRunSync()
        val result4 = room.teamVote(user4, TeamVote(false)).unsafeRunSync()
        val result5 = room.teamVote(user5, TeamVote(false)).unsafeRunSync()
        val result6 = room.teamVote(user6, TeamVote(false)).unsafeRunSync()

        val votes = List(
          PlayerTeamVote(user1, TeamVote(true)),
          PlayerTeamVote(user2, TeamVote(true)),
          PlayerTeamVote(user3, TeamVote(true)),
          PlayerTeamVote(user4, TeamVote(false)),
          PlayerTeamVote(user5, TeamVote(false)),
          PlayerTeamVote(user6, TeamVote(false)))

        val missionsAfterVotes = IO.fromEither(Missions.addFinishedTeamVote(missions, 1, user1, votes)).unsafeRunSync()

        result1 should be(Right(TeamPhaseStillVoting))
        result2 should be(Right(TeamPhaseStillVoting))
        result3 should be(Right(TeamPhaseStillVoting))
        result4 should be(Right(TeamPhaseStillVoting))
        result5 should be(Right(TeamPhaseStillVoting))
        result6 should be(Right(FailedVote(user1, 1, votes, missionsAfterVotes, user1, 4)))

        val proposal = room.proposeMission(user1, users.take(2)).attempt.unsafeRunSync()

        proposal should be(Right(MissionProposal(1, user1, users.take(2), 3, user1)))
      }
    }

    "Return GameOver when 5 failed votes have happened" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

        val gr = GameRepresentation(
          MissionVoting(1, user1, List(user1, user2), Nil, 5),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy)),
          users)

        val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)

        room.teamVote(user1, TeamVote(true)).unsafeRunSync()
        room.teamVote(user2, TeamVote(true)).unsafeRunSync()
        room.teamVote(user3, TeamVote(false)).unsafeRunSync()
        room.teamVote(user4, TeamVote(false)).unsafeRunSync()
        room.teamVote(user5, TeamVote(false)).unsafeRunSync()

        val proposal2 = room.proposeMission(user1, users.take(2)).attempt.unsafeRunSync()

        proposal2 should be(Right(MissionProposal(1, user1, users.take(2), 3, user1)))

        room.teamVote(user1, TeamVote(true)).unsafeRunSync()
        room.teamVote(user2, TeamVote(true)).unsafeRunSync()
        room.teamVote(user3, TeamVote(false)).unsafeRunSync()
        room.teamVote(user4, TeamVote(false)).unsafeRunSync()
        room.teamVote(user5, TeamVote(false)).unsafeRunSync()

        val proposal3 = room.proposeMission(user1, users.take(2)).attempt.unsafeRunSync()

        proposal3 should be(Right(MissionProposal(1, user1, users.take(2), 2, user1)))

        room.teamVote(user1, TeamVote(true)).unsafeRunSync()
        room.teamVote(user2, TeamVote(true)).unsafeRunSync()
        room.teamVote(user3, TeamVote(false)).unsafeRunSync()
        room.teamVote(user4, TeamVote(false)).unsafeRunSync()
        room.teamVote(user5, TeamVote(false)).unsafeRunSync()

        val proposal4 = room.proposeMission(user1, users.take(2)).attempt.unsafeRunSync()

        proposal4 should be(Right(MissionProposal(1, user1, users.take(2), 1, user1)))

        room.teamVote(user1, TeamVote(true)).unsafeRunSync()
        room.teamVote(user2, TeamVote(true)).unsafeRunSync()
        room.teamVote(user3, TeamVote(false)).unsafeRunSync()
        room.teamVote(user4, TeamVote(false)).unsafeRunSync()
        room.teamVote(user5, TeamVote(false)).unsafeRunSync()

        val proposal5 = room.proposeMission(user1, users.take(2)).attempt.unsafeRunSync()

        proposal5 should be(Right(MissionProposal(1, user1, users.take(2), 0, user1)))

        room.teamVote(user1, TeamVote(true)).unsafeRunSync()
        room.teamVote(user2, TeamVote(true)).unsafeRunSync()
        room.teamVote(user3, TeamVote(false)).unsafeRunSync()
        room.teamVote(user4, TeamVote(false)).unsafeRunSync()
        val finalVote = room.teamVote(user5, TeamVote(false)).unsafeRunSync()

        finalVote should be(Left(Room.badGuysWinGameOver[IO](gr).unsafeRunSync))
      }
    }
  }

  "quest vote" should {
    "fail if someone who isn't a quester tries to vote" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

        val gr = GameRepresentation(
          QuestPhase(1, user1, List(user1, user2), Nil),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy)),
          users)

        val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)

        room.questVote(user5, QuestVote(true)).attempt.unsafeRunSync() should be(Left(PlayerNotPartOfQuest(user5)))
      }
    }

    "fail if someone who has already voted tries to vote again" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

        val gr = GameRepresentation(
          QuestPhase(1, user1, List(user1, user2), Nil),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy)),
          users)

        val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)

        room.questVote(user1, QuestVote(true)).unsafeRunSync() should be(QuestPhaseStillVoting)
        room.questVote(user1, QuestVote(true)).attempt.unsafeRunSync() should be(Left(PlayerCantVoteMoreThanOnce(user1)))
      }
    }

    "Move to next mission if no GameOver condition is met" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

        val updatedMissions = {
          val default = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          IO.fromEither(Missions.completeMission(default, 1, QuestVote(false))).unsafeRunSync()
        }

        val gr = GameRepresentation(
          QuestPhase(1, user1, List(user1, user2), Nil),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy)),
          users)

        val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)

        room.questVote(user1, QuestVote(true)).unsafeRunSync() should be(QuestPhaseStillVoting)
        room.questVote(user2, QuestVote(false)).unsafeRunSync() should be(
          FinishedVote(List(QuestVote(true), QuestVote(false)), GameContinues(user1, 2, updatedMissions, user1, 5)))

        val repr = mvar.read.unsafeRunSync().gameRepresentation.get
        val completedMission = IO.fromEither(Missions.fromInt(repr.missions, 1)).unsafeRunSync()
        completedMission.pass should be(Some(QuestVote(false)))
        repr.state should be(MissionProposing(2, user1, 5))
      }
    }

    "Fail quest if even one person fail the quest" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")
        val user6 = Nickname("Kelsey")
        val user7 = Nickname("Adrian")

        val users = List(user1, user2, user3, user4, user5, user6, user7)

        val missions = IO.fromEither(Missions.fromPlayers(7)).unsafeRunSync()

        val updatedMissions = {
          val default = IO.fromEither(Missions.fromPlayers(7)).unsafeRunSync()

          IO.fromEither(Missions.completeMission(default, 1, QuestVote(false))).unsafeRunSync()
        }

        val gr = GameRepresentation(
          QuestPhase(1, user1, List(user1, user2), Nil),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          List(
            GoodPlayerRole(user3, Merlin),
            GoodPlayerRole(user4, NormalGoodGuy),
            GoodPlayerRole(user5, NormalGoodGuy),
            GoodPlayerRole(user6, NormalGoodGuy),
            GoodPlayerRole(user7, NormalGoodGuy)),
          users)

        val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)

        room.questVote(user1, QuestVote(true)).unsafeRunSync() should be(QuestPhaseStillVoting)
        room.questVote(user2, QuestVote(false)).unsafeRunSync() should be(
          FinishedVote(List(QuestVote(true), QuestVote(false)), GameContinues(user1, 2, updatedMissions, user1, 5)))

        val repr = mvar.read.unsafeRunSync().gameRepresentation.get
        val completedMission = IO.fromEither(Missions.fromInt(repr.missions, 1)).unsafeRunSync()
        completedMission.pass should be(Some(QuestVote(false)))

        repr.state should be(MissionProposing(2, user1, 5))
      }
    }

    "Pass quest if 2 people don't fail on the 4th quest with 7 or more players" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")
        val user6 = Nickname("Kelsey")
        val user7 = Nickname("Adrian")

        val users = List(user1, user2, user3, user4, user5, user6, user7)

        val missions = {
          val default = IO.fromEither(Missions.fromPlayers(7)).unsafeRunSync()

          val updateMissions1 = IO.fromEither(Missions.completeMission(default, 1, QuestVote(false))).unsafeRunSync()
          val updateMissions2 = IO.fromEither(Missions.completeMission(updateMissions1, 2, QuestVote(false))).unsafeRunSync()
          IO.fromEither(Missions.completeMission(updateMissions2, 3, QuestVote(true))).unsafeRunSync()
        }

        val updatedMissions =
          IO.fromEither(Missions.completeMission(missions, 4, QuestVote(true))).unsafeRunSync()

        val gr = GameRepresentation(
          QuestPhase(4, user1, List(user1, user2, user3, user4), Nil),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy), BadPlayerRole(user3, NormalBadGuy)),
          List(GoodPlayerRole(user4, Merlin), GoodPlayerRole(user5, NormalGoodGuy), GoodPlayerRole(user6, NormalGoodGuy), GoodPlayerRole(user7, NormalGoodGuy)),
          users)

        val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)

        room.questVote(user1, QuestVote(false)).unsafeRunSync() should be(QuestPhaseStillVoting)
        room.questVote(user2, QuestVote(true)).unsafeRunSync() should be(QuestPhaseStillVoting)
        room.questVote(user3, QuestVote(true)).unsafeRunSync() should be(QuestPhaseStillVoting)
        room
          .questVote(user4, QuestVote(true))
          .unsafeRunSync() should be(FinishedVote(
          List(QuestVote(false), QuestVote(true), QuestVote(true), QuestVote(true)), GameContinues(user1, 5, updatedMissions, user1, 5)))

        val repr = mvar.read.unsafeRunSync().gameRepresentation.get
        val completedMission = IO.fromEither(Missions.fromInt(repr.missions, 4)).unsafeRunSync()
        completedMission.pass should be(Some(QuestVote(true)))

        repr.state should be(MissionProposing(5, user1, 5))
      }
    }

    "Fail quest if 2 people fail on the 4th quest with 7 or more players" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")
        val user6 = Nickname("Kelsey")
        val user7 = Nickname("Adrian")

        val users = List(user1, user2, user3, user4, user5, user6, user7)

        val missions = {
          val default = IO.fromEither(Missions.fromPlayers(7)).unsafeRunSync()

          val updateMissions1 = IO.fromEither(Missions.completeMission(default, 1, QuestVote(true))).unsafeRunSync()
          val updateMissions2 = IO.fromEither(Missions.completeMission(updateMissions1, 2, QuestVote(false))).unsafeRunSync()
          IO.fromEither(Missions.completeMission(updateMissions2, 3, QuestVote(true))).unsafeRunSync()
        }

        val updatedMissions =
          IO.fromEither(Missions.completeMission(missions, 4, QuestVote(false))).unsafeRunSync()

        val gr = GameRepresentation(
          QuestPhase(4, user1, List(user1, user2, user3, user4), Nil),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy), BadPlayerRole(user3, NormalBadGuy)),
          List(GoodPlayerRole(user4, Merlin), GoodPlayerRole(user5, NormalGoodGuy), GoodPlayerRole(user6, NormalGoodGuy), GoodPlayerRole(user7, NormalGoodGuy)),
          users)

        val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)

        room.questVote(user1, QuestVote(false)).unsafeRunSync() should be(QuestPhaseStillVoting)
        room.questVote(user2, QuestVote(false)).unsafeRunSync() should be(QuestPhaseStillVoting)
        room.questVote(user3, QuestVote(true)).unsafeRunSync() should be(QuestPhaseStillVoting)
        room
          .questVote(user4, QuestVote(true))
          .unsafeRunSync() should be(FinishedVote(
          List(QuestVote(false), QuestVote(false), QuestVote(true), QuestVote(true)),
          GameContinues(user1, 5, updatedMissions, user1, 5)))

        val repr = mvar.read.unsafeRunSync().gameRepresentation.get
        val completedMission = IO.fromEither(Missions.fromInt(repr.missions, 4)).unsafeRunSync()
        completedMission.pass should be(Some(QuestVote(false)))

        repr.state should be(MissionProposing(5, user1, 5))
      }
    }

    "Good guys cannot fail a mission even if they try" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()
        val updatedMissions =
          IO.fromEither(Missions.completeMission(missions, 1, QuestVote(true))).unsafeRunSync()

        val gr = GameRepresentation(
          QuestPhase(1, user1, List(user3, user4), Nil),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy)),
          users)

        val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)

        room.questVote(user3, QuestVote(false)).unsafeRunSync() should be(QuestPhaseStillVoting)
        room.questVote(user4, QuestVote(false)).unsafeRunSync() should be(FinishedVote(
          List(QuestVote(true), QuestVote(true)), GameContinues(user1, 2, updatedMissions, user1, 5)))

        val repr = mvar.read.unsafeRunSync().gameRepresentation.get
        val completedMission = IO.fromEither(Missions.fromInt(repr.missions, 1)).unsafeRunSync()
        completedMission.pass should be(Some(QuestVote(true)))

        repr.state should be(MissionProposing(2, user1, 5))
      }
    }

    "set Room in BadGuysWinState when third failed mission happens" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val missions = {
          val default = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val updateMissions1 = IO.fromEither(Missions.completeMission(default, 1, QuestVote(false))).unsafeRunSync()
          IO.fromEither(Missions.completeMission(updateMissions1, 2, QuestVote(false))).unsafeRunSync()
        }

        val goodPlayerRoles = List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy))
        val badPlayerRoles = List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy))

        val gr = GameRepresentation(
          QuestPhase(3, user1, List(user1, user2), Nil),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy)),
          users)

        val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)

        room.questVote(user1, QuestVote(false)).unsafeRunSync() should be(QuestPhaseStillVoting)
        room.questVote(user2, QuestVote(false)).unsafeRunSync() should be(FinishedVote(
          List(QuestVote(false), QuestVote(false)),
          BadGuyVictory(BadPlayerRole(user1, Assassin), None, GoodPlayerRole(user3, Merlin), goodPlayerRoles, badPlayerRoles, BadGuys)))

        val repr = mvar.read.unsafeRunSync().gameRepresentation.get
        val completedMission = IO.fromEither(Missions.fromInt(repr.missions, 3)).unsafeRunSync()
        completedMission.pass should be(Some(QuestVote(false)))

        repr.state should be(BadSideWins)
      }
    }

    "set Room in AssassinNeedsToVote when third failed mission happens" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val missions = {
          val default = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val updateMissions1 = IO.fromEither(Missions.completeMission(default, 1, QuestVote(true))).unsafeRunSync()
          IO.fromEither(Missions.completeMission(updateMissions1, 2, QuestVote(true))).unsafeRunSync()
        }

        val updatedMissions =
          IO.fromEither(Missions.completeMission(missions, 3, QuestVote(true))).unsafeRunSync()

        val goodPlayerRoles = List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy))

        val gr = GameRepresentation(
          QuestPhase(3, user1, List(user1, user2), Nil),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy)),
          users)

        val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)

        room.questVote(user1, QuestVote(true)).unsafeRunSync() should be(QuestPhaseStillVoting)
        room.questVote(user2, QuestVote(true)).unsafeRunSync() should be(FinishedVote(List(
          QuestVote(true), QuestVote(true)), AssassinVote(user1, goodPlayerRoles, updatedMissions)))

        val repr = mvar.read.unsafeRunSync().gameRepresentation.get
        val completedMission = IO.fromEither(Missions.fromInt(repr.missions, 3)).unsafeRunSync()
        completedMission.pass should be(Some(QuestVote(true)))

        repr.state should be(AssassinVoteState)
      }
    }

    "return appropriate next mission" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val missions = {
          val default = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val updateMissions1 = IO.fromEither(Missions.completeMission(default, 1, QuestVote(true))).unsafeRunSync()
          IO.fromEither(Missions.completeMission(updateMissions1, 2, QuestVote(false))).unsafeRunSync()
        }
        val updatedMissions =
          IO.fromEither(Missions.completeMission(missions, 3, QuestVote(true))).unsafeRunSync()

        val goodPlayerRoles = List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy))

        val gr = GameRepresentation(
          QuestPhase(123, user1, List(user1, user2), Nil),
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          goodPlayerRoles,
          users)

        val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)

        room.questVote(user1, QuestVote(true)).unsafeRunSync() should be(QuestPhaseStillVoting)
        room.questVote(user2, QuestVote(true)).unsafeRunSync() should be(FinishedVote(List(
          QuestVote(true), QuestVote(true)), GameContinues(user1, 4, updatedMissions, user1, 5)))

        val repr = mvar.read.unsafeRunSync().gameRepresentation.get
        val completedMission = IO.fromEither(Missions.fromInt(repr.missions, 1)).unsafeRunSync()
        completedMission.pass should be(Some(QuestVote(true)))

        val completedMission2 = IO.fromEither(Missions.fromInt(repr.missions, 2)).unsafeRunSync()
        completedMission2.pass should be(Some(QuestVote(false)))

        val completedMission3 = IO.fromEither(Missions.fromInt(repr.missions, 3)).unsafeRunSync()
        completedMission3.pass should be(Some(QuestVote(true)))

        repr.state should be(MissionProposing(4, user1, 5))
      }
    }
  }

//    "be valid when GameContinues is returned" should {
//      "tell us the GameContinues when everyone has viewed results - and our state was NextMission" in {
//        forAll { (roomId: RoomId, config: GameConfig) =>
//
//          val user1 = Nickname("Taylor")
//          val user2 = Nickname("Nick")
//          val user3 = Nickname("Chris")
//          val user4 = Nickname("Carter")
//          val user5 = Nickname("Austin")
//
//          val users = List(user1, user2, user3, user4, user5)
//
//          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()
//
//          val goodPlayerRoles = List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy))
//
//          val gr = GameRepresentation(
//            QuestResultsViewing(NextMission(user1), Nil),
//            missions,
//            List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
//            goodPlayerRoles,
//            users)
//
//          val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()
//
//          val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)
//
//          room.questResultsSeen(user1).unsafeRunSync() should be(StillViewingQuestResults)
//          room.questResultsSeen(user2).unsafeRunSync() should be(StillViewingQuestResults)
//          room.questResultsSeen(user3).unsafeRunSync() should be(StillViewingQuestResults)
//          room.questResultsSeen(user4).unsafeRunSync() should be(StillViewingQuestResults)
//          room.questResultsSeen(user5).unsafeRunSync() should be(GameContinues(user1, 1, missions, user1, 5))
//
//          val repr = mvar.read.unsafeRunSync().gameRepresentation.get
//
//          repr.state should be(MissionProposing(1, user1, 5))
//        }
//      }
//
//      "return the latest mission based on which ones have been completed" in {
//        forAll { (roomId: RoomId, config: GameConfig) =>
//
//          val user1 = Nickname("Taylor")
//          val user2 = Nickname("Nick")
//          val user3 = Nickname("Chris")
//          val user4 = Nickname("Carter")
//          val user5 = Nickname("Austin")
//
//          val users = List(user1, user2, user3, user4, user5)
//
//          val missions = {
//            val default = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()
//
//            val updateMissions1 = IO.fromEither(Missions.completeMission(default, 1, QuestVote(true))).unsafeRunSync()
//            IO.fromEither(Missions.completeMission(updateMissions1, 2, QuestVote(true))).unsafeRunSync()
//          }
//
//          val goodPlayerRoles = List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy))
//
//          val gr = GameRepresentation(
//            QuestResultsViewing(NextMission(user1), Nil),
//            missions,
//            List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
//            goodPlayerRoles,
//            users)
//
//          val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()
//
//          val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)
//
//          room.questResultsSeen(user1).unsafeRunSync() should be(StillViewingQuestResults)
//          room.questResultsSeen(user2).unsafeRunSync() should be(StillViewingQuestResults)
//          room.questResultsSeen(user3).unsafeRunSync() should be(StillViewingQuestResults)
//          room.questResultsSeen(user4).unsafeRunSync() should be(StillViewingQuestResults)
//          room.questResultsSeen(user5).unsafeRunSync() should be(GameContinues(user1, 3, missions, user1, 5))
//
//          val repr = mvar.read.unsafeRunSync().gameRepresentation.get
//
//          repr.state should be(MissionProposing(3, user1, 5))
//        }
//      }
//    }
//  }

  "assassinVote" should {
    "fail if the person voting isn't the assassin" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val missions = {
          val default = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val updateMissions1 = IO.fromEither(Missions.completeMission(default, 1, QuestVote(true))).unsafeRunSync()
          IO.fromEither(Missions.completeMission(updateMissions1, 2, QuestVote(true))).unsafeRunSync()
        }

        val goodPlayerRoles = List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy))

        val gr = GameRepresentation(
          AssassinVoteState,
          missions,
          List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy)),
          goodPlayerRoles,
          users)

        val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)

        room.assassinVote(user5, user3).attempt.unsafeRunSync() should be(Left(PlayerIsNotTheAssassin(user5)))

        val repr = mvar.read.unsafeRunSync().gameRepresentation.get

        repr.state should be(AssassinVoteState)
      }
    }

    "return GameOver with GoodGuys winning if the guess was incorrect - state GoodSideWins" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val missions = {
          val default = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val updateMissions1 = IO.fromEither(Missions.completeMission(default, 1, QuestVote(true))).unsafeRunSync()
          IO.fromEither(Missions.completeMission(updateMissions1, 2, QuestVote(true))).unsafeRunSync()
        }

        val goodPlayerRoles = List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy))
        val badPlayerRoles = List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy))

        val gr = GameRepresentation(
          AssassinVoteState,
          missions,
          badPlayerRoles,
          goodPlayerRoles,
          users)

        val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)

        room.assassinVote(user1, user4).unsafeRunSync() should be(
          GameOver(BadPlayerRole(user1, Assassin), Some(user4), GoodPlayerRole(user3, Merlin), goodPlayerRoles, badPlayerRoles, GoodGuys))

        val repr = mvar.read.unsafeRunSync().gameRepresentation.get

        repr.state should be(GoodSideWins)
      }
    }

    "return true if the guess was incorrect - BadSideWins state" in {
      forAll { (roomId: RoomId, config: GameConfig) =>

        val user1 = Nickname("Taylor")
        val user2 = Nickname("Nick")
        val user3 = Nickname("Chris")
        val user4 = Nickname("Carter")
        val user5 = Nickname("Austin")

        val users = List(user1, user2, user3, user4, user5)

        val missions = {
          val default = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val updateMissions1 = IO.fromEither(Missions.completeMission(default, 1, QuestVote(true))).unsafeRunSync()
          IO.fromEither(Missions.completeMission(updateMissions1, 2, QuestVote(true))).unsafeRunSync()
        }

        val goodPlayerRoles = List(GoodPlayerRole(user3, Merlin), GoodPlayerRole(user4, NormalGoodGuy), GoodPlayerRole(user5, NormalGoodGuy))
        val badPlayerRoles = List(BadPlayerRole(user1, Assassin), BadPlayerRole(user2, NormalBadGuy))

        val gr = GameRepresentation(
          AssassinVoteState,
          missions,
          badPlayerRoles,
          goodPlayerRoles,
          users)

        val mvar = MVar.of[IO, InternalRoom](InternalRoom(users, Some(gr))).unsafeRunSync()

        val room = Room.buildPrivate(mockRandomAlg, roomId, mvar)

        room.assassinVote(user1, user3).unsafeRunSync() should be(
          GameOver(BadPlayerRole(user1, Assassin), Some(user3), GoodPlayerRole(user3, Merlin), goodPlayerRoles, badPlayerRoles, BadGuys))

        val repr = mvar.read.unsafeRunSync().gameRepresentation.get

        repr.state should be(BadSideWins)
      }
    }
  }
}