package com.avalon.avalongame.events

import cats.effect.concurrent.{MVar, Ref}
import cats.effect.{ContextShift, IO, Timer}
import cats.implicits._
import com.avalon.avalongame.RandomAlg
import com.avalon.avalongame.common._
import com.avalon.avalongame.room._
import fs2._
import fs2.concurrent.Queue
import org.scalatest.{FunSuite, Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration._

class EventManagerSpec extends WordSpec with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {
  import com.mrdziuban.ScalacheckMagnolia._

  implicit val cs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)
  implicit val t: Timer[IO] = IO.timer(scala.concurrent.ExecutionContext.Implicits.global)

  "GameCreated" should {
    "Get `GameCreated` in response to `CreateGame` message" in {
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
  }

  "UserJoined" should {
    "Get `UserJoined` for all Sockets in response to `JoinGame` message" in {
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
          eventManager.interpret(userQueue, Stream.eval(IO.pure(CreateGame(nickname1)))).unsafeRunSync()

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
  }

  "PlayerInfo" should {
    "Get `PlayerInfo` for all Sockets in response to `StartGame` message" in {
      forAll { (roomId: RoomId, gameConfig: GameConfig) =>
        new context {

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val nickname2 = Nickname(java.util.UUID.randomUUID().toString)
          val nickname3 = Nickname(java.util.UUID.randomUUID().toString)
          val nickname4 = Nickname(java.util.UUID.randomUUID().toString)
          val nickname5 = Nickname(java.util.UUID.randomUUID().toString)

          val eventManager: EventManager[IO] = EventManager.build[IO](roomManager, mockRoomIdGenerator).unsafeRunSync()

          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
          val userQueue2 = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
          val userQueue3 = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
          val userQueue4 = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
          val userQueue5 = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          //================
          eventManager.interpret(userQueue, Stream.eval(IO.pure(CreateGame(nickname1)))).unsafeRunSync()

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

          //================
          eventManager.interpret(userQueue4, Stream.eval(IO.pure(JoinGame(nickname4, roomId)))).unsafeRunSync()

          room.players.unsafeRunSync() should contain allOf (nickname1, nickname2, nickname3, nickname4)
          userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(ChangeInLobby(room.players.unsafeRunSync()))
          userQueue2.dequeue1.timeout(1 second).unsafeRunSync() should be(ChangeInLobby(room.players.unsafeRunSync()))
          userQueue3.dequeue1.timeout(1 second).unsafeRunSync() should be(ChangeInLobby(room.players.unsafeRunSync()))
          userQueue4.dequeue1.timeout(1 second).unsafeRunSync() should be(MoveToLobby(roomId, room.players.unsafeRunSync()))
          //================

          //================
          eventManager.interpret(
            userQueue5,
            Stream.eval(IO.pure(JoinGame(nickname5, roomId))) ++ Stream.eval(IO.pure(StartGame))).unsafeRunSync()

          room.players.unsafeRunSync() should contain allOf (nickname1, nickname2, nickname3, nickname4, nickname5)
          userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(ChangeInLobby(room.players.unsafeRunSync()))
          userQueue2.dequeue1.timeout(1 second).unsafeRunSync() should be(ChangeInLobby(room.players.unsafeRunSync()))
          userQueue3.dequeue1.timeout(1 second).unsafeRunSync() should be(ChangeInLobby(room.players.unsafeRunSync()))
          userQueue4.dequeue1.timeout(1 second).unsafeRunSync() should be(ChangeInLobby(room.players.unsafeRunSync()))
          userQueue5.dequeue1.timeout(1 second).unsafeRunSync() should be(MoveToLobby(roomId, room.players.unsafeRunSync()))
          //================

          val users = List(nickname1, nickname2, nickname3, nickname4, nickname5)

          def playerInfo(role: Role): PlayerInfo = {
            val charRole = CharacterRole.fromRole(role, List(BadPlayerRole(nickname1, Assassin), BadPlayerRole(nickname2, NormalBadGuy)))
            PlayerInfo(charRole.character, charRole.badGuys)
          }

          userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(playerInfo(Assassin))
          userQueue2.dequeue1.timeout(1 second).unsafeRunSync() should be(playerInfo(NormalBadGuy))
          userQueue3.dequeue1.timeout(1 second).unsafeRunSync() should be(playerInfo(Merlin))
          userQueue4.dequeue1.timeout(1 second).unsafeRunSync() should be(playerInfo(NormalGoodGuy))
          userQueue5.dequeue1.timeout(1 second).unsafeRunSync() should be(playerInfo(NormalGoodGuy))
        }
      }
    }

    "Make sure we send PlayerInfo messages to _everyone_ when the game is started" in {
      forAll { (roomId: RoomId, gameConfig: GameConfig) =>
        new context {

          val sendToAllUserSpecificRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()

          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)

          val mockOutgoingManager: OutgoingManager[IO] = new OutgoingManager[IO] {
            override def add(usernameWithSend: UsernameWithSend[IO]): IO[Unit] = IO.unit
            override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = IO.unit
            override def broadcastUserSpecific(nickname: Nickname, outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
            override def sendToAll(event: OutgoingEvent): IO[Unit] = IO.unit
            override def sendToAllUserSpecific(outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] =
              sendToAllUserSpecificRef.set(Some(outgoingF(nickname1).unsafeRunSync()))
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def startGame: IO[AllPlayerRoles] = IO.pure(AllPlayerRoles(Nil, List(BadPlayerRole(nickname1, Assassin))))
              }
            }
          }

          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val eventManager: EventManager[IO] = EventManager.buildOutgoing[IO](mockRoomManager, mockRoomIdGenerator, outgoingRef)
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          eventManager.interpret(userQueue, Stream.eval(IO.pure(JoinGame(nickname1, roomId))) ++ Stream.eval(IO.pure(StartGame))).unsafeRunSync()
          sendToAllUserSpecificRef.get.unsafeRunSync() should be(
            Some(PlayerInfo(Assassin, Some(List(BadPlayerRole(nickname1, Assassin))))))
        }
      }
    }
  }

  "TeamAssignmentPhase" should {
    "Make sure we send the TeamAssignmentPhase to _everyone_ when the Game decides it is time" in {
      forAll { (roomId: RoomId, gameConfig: GameConfig) =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new OutgoingManager[IO] {
            override def add(usernameWithSend: UsernameWithSend[IO]): IO[Unit] = IO.unit
            override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = IO.unit
            override def broadcastUserSpecific(nickname: Nickname, outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
            def sendToAllUserSpecific(outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
          }

          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()
          val mockAllReady = AllReady(1, Nickname("Blah"), missions)

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def playerReady(nickname: Nickname): IO[PlayerReadyEnum] = IO.pure(AllReady(1, Nickname("Blah"), missions))
              }
            }
          }

          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)

          val eventManager: EventManager[IO] = EventManager.buildOutgoing[IO](mockRoomManager, mockRoomIdGenerator, outgoingRef)
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          eventManager.interpret(userQueue, Stream.eval(IO.pure(JoinGame(nickname1, roomId))) ++ Stream.eval(IO.pure(PlayerReady))).unsafeRunSync()
          sendToAllRef.get.unsafeRunSync() should be(
            Some(TeamAssignmentPhase(mockAllReady.missionNumber, mockAllReady.missionLeader, mockAllReady.missions)))
        }
      }
    }

    "Make sure we send the TeamAssignmentPhase message to _everyone_ when a proposed party is Failed" in {
      forAll { (roomId: RoomId) =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new OutgoingManager[IO] {
            override def add(usernameWithSend: UsernameWithSend[IO]): IO[Unit] = IO.unit
            override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = IO.unit
            override def broadcastUserSpecific(nickname: Nickname, outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
            def sendToAllUserSpecific(outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {

              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def teamVote(nickname: Nickname, vote: TeamVote): IO[TeamVoteEnum] =
                  IO.pure(FailedVote(nickname1, 1, Nil, missions))
              }
            }
          }

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()

          val eventManager: EventManager[IO] = EventManager.buildOutgoing[IO](mockRoomManager, mockRoomIdGenerator, outgoingRef)
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          eventManager.interpret(
            userQueue,
            Stream.eval(IO.pure(JoinGame(nickname1, roomId))) ++
              Stream.eval(IO.pure(PartyApprovalVote(TeamVote(false))))).unsafeRunSync()
          sendToAllRef.get.unsafeRunSync() should be(
            Some(TeamAssignmentPhase(1, nickname1, missions)))
        }
      }
    }
  }

  "ProposeParty" should {
    "Make sure we send the TeamAssignment message to _everyone_ when mission is proposed" in {
      forAll { (roomId: RoomId) =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)

          val mockOutgoingManager: OutgoingManager[IO] = new OutgoingManager[IO] {
            override def add(usernameWithSend: UsernameWithSend[IO]): IO[Unit] = IO.unit
            override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = IO.unit
            override def broadcastUserSpecific(nickname: Nickname, outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
            def sendToAllUserSpecific(outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
          }

          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()
          val mockAllReady = AllReady(1, Nickname("Blah"), missions)

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def proposeMission(nickname: Nickname, players: List[Nickname]): IO[MissionProposal] =
                  IO.pure(MissionProposal(1, nickname1, players))
              }
            }
          }

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()


          val eventManager: EventManager[IO] = EventManager.buildOutgoing[IO](mockRoomManager, mockRoomIdGenerator, outgoingRef)
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          eventManager.interpret(
            userQueue,
            Stream.eval(IO.pure(JoinGame(nickname1, roomId))) ++ Stream.eval(IO.pure(ProposeParty(List(nickname1))))).unsafeRunSync()
          sendToAllRef.get.unsafeRunSync() should be(
            Some(ProposedParty(List(nickname1))))
        }
      }
    }
  }


  "PartyApprovalVote" should {
    "Make sure we send the PartyApproved message to _everyone_ when a proposed party is successful" in {
      forAll { (roomId: RoomId) =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new OutgoingManager[IO] {
            override def add(usernameWithSend: UsernameWithSend[IO]): IO[Unit] = IO.unit
            override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = IO.unit
            override def broadcastUserSpecific(nickname: Nickname, outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
            def sendToAllUserSpecific(outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def teamVote(nickname: Nickname, vote: TeamVote): IO[TeamVoteEnum] =
                  IO.pure(SuccessfulVote(Nil))
              }
            }
          }

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()


          val eventManager: EventManager[IO] = EventManager.buildOutgoing[IO](mockRoomManager, mockRoomIdGenerator, outgoingRef)
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          eventManager.interpret(
            userQueue,
            Stream.eval(IO.pure(JoinGame(nickname1, roomId))) ++
              Stream.eval(IO.pure(PartyApprovalVote(TeamVote(false))))).unsafeRunSync()
          sendToAllRef.get.unsafeRunSync() should be(
            Some(PartyApproved))
        }
      }
    }
  }

  "QuestVote" should {
    "Make sure we send the PassFailVoteResults message to _everyone_ when a mission is finished" in {
      forAll { (roomId: RoomId) =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new OutgoingManager[IO] {
            override def add(usernameWithSend: UsernameWithSend[IO]): IO[Unit] = IO.unit
            override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = IO.unit
            override def broadcastUserSpecific(nickname: Nickname, outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
            def sendToAllUserSpecific(outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def questVote(nickname: Nickname, vote: QuestVote): IO[QuestVotingEnum] =
                  IO.pure(FinishedVote(List(QuestVote(true), QuestVote(false))))
              }
            }
          }

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()

          val eventManager: EventManager[IO] = EventManager.buildOutgoing[IO](mockRoomManager, mockRoomIdGenerator, outgoingRef)
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          eventManager.interpret(
            userQueue,
            Stream.eval(IO.pure(JoinGame(nickname1, roomId))) ++
              Stream.eval(IO.pure(QuestVoteEvent(QuestVote(false))))).unsafeRunSync()
          sendToAllRef.get.unsafeRunSync() should be(
            Some(PassFailVoteResults(1, 1)))
        }
      }
    }
  }

  "QuestVotesDisplayed" should {
    "Make sure we send out AssassinOutgoingEvent when we get that back from the questResultsSeen method" in {
      forAll { (roomId: RoomId) =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new OutgoingManager[IO] {
            override def add(usernameWithSend: UsernameWithSend[IO]): IO[Unit] = IO.unit
            override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = IO.unit
            override def broadcastUserSpecific(nickname: Nickname, outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
            def sendToAllUserSpecific(outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def questResultsSeen(nickname: Nickname): IO[AfterQuest] = IO.pure(AssassinVote(nickname1, Nil))
              }
            }
          }

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()


          val eventManager: EventManager[IO] = EventManager.buildOutgoing[IO](mockRoomManager, mockRoomIdGenerator, outgoingRef)
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          eventManager.interpret(
            userQueue,
            Stream.eval(IO.pure(JoinGame(nickname1, roomId))) ++
              Stream.eval(IO.pure(QuestVotesDisplayed))).unsafeRunSync()
          sendToAllRef.get.unsafeRunSync() should be(
            Some(AssassinVoteOutgoingEvent(nickname1, Nil)))
        }
      }
    }

    "Make sure we send out BadGuyVictory when we get that back from the questResultsSeen method" in {
      forAll { (roomId: RoomId) =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val nickname2 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new OutgoingManager[IO] {
            override def add(usernameWithSend: UsernameWithSend[IO]): IO[Unit] = IO.unit
            override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = IO.unit
            override def broadcastUserSpecific(nickname: Nickname, outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
            def sendToAllUserSpecific(outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def questResultsSeen(nickname: Nickname): IO[AfterQuest] =
                  IO.pure(BadGuyVictory(BadPlayerRole(nickname1, Assassin), None, GoodPlayerRole(nickname2, Merlin), Nil, Nil, BadGuys))
              }
            }
          }

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()

          val eventManager: EventManager[IO] = EventManager.buildOutgoing[IO](mockRoomManager, mockRoomIdGenerator, outgoingRef)
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          eventManager.interpret(
            userQueue,
            Stream.eval(IO.pure(JoinGame(nickname1, roomId))) ++
              Stream.eval(IO.pure(QuestVotesDisplayed))).unsafeRunSync()
          sendToAllRef.get.unsafeRunSync() should be(
            Some(GameOverOutgoingEvent(nickname1, None, nickname2, Nil, Nil, BadGuys)))
        }
      }
    }

    "Make sure we send out TeamAssignmentPhase when we get that back from the questResultsSeen method" in { //
      forAll { (roomId: RoomId) =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val nickname2 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new OutgoingManager[IO] {
            override def add(usernameWithSend: UsernameWithSend[IO]): IO[Unit] = IO.unit
            override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = IO.unit
            override def broadcastUserSpecific(nickname: Nickname, outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
            def sendToAllUserSpecific(outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def questResultsSeen(nickname: Nickname): IO[AfterQuest] =
                  IO.pure(GameContinues(nickname1, 2, missions))
              }
            }
          }

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()

          val eventManager: EventManager[IO] = EventManager.buildOutgoing[IO](mockRoomManager, mockRoomIdGenerator, outgoingRef)
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          eventManager.interpret(
            userQueue,
            Stream.eval(IO.pure(JoinGame(nickname1, roomId))) ++
              Stream.eval(IO.pure(QuestVotesDisplayed))).unsafeRunSync()
          sendToAllRef.get.unsafeRunSync() should be(
            Some(TeamAssignmentPhase(2, nickname1, missions)))
        }
      }
    }
  }





  "IncomingAsssassinVote" should {
    "Make sure we send out GameOver when the AssassinVote is done" in {
      forAll { (roomId: RoomId) =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val nickname2 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new OutgoingManager[IO] {
            override def add(usernameWithSend: UsernameWithSend[IO]): IO[Unit] = IO.unit
            override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = IO.unit
            override def broadcastUserSpecific(nickname: Nickname, outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
            def sendToAllUserSpecific(outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def assassinVote(assassin: Nickname, guess: Nickname): IO[GameOver] =
                  IO.pure(GameOver(BadPlayerRole(nickname1, Assassin), None, GoodPlayerRole(nickname2, Merlin), Nil, Nil, BadGuys))
              }
            }
          }

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()


          val eventManager: EventManager[IO] = EventManager.buildOutgoing[IO](mockRoomManager, mockRoomIdGenerator, outgoingRef)
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          eventManager.interpret(
            userQueue,
            Stream.eval(IO.pure(JoinGame(nickname1, roomId))) ++
              Stream.eval(IO.pure(IncomingAssassinVote(nickname1)))).unsafeRunSync()
          sendToAllRef.get.unsafeRunSync() should be(
            Some(GameOverOutgoingEvent(nickname1, None, nickname2, Nil, Nil, BadGuys)))
        }
      }
    }
  }


  "Disconnect Stuff" should {
    "Make sure we send out ChangeInLobby on Disconnect" in {
      forAll { (roomId: RoomId) =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val nickname2 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new OutgoingManager[IO] {
            override def add(usernameWithSend: UsernameWithSend[IO]): IO[Unit] = IO.unit
            override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = IO.unit
            override def broadcastUserSpecific(nickname: Nickname, outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
            def sendToAllUserSpecific(outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = IO.unit
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def removePlayer(player: Nickname): IO[Unit] = IO.unit
              }
            }
          }

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()


          val eventManager: EventManager[IO] = EventManager.buildOutgoing[IO](mockRoomManager, mockRoomIdGenerator, outgoingRef)
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          val mvar: MVar[IO, Unit] = MVar.empty[IO, Unit].unsafeRunSync()

          val fiber = eventManager.interpret(
            userQueue,
            Stream.eval(IO.pure(JoinGame(nickname1, roomId))) ++ Stream.eval(mvar.put(())).flatMap(_ => Stream.never[IO])).start.unsafeRunSync()

          mvar.take.timeout(1 second).flatMap(_ => fiber.cancel).unsafeRunSync()

          sendToAllRef.get.unsafeRunSync() should be(Some(ChangeInLobby(Nil)))
        }
      }
    }
  }

  trait context {
    val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
      override def generate: IO[RoomId] = IO.pure(RoomId("blah"))
    }

    val mockRandomAlg: RandomAlg[IO] = new RandomAlg[IO] {
      override def shuffle[A](l: List[A]): IO[List[A]] = IO.pure(l)

      override def randomGet[A](l: List[A]): IO[A] = IO(l.head) //oops
    }

    val roomManager: RoomManager[IO] = RoomManager.build[IO](mockRandomAlg).unsafeRunSync()
  }
}