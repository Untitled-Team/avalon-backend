package com.avalon.avalongame.events

import cats.Eq
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

  "CreateGame" should {

    "fail when a ConnectionContext already exists for the user" in {
      forAll { (roomId: RoomId, gameConfig: GameConfig) =>
        new context {
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager{}

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure(new MockRoom {})
          }
          val mockRoom = mockRoomManager.get(roomId).unsafeRunSync()

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            CreateGame(nickname1),
            userQueue,
            mockRoomManager,
            mockRoomIdGenerator,
            outgoingRef,
            ctxRef
          ).attempt.unsafeRunSync() should be(Left(ContextExistsAlready))
        }
      }
    }

    "successfully respond with MoveToLobby when the game is created" in {
      forAll { (roomId: RoomId, gameConfig: GameConfig) =>
        new context {
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager{}

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
              }
            }
          }
          val mockRoom = mockRoomManager.get(roomId).unsafeRunSync()

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](None).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map.empty).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            CreateGame(nickname1),
            userQueue,
            mockRoomManager,
            mockRoomIdGenerator,
            outgoingRef,
            ctxRef
          ).attempt.unsafeRunSync()

          userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(MoveToLobby(roomId, mockRoom.players.unsafeRunSync()))
        }
      }
    }
  }

  "JoinGame" should {
    "fail when a ConnectionContext already exists for the user" in {
      forAll { (roomId: RoomId, gameConfig: GameConfig) =>
        new context {

          val broadcastRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()

          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def add(nickname: Nickname, respond: Queue[IO, OutgoingEvent]): IO[Unit] = IO.unit
            override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = broadcastRef.set(Some(outgoingEvent))
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
          val mockRoom = mockRoomManager.get(roomId).unsafeRunSync()

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            JoinGame(nickname1, roomId),
            userQueue,
            mockRoomManager,
            mockRoomIdGenerator,
            outgoingRef,
            ctxRef
          ).attempt.unsafeRunSync() should be(Left(ContextExistsAlready))
        }
      }
    }

    "successfully broadcast ChangeInLobby and respond with MoveToLobby on success" in {
      forAll { (roomId: RoomId, gameConfig: GameConfig) =>
        new context {

          val broadcastRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()

          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def add(nickname: Nickname, respond: Queue[IO, OutgoingEvent]): IO[Unit] = IO.unit
            override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = broadcastRef.set(Some(outgoingEvent))
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
              }
            }
          }
          val mockRoom = mockRoomManager.get(roomId).unsafeRunSync()

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](None).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            JoinGame(nickname1, roomId),
            userQueue,
            mockRoomManager,
            mockRoomIdGenerator,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(MoveToLobby(roomId, mockRoom.players.unsafeRunSync()))
          broadcastRef.get.unsafeRunSync() should be(Some(ChangeInLobby(mockRoom.players.unsafeRunSync())))
        }
      }
    }
  }

  "StartGame" should {
    "send PlayerInfo messages to _everyone_ when the game is started" in {
      forAll { (roomId: RoomId, gameConfig: GameConfig) =>
        new context {

          val sendToAllUserSpecificRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()

          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
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

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            StartGame,
            userQueue,
            mockRoomManager,
            mockRoomIdGenerator,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendToAllUserSpecificRef.get.unsafeRunSync() should be(
            Some(PlayerInfo(Assassin, Some(List(BadPlayerRole(nickname1, Assassin))))))
        }
      }
    }
  }

  "PlayerReady" should {
    "send the TeamAssignmentPhase to _everyone_ when everyone is ready" in {
      forAll { (roomId: RoomId, gameConfig: GameConfig) =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname("")

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
          }

          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()
          val mockAllReady = AllReady(1, Nickname("Blah"), missions)

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def playerReady(nickname: Nickname): IO[PlayerReadyEnum] = IO.pure(AllReady(1, Nickname("Blah"), missions))
              }
            }
          }

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            PlayerReady,
            userQueue,
            mockRoomManager,
            mockRoomIdGenerator,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendToAllRef.get.unsafeRunSync() should be(
            Some(TeamAssignmentPhase(mockAllReady.missionNumber, mockAllReady.missionLeader, mockAllReady.missions)))
        }
      }
    }
  }

  "PartyApprovalVote" should {
    "send the TeamAssignmentPhase message to _everyone_ when a proposed party is failed" in {
      forAll { (roomId: RoomId) =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
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

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            PartyApprovalVote(TeamVote(false)),
            userQueue,
            mockRoomManager,
            mockRoomIdGenerator,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendToAllRef.get.unsafeRunSync() should be(
            Some(TeamAssignmentPhase(1, nickname1, missions)))
        }
      }
    }
  }

  "ProposeParty" should {
    "send the ProposedParty message to _everyone_ when mission is proposed" in {
      forAll { roomId: RoomId =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
          }

          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()
          val mockAllReady = AllReady(1, Nickname("Blah"), missions)

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def proposeMission(nickname: Nickname, players: List[Nickname]): IO[MissionProposal] =
                  IO.pure(MissionProposal(1, nickname1, players))
              }
            }
          }

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            ProposeParty(List(nickname1)),
            userQueue,
            mockRoomManager,
            mockRoomIdGenerator,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendToAllRef.get.unsafeRunSync() should be(
            Some(ProposedParty(List(nickname1))))
        }
      }
    }
  }


  "PartyApprovalVote" should {
    "send the PartyApproved message to _everyone_ when a proposed party is successful" in {
      forAll { (roomId: RoomId) =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def teamVote(nickname: Nickname, vote: TeamVote): IO[TeamVoteEnum] =
                  IO.pure(SuccessfulVote(Nil))
              }
            }
          }

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            PartyApprovalVote(TeamVote(false)),
            userQueue,
            mockRoomManager,
            mockRoomIdGenerator,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendToAllRef.get.unsafeRunSync() should be(
            Some(PartyApproved))
        }
      }
    }
  }

  "QuestVoteEvent" should {
    "send the PassFailVoteResults message to _everyone_ when a mission is finished" in {
      forAll { (roomId: RoomId) =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
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

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            QuestVoteEvent(QuestVote(false)),
            userQueue,
            mockRoomManager,
            mockRoomIdGenerator,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendToAllRef.get.unsafeRunSync() should be(
            Some(PassFailVoteResults(1, 1)))
        }
      }
    }
  }

  "QuestVotesDisplayed" should {
    "send out AssassinOutgoingEvent when the game says the AssassinVote is After the Quest" in {
      forAll { (roomId: RoomId) =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def questResultsSeen(nickname: Nickname): IO[AfterQuest] = IO.pure(AssassinVote(nickname1, Nil))
              }
            }
          }

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            QuestVotesDisplayed,
            userQueue,
            mockRoomManager,
            mockRoomIdGenerator,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendToAllRef.get.unsafeRunSync() should be(
            Some(AssassinVoteOutgoingEvent(nickname1, Nil)))
        }
      }
    }

    "send out GameOverOutgoingEvent with BadGuys winning when the game says BadGuyVictory" in {
      forAll { (roomId: RoomId) =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val nickname2 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def questResultsSeen(nickname: Nickname): IO[AfterQuest] =
                  IO.pure(BadGuyVictory(BadPlayerRole(nickname1, Assassin), None, GoodPlayerRole(nickname2, Merlin), Nil, Nil, BadGuys))
              }
            }
          }

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            QuestVotesDisplayed,
            userQueue,
            mockRoomManager,
            mockRoomIdGenerator,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendToAllRef.get.unsafeRunSync() should be(
            Some(GameOverOutgoingEvent(nickname1, None, nickname2, Nil, Nil, BadGuys)))
        }
      }
    }

    "send out TeamAssignmentPhase when the GameContinues AfterQuest" in { //
      forAll { (roomId: RoomId) =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val nickname2 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {

                override def questResultsSeen(nickname: Nickname): IO[AfterQuest] =
                  IO.pure(GameContinues(nickname1, 2, missions))
              }
            }
          }

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            QuestVotesDisplayed,
            userQueue,
            mockRoomManager,
            mockRoomIdGenerator,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendToAllRef.get.unsafeRunSync() should be(
            Some(TeamAssignmentPhase(2, nickname1, missions)))
        }
      }
    }
  }

  "IncomingAsssassinVote" should {
    "send out GameOverOutgoingEvent when the AssassinVote is done" in {
      forAll { (roomId: RoomId) =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val nickname2 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create(roomId: RoomId): IO[Unit] = IO.unit
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def assassinVote(assassin: Nickname, guess: Nickname): IO[GameOver] =
                  IO.pure(GameOver(BadPlayerRole(nickname1, Assassin), None, GoodPlayerRole(nickname2, Merlin), Nil, Nil, BadGuys))
              }
            }
          }

          override val mockRoomIdGenerator: RoomIdGenerator[IO] = new RoomIdGenerator[IO] {
            override def generate: IO[RoomId] = IO.pure(roomId)
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            IncomingAssassinVote(nickname1),
            userQueue,
            mockRoomManager,
            mockRoomIdGenerator,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

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

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def add(nickname: Nickname, respond: Queue[IO, OutgoingEvent]): IO[Unit] = IO.unit
            override def remove(nickname: Nickname): IO[Unit] = IO.unit
            override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = IO.unit
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
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
      override def clockwise[A: Eq](previous: A, l: List[A]): IO[A] = IO(l.head)
    }

    val roomManager: RoomManager[IO] = RoomManager.build[IO](mockRandomAlg).unsafeRunSync()
  }
}