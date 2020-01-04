package com.avalon.avalongame.events

import com.avalon.avalongame.Arbitraries._
import cats.Eq
import cats.effect.concurrent.{MVar, Ref}
import cats.effect.{ContextShift, IO, Timer}
import cats.implicits._
import com.avalon.avalongame.RandomAlg
import com.avalon.avalongame.common._
import com.avalon.avalongame.events.EventManager.NoContext
import com.avalon.avalongame.room._
import io.chrisdavenport.fuuid._
import fs2._
import fs2.concurrent.Queue
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FunSuite, Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration._

class EventManagerSpec extends WordSpec with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {
  import com.mrdziuban.ScalacheckMagnolia._

  implicit val cs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)
  implicit val t: Timer[IO] = IO.timer(scala.concurrent.ExecutionContext.Implicits.global)
  implicit val R: RandomAlg[IO] = new RandomAlg[IO] {
    val constant = FUUID.randomFUUID[IO].unsafeRunSync()
    def shuffle[A](l: List[A]): IO[List[A]] = ???
    def randomGet[A](l: List[A]): IO[A] = ???
    def clockwise[A: Eq](previous: A, l: List[A]): IO[A] = ???
    def fuuid: IO[FUUID] = IO.pure(constant)
  }

  "CreateGame" should {

    "fail when a ConnectionContext already exists for the user" in {
      forAll { (roomId: RoomId, gameConfig: GameConfig) =>
        new context {
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager{}

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure(new MockRoom {})
          }
          val mockRoom = mockRoomManager.get(roomId).unsafeRunSync()

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            CreateGame(nickname1),
            userQueue,
            mockRoomManager,
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

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
              }
            }
          }
          val mockRoom = mockRoomManager.get(roomId).unsafeRunSync()

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](None).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map.empty).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            CreateGame(nickname1),
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).attempt.unsafeRunSync()

          userQueue.tryDequeue1.unsafeRunSync() should be(Some(MoveToLobby.make[IO](roomId, mockRoom.players.unsafeRunSync()).unsafeRunSync()))
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
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
              }
            }
          }
          val mockRoom = mockRoomManager.get(roomId).unsafeRunSync()

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            JoinGame(nickname1, roomId),
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).attempt.unsafeRunSync() should be(Left(ContextExistsAlready))
        }
      }
    }

    "successfully broadcast ChangeInLobby and respond with MoveToLobby on success" in {
      forAll { (roomId: RoomId, gameConfig: GameConfig) =>
        new context {

          val sendRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val broadcastRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()

          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def add(nickname: Nickname, respond: Queue[IO, OutgoingEvent]): IO[Unit] = IO.unit
            override def send(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = sendRef.set(Some(outgoingEvent))
            override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = broadcastRef.set(Some(outgoingEvent))
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
              }
            }
          }
          val mockRoom = mockRoomManager.get(roomId).unsafeRunSync()

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](None).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            JoinGame(nickname1, roomId),
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()


          sendRef.get.unsafeRunSync() should be(Some(MoveToLobby.make[IO](roomId, mockRoom.players.unsafeRunSync()).unsafeRunSync()))
          broadcastRef.get.unsafeRunSync() should be(Some(ChangeInLobby.make[IO](mockRoom.players.unsafeRunSync()).unsafeRunSync()))
        }
      }
    }
  }

  "LeaveGame" should {
    "fail when no ConnectionContext exists for the user" in {
      forAll { (roomId: RoomId, gameConfig: GameConfig) =>
        new context {

          val broadcastRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val outgoingRemoveRef = Ref.of[IO, Option[Unit]](None).unsafeRunSync()
          val roomRemoveRef = Ref.of[IO, Option[Unit]](None).unsafeRunSync()

          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def add(nickname: Nickname, respond: Queue[IO, OutgoingEvent]): IO[Unit] = IO.unit
            override def remove(nickname: Nickname): IO[Unit] = outgoingRemoveRef.set(Some(()))
            override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = broadcastRef.set(Some(outgoingEvent))
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def removePlayer(player: Nickname): IO[Unit] = roomRemoveRef.set(Some(()))
              }
            }
          }
          val mockRoom = mockRoomManager.get(roomId).unsafeRunSync()

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](None).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            LeaveGame,
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).attempt.unsafeRunSync() should be(Left(NoContext))
        }
      }
    }

    "fail when the game has already started" in {
      forAll { (roomId: RoomId, gameConfig: GameConfig) =>
        new context {

          val broadcastRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val outgoingRemoveRef = Ref.of[IO, Option[Unit]](None).unsafeRunSync()

          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def add(nickname: Nickname, respond: Queue[IO, OutgoingEvent]): IO[Unit] = IO.unit
            override def remove(nickname: Nickname): IO[Unit] = outgoingRemoveRef.set(Some(()))
            override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = broadcastRef.set(Some(outgoingEvent))
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def removePlayer(player: Nickname): IO[Unit] = IO.raiseError(GameHasStarted)
              }
            }
          }
          val mockRoom = mockRoomManager.get(roomId).unsafeRunSync()

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            LeaveGame,
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).attempt.unsafeRunSync() should be(Left(GameHasStarted))
        }
      }
    }

    "successfully remove player from room and outgoing manager" in {
      forAll { (roomId: RoomId, gameConfig: GameConfig) =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val outgoingRemoveRef = Ref.of[IO, Option[Unit]](None).unsafeRunSync()
          val roomRemoveRef = Ref.of[IO, Option[Unit]](None).unsafeRunSync()

          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def add(nickname: Nickname, respond: Queue[IO, OutgoingEvent]): IO[Unit] = IO.unit
            override def remove(nickname: Nickname): IO[Unit] = outgoingRemoveRef.set(Some(()))
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
            override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = ???
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO.pure(Nil)
                override def removePlayer(player: Nickname): IO[Unit] = roomRemoveRef.set(Some(()))
              }
            }
          }
          val mockRoom = mockRoomManager.get(roomId).unsafeRunSync()

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            LeaveGame,
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendToAllRef.get.unsafeRunSync() should be(Some(ChangeInLobby.make[IO](Nil).unsafeRunSync()))
          outgoingRemoveRef.get.unsafeRunSync() should be(Some(()))
          roomRemoveRef.get.unsafeRunSync() should be(Some(()))
          userQueue.tryDequeue1.unsafeRunSync() should be(Some(GameLeft.make[IO].unsafeRunSync()))
        }
      }
    }
  }

  "Reconnect" should {
    "fail when the nickname isn't found in the room" in {
      forAll { (roomId: RoomId, gameConfig: GameConfig) =>
        new context {

          val outgoingRemoveRef = Ref.of[IO, Option[Unit]](None).unsafeRunSync()
          val roomRemoveRef = Ref.of[IO, Option[Unit]](None).unsafeRunSync()

          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager{}

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO.pure(Nil)
              }
            }
          }

          val mockRoom = mockRoomManager.get(roomId).unsafeRunSync()

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](None).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            Reconnect(nickname1, roomId, FUUID.randomFUUID[IO].unsafeRunSync()),
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).attempt.unsafeRunSync() should be(Left(NicknameNotFoundInRoom(nickname1)))
        }
      }
    }

    "return GameNoLongerExists when the outgoing does not have the room in it" in {
      forAll { (roomId: RoomId, gameConfig: GameConfig) =>
        new context {

          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO.pure(List(nickname1))
              }
            }
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](None).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map.empty).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            Reconnect(nickname1, roomId, FUUID.randomFUUID[IO].unsafeRunSync()),
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).attempt.unsafeRunSync()

          userQueue.tryDequeue1.unsafeRunSync().get should be(GameNoLongerExists.make[IO].unsafeRunSync())
        }
      }
    }

    "fail when no room exists for outgoing managing" in {
      forAll { (roomId: RoomId, gameConfig: GameConfig) =>
        new context {

          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.raiseError(NoRoomFoundForChatId)
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](None).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map.empty).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            Reconnect(nickname1, roomId, FUUID.randomFUUID[IO].unsafeRunSync()),
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          userQueue.tryDequeue1.unsafeRunSync().get should be(GameNoLongerExists.make[IO].unsafeRunSync())
        }
      }
    }

    "successfully reconnect player from room and outgoing manager" in {
      forAll { (roomId: RoomId, gameConfig: GameConfig) =>
        new context {

          val reconnectRef = Ref.of[IO, Option[Nickname]](None).unsafeRunSync()

          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def reconnect(nickname: Nickname, id: FUUID, respond: Queue[IO, OutgoingEvent]): IO[Unit] = reconnectRef.set(Some(nickname))
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO.pure(List(nickname1))
              }
            }
          }

          val mockRoom = mockRoomManager.get(roomId).unsafeRunSync()

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](None).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            Reconnect(nickname1, roomId, FUUID.randomFUUID[IO].unsafeRunSync()),
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          ctxRef.get.unsafeRunSync() should be(Some(ConnectionContext(nickname1, roomId)))
          reconnectRef.get.unsafeRunSync() should be(Some(nickname1))
        }
      }
    }
  }

  "StartGame" should {
    "send PlayerInfo and TeamAssignmentPhase when the game starts" in {
      forAll { (roomId: RoomId, gameConfig: GameConfig) =>
        new context {

          val sendToAllUserSpecificRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()

          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def sendToAllUserSpecific(outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] =
              sendToAllUserSpecificRef.set(Some(outgoingF(nickname1).unsafeRunSync()))
            override def sendToAll(event: OutgoingEvent): IO[Unit] =
              sendToAllRef.set(Some(event))
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def startGame: IO[StartGameInfo] =
                  IO.pure(
                    StartGameInfo(
                      AllPlayerRoles(Nil, List(BadPlayerRole(nickname1, Assassin))),
                      AllReady(1, nickname1, missions, nickname1, 5)))
              }
            }
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            StartGame,
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendToAllUserSpecificRef.get.unsafeRunSync() should be(
            Some(
              PlayerInfo.make[IO](Assassin, Some(List(BadPlayerRole(nickname1, Assassin)))).unsafeRunSync()))

          sendToAllRef.get.unsafeRunSync() should be(
            Some(
              TeamAssignmentPhase.make[IO](1, nickname1, missions, nickname1, 5).unsafeRunSync()))
        }
      }
    }
  }

  "PartyApprovalVote" should {
    "acknowledge message when votes are still coming iin" in {
      forAll { (roomId: RoomId) =>
        new context {

          val sendRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def send(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = sendRef.set(Some(outgoingEvent))
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {

              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def teamVote(nickname: Nickname, vote: TeamVote): IO[Either[GameOver, TeamVoteEnum]] =
                  IO.pure(Right(TeamPhaseStillVoting))
              }
            }
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            PartyApprovalVote(TeamVote(false)),
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendRef.get.unsafeRunSync() should be(Some(PartyApprovalVoteAcknowledgement.make[IO].unsafeRunSync()))
        }
      }
    }

    "send the TeamAssignmentPhase message to _everyone_ when a proposed party is failed" in {
      forAll { (roomId: RoomId) =>
        new context {

          val sendRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val sendToAllRef = Ref.of[IO, List[OutgoingEvent]](Nil).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val nickname2 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def send(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = sendRef.set(Some(outgoingEvent))
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.update(_ ::: List(event))
          }

          val playerTeamVotes = List(PlayerTeamVote(nickname1, TeamVote(true)), PlayerTeamVote(nickname2, TeamVote(false)))

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {

              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def teamVote(nickname: Nickname, vote: TeamVote): IO[Either[GameOver, TeamVoteEnum]] =
                  IO.pure(Right(FailedVote(nickname1, 1, playerTeamVotes, missions, nickname1, 5)))
              }
            }
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            PartyApprovalVote(TeamVote(false)),
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendRef.get.unsafeRunSync() should be(Some(PartyApprovalVoteAcknowledgement.make[IO].unsafeRunSync()))
          sendToAllRef.get.unsafeRunSync() should be(
            List(
              PartyVotes.make[IO](List(nickname1), List(nickname2)).unsafeRunSync(),
              TeamAssignmentPhase.make[IO](1, nickname1, missions, nickname1, 5).unsafeRunSync()))
        }
      }
    }

    "send the TeamAssignmentPhase message to _everyone_ when a proposed party is successful" in {
      forAll { roomId: RoomId =>
        new context {

          val sendRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val sendToAllRef = Ref.of[IO, List[OutgoingEvent]](Nil).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val nickname2 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def send(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = sendRef.set(Some(outgoingEvent))
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.update(_ ::: List(event))
          }

          val playerTeamVotes = List(PlayerTeamVote(nickname1, TeamVote(true)), PlayerTeamVote(nickname2, TeamVote(false)))

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {

              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def teamVote(nickname: Nickname, vote: TeamVote): IO[Either[GameOver, TeamVoteEnum]] =
                  IO.pure(Right(SuccessfulVote(playerTeamVotes)))
              }
            }
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            PartyApprovalVote(TeamVote(false)),
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendRef.get.unsafeRunSync() should be(Some(PartyApprovalVoteAcknowledgement.make[IO].unsafeRunSync()))
          sendToAllRef.get.unsafeRunSync() should be(
            List(
              PartyVotes.make[IO](List(nickname1), List(nickname2)).unsafeRunSync(),
              PartyApproved.make[IO].unsafeRunSync()))
        }
      }
    }

    "send GameOver message to _everyone_ when a proposed party is failed" in {
      forAll { roomId: RoomId =>
        new context {

          val sendRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def send(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = sendRef.set(Some(outgoingEvent))
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
          }

          val gameOver = GameOver(BadPlayerRole(Nickname("blah"), Assassin), None, GoodPlayerRole(Nickname("blah"), Merlin), Nil, Nil, BadGuys)

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {

              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def teamVote(nickname: Nickname, vote: TeamVote): IO[Either[GameOver, TeamVoteEnum]] =
                  IO.pure(Left(gameOver))
              }
            }
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            PartyApprovalVote(TeamVote(false)),
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendToAllRef.get.unsafeRunSync() should be(Some(
            GameOverOutgoingEvent.make[IO](
              gameOver.assassin.nickname,
              gameOver.assassinGuess,
              gameOver.merlin.nickname,
              gameOver.goodGuys,
              gameOver.badGuys,
              gameOver.winningTeam).unsafeRunSync()
          ))
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

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def proposeMission(nickname: Nickname, players: List[Nickname]): IO[MissionProposal] =
                  IO.pure(MissionProposal(1, nickname1, players, 5, nickname1))
              }
            }
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            ProposeParty(List(nickname1)),
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendToAllRef.get.unsafeRunSync() should be(
            Some(ProposedParty.make[IO](List(nickname1)).unsafeRunSync()))
        }
      }
    }
  }

  "QuestVoteEvent" should {
    "acknowledge vote when vote isn't finished" in {
      forAll { roomId: RoomId =>
        new context {

          val sendRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def send(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = sendRef.set(Some(outgoingEvent))
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def questVote(nickname: Nickname, vote: QuestVote): IO[QuestVotingEnum] =
                  IO.pure(QuestPhaseStillVoting)
              }
            }
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            QuestVoteEvent(QuestVote(false)),
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendRef.get.unsafeRunSync() should be(Some(QuestVoteAcknowledgement.make[IO].unsafeRunSync()))
        }
      }
    }

    "send the PassFailVoteResults message to _everyone_ when a mission continues" in {
      forAll { roomId: RoomId =>
        new context {

          val sendRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val sendToAllRef = Ref.of[IO, List[OutgoingEvent]](Nil).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def send(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = sendRef.set(Some(outgoingEvent))
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.update(_ ::: List(event))
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def questVote(nickname: Nickname, vote: QuestVote): IO[QuestVotingEnum] =
                  IO.pure(FinishedVote(List(QuestVote(true), QuestVote(false)), GameContinues(nickname1, 2, missions, nickname1, 1)))
              }
            }
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            QuestVoteEvent(QuestVote(false)),
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendRef.get.unsafeRunSync() should be(Some(QuestVoteAcknowledgement.make[IO].unsafeRunSync()))
          sendToAllRef.get.unsafeRunSync() should be(
            List(
              PassFailVoteResults.make[IO](1, 1).unsafeRunSync(),
              TeamAssignmentPhase.make[IO](2, nickname1, missions, nickname1, 1).unsafeRunSync()))
        }
      }
    }

    "send the PassFailVoteResults message to _everyone_ when a mission is finished and assassin must vote" in {
      forAll { roomId: RoomId =>
        new context {

          val sendRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val sendToAllRef = Ref.of[IO, List[OutgoingEvent]](Nil).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def send(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = sendRef.set(Some(outgoingEvent))
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.update(_ ::: List(event))
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def questVote(nickname: Nickname, vote: QuestVote): IO[QuestVotingEnum] =
                  IO.pure(FinishedVote(List(QuestVote(true), QuestVote(false)), AssassinVote(nickname1, Nil, missions)))
              }
            }
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            QuestVoteEvent(QuestVote(false)),
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendRef.get.unsafeRunSync() should be(Some(QuestVoteAcknowledgement.make[IO].unsafeRunSync()))
          sendToAllRef.get.unsafeRunSync() should be(
            List(
              PassFailVoteResults.make[IO](1, 1).unsafeRunSync(),
              AssassinVoteOutgoingEvent.make[IO](nickname1, Nil, missions).unsafeRunSync()))
        }
      }
    }

    "send the PassFailVoteResults message to _everyone_ when a mission is finished when bad guys win" in {
      forAll { roomId: RoomId =>
        new context {

          val sendRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val sendToAllRef = Ref.of[IO, List[OutgoingEvent]](Nil).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val nickname2 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def send(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = sendRef.set(Some(outgoingEvent))
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.update(_ ::: List(event))
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def questVote(nickname: Nickname, vote: QuestVote): IO[QuestVotingEnum] =
                  IO.pure(FinishedVote(List(QuestVote(true), QuestVote(false)),
                    BadGuyVictory(BadPlayerRole(nickname1, Assassin), None, GoodPlayerRole(nickname2, Merlin), Nil, Nil, BadGuys)))
              }
            }
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            QuestVoteEvent(QuestVote(false)),
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendRef.get.unsafeRunSync() should be(Some(QuestVoteAcknowledgement.make[IO].unsafeRunSync()))
          sendToAllRef.get.unsafeRunSync() should be(
            List(
              PassFailVoteResults.make[IO](1, 1).unsafeRunSync(),
              GameOverOutgoingEvent.make[IO](nickname1, None, nickname2, Nil, Nil, BadGuys).unsafeRunSync()))
        }
      }
    }
  }

  "IncomingAsssassinVote" should {
    "send out GameOverOutgoingEvent when the AssassinVote is done" in {
      forAll { roomId: RoomId =>
        new context {

          val sendToAllRef = Ref.of[IO, Option[OutgoingEvent]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)
          val nickname2 = Nickname(java.util.UUID.randomUUID().toString)
          val missions = IO.fromEither(Missions.fromPlayers(5)).unsafeRunSync()

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def sendToAll(event: OutgoingEvent): IO[Unit] = sendToAllRef.set(Some(event))
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def assassinVote(assassin: Nickname, guess: Nickname): IO[GameOver] =
                  IO.pure(GameOver(BadPlayerRole(nickname1, Assassin), None, GoodPlayerRole(nickname2, Merlin), Nil, Nil, BadGuys))
              }
            }
          }

          val ctxRef = Ref.of[IO, Option[ConnectionContext]](Some(ConnectionContext(nickname1, roomId))).unsafeRunSync()
          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          EventManager.handleEvent[IO](
            IncomingAssassinVote(nickname1),
            userQueue,
            mockRoomManager,
            outgoingRef,
            ctxRef
          ).unsafeRunSync()

          sendToAllRef.get.unsafeRunSync() should be(
            Some(GameOverOutgoingEvent.make[IO](nickname1, None, nickname2, Nil, Nil, BadGuys).unsafeRunSync()))
        }
      }
    }
  }

  "Disconnect Stuff" should {
    "Make sure we send out ChangeInLobby on Disconnect" in {
      forAll { (roomId: RoomId) =>
        new context {

          val disconnectedRef = Ref.of[IO, Option[Nickname]](None).unsafeRunSync()
          val nickname1 = Nickname(java.util.UUID.randomUUID().toString)

          val mockOutgoingManager: OutgoingManager[IO] = new MockOutgoingManager {
            override def add(nickname: Nickname, respond: Queue[IO, OutgoingEvent]): IO[Unit] = IO.unit
            override def remove(nickname: Nickname): IO[Unit] = IO.unit
            override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = IO.unit
            override def sendToAll(event: OutgoingEvent): IO[Unit] = IO.unit
            override def disconnected(nickname: Nickname): IO[Unit] = disconnectedRef.set(Some(nickname))
          }

          val mockRoomManager: RoomManager[IO] = new RoomManager[IO] {
            override def create: IO[RoomId] = IO.pure(roomId)
            override def get(roomId: RoomId): IO[Room[IO]] = IO.pure {
              new MockRoom {
                override def players: IO[List[Nickname]] = IO(Nil)
                override def addUser(player: Nickname): IO[Unit] = IO.unit
                override def removePlayer(player: Nickname): IO[Unit] = IO.unit
              }
            }
          }

          val outgoingRef = Ref.of[IO, Map[RoomId, OutgoingManager[IO]]](Map(roomId -> mockOutgoingManager)).unsafeRunSync()

          val eventManager: EventManager[IO] = EventManager.buildOutgoing[IO](mockRoomManager, outgoingRef)
          val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()

          val mvar: MVar[IO, Unit] = MVar.empty[IO, Unit].unsafeRunSync()

          val fiber = eventManager.interpret(
            userQueue,
            Stream.eval(IO.pure(JoinGame(nickname1, roomId))) ++ Stream.eval(mvar.put(())).flatMap(_ => Stream.never[IO])).start.unsafeRunSync()

          mvar.take.timeout(1 second).flatMap(_ => fiber.cancel).unsafeRunSync()

          disconnectedRef.get.unsafeRunSync() should be(Some(nickname1))
        }
      }
    }
  }

  trait context {}
}