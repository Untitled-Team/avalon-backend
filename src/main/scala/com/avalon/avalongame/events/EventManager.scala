package com.avalon.avalongame.events

import java.io.{PrintWriter, StringWriter}

import cats.implicits._
import cats.effect._
import cats.effect.concurrent.{Ref, Semaphore}
import cats.effect.implicits._
import cats.temp.par._
import com.avalon.avalongame.common._
import com.avalon.avalongame.room._
import fs2._
import fs2.concurrent.Queue

import scala.util.control.NoStackTrace

case class OutgoingConnectionContext[F[_]](nickname: Nickname,
                                           respond: Option[Queue[F, OutgoingEvent]],
                                           events: List[OutgoingEvent],
                                           eventsSinceDisconnect: Option[List[OutgoingEvent]] = None)

trait EventManager[F[_]] {
  def interpret(respond: Queue[F, OutgoingEvent], events: Stream[F, IncomingEvent]): F[Unit]
}

case class ConnectionContext(nickname: Nickname, roomId: RoomId)

object EventManager {
  import Utils._
  val r = scala.util.Random

  case object NoContext extends RuntimeException with NoStackTrace

  private[events] def buildOutgoing[F[_]: Par](roomManager: RoomManager[F],
                                               outgoingRef: Ref[F, Map[RoomId, OutgoingManager[F]]])(implicit F: Concurrent[F]): EventManager[F] =
    new EventManager[F] {
        //it's possible we could have two rooms with same Id, but we don't care right now.
        def interpret(respond: Queue[F, OutgoingEvent], events: Stream[F, IncomingEvent]): F[Unit] =
          Stream.eval(Ref.of[F, Option[ConnectionContext]](None)).flatMap { context =>
            events
              .evalMap {
                handleEvent(_, respond, roomManager, outgoingRef, context)
                  .handleErrorWith(t => F.delay(println(s"We encountered an error while trying to handle the incoming event:$t")))
              }
              .onFinalize { //disconnected
                //if they are in the lobby we should remove them from room and then remove them from the OutgoingManager
                //if they in a game we should remove their Responder and hopefully they will join back
                  (for {
                    ctx           <- context.get.flatMap(c => F.fromOption(c, NoContext))
                    mapping       <- outgoingRef.get//also need to remove the responder from the outgoing, but leave the username in case they reocnnect!
                    outgoing      <- Sync[F].fromOption(mapping.get(ctx.roomId), NoRoomFoundForChatId)
                    _             <- outgoing.disconnected(ctx.nickname)
                  } yield ()).onError {
                    case t => Sync[F].delay(println(s"We encountered an error while disconnecting player,  ${t.getStackTrace}"))
                  }
              }
          }.compile.drain
    }

  def build[F[_]: Par](roomManager: RoomManager[F])(implicit F: Concurrent[F]): F[EventManager[F]] =
    Ref.of[F, Map[RoomId, OutgoingManager[F]]](Map.empty).map { outgoingRef =>
      buildOutgoing(roomManager, outgoingRef)
    }

  //this shouldn't need the respond Queue, it should be able tou se the OutgoingManager
  def handleEvent[F[_]: Par](event: IncomingEvent,
                             respond: Queue[F, OutgoingEvent],
                             roomManager: RoomManager[F],
                             outgoingRef: Ref[F, Map[RoomId, OutgoingManager[F]]],
                             context: Ref[F, Option[ConnectionContext]])(implicit F: Concurrent[F]): F[Unit] = {
    event match {
      case CreateGame(nickname) => //can't do this if ConnectionContext exists
        (for {
          _        <- context.get.flatMap(c => if (c.isEmpty) F.unit else F.raiseError[Unit](ContextExistsAlready))//tests
          roomId   <- roomManager.create
          room     <- roomManager.get(roomId)
          _        <- room.addUser(nickname)
          outgoing <- OutgoingManager.build[F]
          _        <- outgoing.add(nickname, respond)
          _        <- outgoingRef.update(_ + (roomId -> outgoing))
          _        <- context.update(_ => Some(ConnectionContext(nickname, roomId)))
          players  <- room.players
          _        <- outgoing.send(nickname, MoveToLobby(roomId, players))
        } yield ()).onError {
          case t => Sync[F].delay(println(s"We encountered an error while creating game for $nickname,  ${t.getStackTrace}"))
        }

      case JoinGame(nickname, roomId) => //can't do this if ConnectionContext exists
        (for {
          _        <- context.get.flatMap(c => if (c.isEmpty) F.unit else F.raiseError[Unit](ContextExistsAlready))//tests
          room     <- roomManager.get(roomId)
          _        <- room.addUser(nickname)
          players  <- room.players
          mapping  <- outgoingRef.get
          outgoing <- Sync[F].fromOption(mapping.get(roomId), NoRoomFoundForChatId)
          _        <- outgoing.broadcast(nickname, ChangeInLobby(players))
          _        <- outgoing.add(nickname, respond)
          _        <- context.update(_ => Some(ConnectionContext(nickname, roomId)))
          _        <- outgoing.send(nickname, MoveToLobby(roomId, players))
        } yield ()).onError {
          case t => Sync[F].delay(println(s"We encountered an error while joining game for $nickname,  $t"))
        }

      case LeaveGame =>
        (for {
          ctx      <- context.get.flatMap(c => F.fromOption(c, NoContext))
          room     <- roomManager.get(ctx.roomId)
          mapping  <- outgoingRef.get
          outgoing <- Sync[F].fromOption(mapping.get(ctx.roomId), NoRoomFoundForChatId)
          _        <- room.removePlayer(ctx.nickname)
          _        <- outgoing.remove(ctx.nickname)
          players  <- room.players
          _        <- outgoing.sendToAll(ChangeInLobby(players))
          _        <- respond.enqueue1(GameLeft)
          _        <- context.update(_ => None)
        } yield ()).onError {
          case t => Sync[F].delay(println(s"We encountered an error while disconnecting player,  ${t.getStackTrace}"))
        }

      case Reconnect(nickname, roomId) =>
        (for {
          _        <- context.get.flatMap(c => if (c.isEmpty) F.unit else F.raiseError[Unit](ContextExistsAlready))//tests
          room     <- roomManager.get(roomId)
          _        <- room.players.ensure(NicknameNotFoundInRoom(nickname))(_.contains(nickname))
          mapping  <- outgoingRef.get
          outgoing <- Sync[F].fromOption(mapping.get(roomId), NoRoomFoundForChatId)
          _        <- outgoing.reconnect(nickname, respond)
          _        <- context.update(_ => Some(ConnectionContext(nickname, roomId)))
        } yield ()).onError {
          case t => Sync[F].delay(println(s"We encountered an error while creating game for $nickname,  ${t.getStackTrace}"))
        }

      case StartGame =>
        (for {
          ctx           <- context.get.flatMap(c => F.fromOption(c, NoContext))
          room          <- roomManager.get(ctx.roomId)
          roles         <- room.startGame
          mapping       <- outgoingRef.get
          outgoing      <- Sync[F].fromOption(mapping.get(ctx.roomId), NoRoomFoundForChatId)
          _             <- outgoing.sendToAllUserSpecific(playerRole(_, roles).widen[OutgoingEvent])
        } yield ()).onError {
          case t => Sync[F].delay(println(s"We encountered an error while starting game for ???,  ${t.getStackTrace}"))
        }

      case PlayerReady => F.unit
        (for {
          ctx           <- context.get.flatMap(c => F.fromOption(c, NoContext))
          room          <- roomManager.get(ctx.roomId)
          result        <- room.playerReady(ctx.nickname)
          mapping       <- outgoingRef.get
          outgoing      <- Sync[F].fromOption(mapping.get(ctx.roomId), NoRoomFoundForChatId)
          _             <- outgoing.send(ctx.nickname, PlayerReadyAcknowledgement)
          _ <- result match {
            case AllReady(missionNumber, leader, missions) =>
              outgoing.sendToAll(TeamAssignmentPhase(missionNumber, leader, missions))
            case _ => F.unit
          }
        } yield ()).onError {
          case t => Sync[F].delay(println(s"We encountered an error while handling PlayerReady for ???,  ${t.getStackTrace}"))
        }

      case ProposeParty(players) => F.unit
        (for {
          ctx           <- context.get.flatMap(c => F.fromOption(c, NoContext))
          room          <- roomManager.get(ctx.roomId)
          proposal      <- room.proposeMission(ctx.nickname, players)
          outgoingEvent =  ProposedParty(proposal.players)
          mapping       <- outgoingRef.get
          outgoing      <- Sync[F].fromOption(mapping.get(ctx.roomId), NoRoomFoundForChatId)
          _             <- outgoing.sendToAll(outgoingEvent)
        } yield ()).onError {
          case t => Sync[F].delay(println(s"We encountered an error with ProposeParty for ???,  ${t.getStackTrace}"))
        }

      case PartyApprovalVote(vote) =>
        (for {
          ctx           <- context.get.flatMap(c => F.fromOption(c, NoContext))
          room          <- roomManager.get(ctx.roomId)
          voteStatus    <- room.teamVote(ctx.nickname, vote)
          mapping       <- outgoingRef.get
          outgoing      <- Sync[F].fromOption(mapping.get(ctx.roomId), NoRoomFoundForChatId)
          _             <- outgoing.send(ctx.nickname, PartyApprovalVoteAcknowledgement)
          _ <- voteStatus match {
            case TeamPhaseStillVoting => F.unit
            case FailedVote(missionLeader, missionNumber, _, missions) =>
              outgoing.sendToAll(TeamAssignmentPhase(missionNumber, missionLeader, missions))
            case SuccessfulVote(_) => outgoing.sendToAll(PartyApproved)
          }
        } yield ()).onError {
          case t => Sync[F].delay(println(s"We encountered an error with PartyApprovalVote for ???,  ${t.getMessage}"))
        }

      case QuestVoteEvent(vote) =>
        (for {
          ctx           <- context.get.flatMap(c => F.fromOption(c, NoContext))
          room          <- roomManager.get(ctx.roomId)
          voteStatus    <- room.questVote(ctx.nickname, vote)
          mapping       <- outgoingRef.get
          outgoing      <- Sync[F].fromOption(mapping.get(ctx.roomId), NoRoomFoundForChatId)
          _             <- outgoing.send(ctx.nickname, QuestVoteAcknowledgement)
          _ <- voteStatus match {
            case QuestPhaseStillVoting => F.unit
            case FinishedVote(votes) =>
              outgoing.sendToAll(PassFailVoteResults(votes.count(_ === QuestVote(true)), votes.count(_ === QuestVote(false))))
          }
        } yield ()).onError {
          case t => Sync[F].delay(println(s"We encountered an error with PartyApprovalVote for ???,  ${t.getStackTrace}"))
        }

      case QuestVotesDisplayed => F.unit
        (for {
          ctx           <- context.get.flatMap(c => F.fromOption(c, NoContext))
          room          <- roomManager.get(ctx.roomId)
          resultsStatus <- room.questResultsSeen(ctx.nickname)
          mapping       <- outgoingRef.get
          outgoing      <- Sync[F].fromOption(mapping.get(ctx.roomId), NoRoomFoundForChatId)
          _             <- outgoing.send(ctx.nickname, QuestDisplayAcknowledgement)
          _ <- resultsStatus match {
            case StillViewingQuestResults => F.unit
            case AssassinVote(assassin, goodGuys) =>
              outgoing.sendToAll(AssassinVoteOutgoingEvent(assassin, goodGuys.map(_.nickname)))
            case BadGuyVictory(assassin, _, merlin, goodGuys, badGuys, winningTeam) =>
              outgoing.sendToAll(GameOverOutgoingEvent(assassin.nickname, None, merlin.nickname, goodGuys, badGuys, winningTeam))
            case GameContinues(missionLeader, missionNumber, missions) =>
              outgoing.sendToAll(TeamAssignmentPhase(missionNumber, missionLeader, missions))
          }
        } yield ()).onError {
          case t => Sync[F].delay(println(s"We encountered an error with PartyApprovalVote for ???,  ${t.getStackTrace}"))
        }

      case IncomingAssassinVote(guess) =>
        (for {
          ctx      <- context.get.flatMap(c => F.fromOption(c, NoContext))
          room     <- roomManager.get(ctx.roomId)
          gameOver <- room.assassinVote(ctx.nickname, guess)
          mapping  <- outgoingRef.get
          outgoing <- Sync[F].fromOption(mapping.get(ctx.roomId), NoRoomFoundForChatId)
          outgoingEvent = GameOverOutgoingEvent(
            gameOver.assassin.nickname,
            gameOver.assassinGuess,
            gameOver.merlin.nickname,
            gameOver.goodGuys,
            gameOver.badGuys,
            gameOver.winningTeam)
          _ <- outgoing.sendToAll(outgoingEvent)
        } yield ()).onError {
          case t => Sync[F].delay {
            println(s"We encountered an error with IncomingAssassinVote for ???,  $t")
          }
        }
    }
  }
}

trait OutgoingManager[F[_]] {
  def add(nickname: Nickname, respond: Queue[F, OutgoingEvent]): F[Unit]
  def remove(nickname: Nickname): F[Unit]
  def send(nickname: Nickname, outgoingEvent: OutgoingEvent): F[Unit]
  def disconnected(nickname: Nickname): F[Unit]
  def reconnect(nickname: Nickname, respond: Queue[F, OutgoingEvent]): F[Unit]
  def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): F[Unit]
  def broadcastUserSpecific(nickname: Nickname, outgoingF: Nickname => F[OutgoingEvent]): F[Unit]
  def sendToAll(event: OutgoingEvent): F[Unit]
  def sendToAllUserSpecific(outgoingF: Nickname => F[OutgoingEvent]): F[Unit]
}

object OutgoingManager {
  private[events] def buildPrivate[F[_]: Par](sem: Semaphore[F],
                                              ref: Ref[F, List[OutgoingConnectionContext[F]]])(implicit F: Concurrent[F]): OutgoingManager[F] =
    new OutgoingManager[F] {
      def add(nickname: Nickname, respond: Queue[F, OutgoingEvent]): F[Unit] =
        sem.withPermit(ref.update(OutgoingConnectionContext(nickname, Some(respond), Nil) :: _))

      def remove(nickname: Nickname): F[Unit] =
        sem.withPermit(ref.update(_.filter(_.nickname =!= nickname)))

      def disconnected(nickname: Nickname): F[Unit] =
        sem.withPermit {
          ref.update(
            _.map(ctx =>
              if (nickname === ctx.nickname) ctx.copy(eventsSinceDisconnect = Some(Nil), respond = Option.empty[Queue[F, OutgoingEvent]]) else ctx))
        }

      def reconnect(nickname: Nickname, respond: Queue[F, OutgoingEvent]): F[Unit] =
        sem.withPermit {
          for {
            connections <- ref.get
            c <- F.fromOption(connections.find(_.nickname === nickname), NoOutgoingEventContextExistsForUser(nickname))
            eventsToSend = c.eventsSinceDisconnect.getOrElse(Nil)
            _ <- ref.update(_.map(ctx => if (nickname === ctx.nickname) ctx.copy(respond = Some(respond), eventsSinceDisconnect = None) else ctx))
            _ <- eventsToSend.parTraverse_(event => respond.enqueue1(event))
          } yield {}
        }

      def send(nickname: Nickname, outgoingEvent: OutgoingEvent): F[Unit] =
        sem.withPermit {
          for {
            connections <- ref.get
            c <- F.fromOption(connections.find(_.nickname === nickname), NoOutgoingEventContextExistsForUser(nickname))
            _ <- updateContext(nickname, ref, outgoingEvent)
            _ <- c.respond.traverse_(_.enqueue1(outgoingEvent))
          } yield ()
        }

      def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): F[Unit] =
        broadcastUserSpecific(nickname, _ => F.pure(outgoingEvent))

      def broadcastUserSpecific(nickname: Nickname, outgoingF: Nickname => F[OutgoingEvent]): F[Unit] =
        sem.withPermit {
          for {
            l <- ref.get
            _ <- l.filter(_.nickname =!= nickname).parTraverse { u =>
              outgoingF(u.nickname)
                .flatTap(event => u.respond.traverse_(_.enqueue1(event)))
                .flatTap(outgoingEvent => updateContext(u.nickname, ref, outgoingEvent))
            }
          } yield ()
        }

      def sendToAll(event: OutgoingEvent): F[Unit] = sendToAllUserSpecific(_ => F.pure(event))

      def sendToAllUserSpecific(outgoingF: Nickname => F[OutgoingEvent]): F[Unit] =
        sem.withPermit {
          ref.get.flatMap(_.parTraverse_ { u =>
            outgoingF(u.nickname)
              .flatTap(event => u.respond.traverse_(_.enqueue1(event)))
              .flatTap(event => updateContext(u.nickname, ref, event))
          })
        }
    }

  def updateContext[F[_]: Sync](nickname: Nickname, ref: Ref[F, List[OutgoingConnectionContext[F]]], outgoingEvent: OutgoingEvent): F[Unit] =
    ref.update(_.map { ctx =>
      if (nickname === ctx.nickname) {
        val disconnectEvents = ctx.eventsSinceDisconnect.map(_ :+ outgoingEvent)
        ctx.copy(events = ctx.events :+ outgoingEvent, eventsSinceDisconnect = disconnectEvents)
      }
      else ctx
    })

  //don't really need to provide the usernameWithSend when we build. Would make mocking easier if we don't
  def build[F[_]: Par](implicit F: Concurrent[F]): F[OutgoingManager[F]] =
    Semaphore[F](1).flatMap { sem =>
      Ref.of[F, List[OutgoingConnectionContext[F]]](Nil).map { ref =>
        buildPrivate(sem, ref)
      }
    }
}