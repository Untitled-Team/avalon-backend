package com.avalon.avalongame.events

import cats.implicits._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.effect.implicits._
import cats.temp.par._
import com.avalon.avalongame.common._
import com.avalon.avalongame.room._
import fs2._
import fs2.concurrent.Queue

import scala.util.control.NoStackTrace

case class UsernameWithSend[F[_]](nickname: Nickname, respond: Queue[F, OutgoingEvent])

trait EventManager[F[_]] {
  def interpret(respond: Queue[F, OutgoingEvent], events: Stream[F, IncomingEvent]): F[Unit]
}

case class ConnectionContext(nickname: Nickname, roomId: RoomId)

object EventManager {
  import Utils._
  val r = scala.util.Random

  case object NoContext extends RuntimeException with NoStackTrace

  private[events] def buildOutgoing[F[_]: Par](roomManager: RoomManager[F],
                                               roomIdGenerator: RoomIdGenerator[F],
                                               outgoingRef: Ref[F, Map[RoomId, OutgoingManager[F]]])(implicit F: Concurrent[F]): EventManager[F] = {
    new EventManager[F] {
        //it's possible we could have two rooms with same Id, but we don't care right now.
        def interpret(respond: Queue[F, OutgoingEvent], events: Stream[F, IncomingEvent]): F[Unit] =
          Stream.eval(Ref.of[F, Option[ConnectionContext]](None)).flatMap { context =>
            events.evalMap { event => {
              event match {
                case CreateGame(nickname) => //can't do this if ConnectionContext exists
                  (for {
                    roomId   <- roomIdGenerator.generate
                    _        <- roomManager.create(roomId)
                    room     <- roomManager.get(roomId)
                    _        <- room.addUser(nickname)
                    outgoing <- OutgoingManager.build[F](UsernameWithSend[F](nickname, respond))
                    _        <- outgoingRef.update(_ + (roomId -> outgoing))
                    _        <- context.update(_ => Some(ConnectionContext(nickname, roomId)))
                    players  <- room.players
                    _        <- respond.enqueue1(MoveToLobby(roomId, players))
                  } yield ()).onError {
                    case t => Sync[F].delay(println(s"We encountered an error while creating game for $nickname,  ${t.getStackTrace}"))
                  }

                case JoinGame(nickname, roomId) => //can't do this if ConnectionContext exists
                  (for {
                    room     <- roomManager.get(roomId)
                    _        <- room.addUser(nickname)
                    players  <- room.players
                    mapping  <- outgoingRef.get
                    outgoing <- Sync[F].fromOption(mapping.get(roomId), NoRoomFoundForChatId)
                    _        <- outgoing.broadcast(nickname, ChangeInLobby(players))
                    _        <- outgoing.add(UsernameWithSend(nickname, respond))
                    _        <- context.update(_ => Some(ConnectionContext(nickname, roomId)))
                    _        <- respond.enqueue1(MoveToLobby(roomId, players))
                  } yield ()).onError {
                    case t => Sync[F].delay(println(s"We encountered an error while joining game for $nickname,  ${t.getStackTrace}"))
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
                    _ <- voteStatus match {
                      case TeamPhaseStillVoting => F.unit
                      case FailedVote(missionLeader, missionNumber, _, missions) =>
                        outgoing.sendToAll(TeamAssignmentPhase(missionNumber, missionLeader, missions))
                      case SuccessfulVote(_) => outgoing.sendToAll(PartyApproved)
                    }
                  } yield ()).onError {
                    case t => Sync[F].delay(println(s"We encountered an error with PartyApprovalVote for ???,  ${t.getStackTrace}"))
                  }

                case QuestVoteEvent(vote) =>
                  (for {
                    ctx           <- context.get.flatMap(c => F.fromOption(c, NoContext))
                    room          <- roomManager.get(ctx.roomId)
                    voteStatus    <- room.questVote(ctx.nickname, vote)
                    mapping       <- outgoingRef.get
                    outgoing      <- Sync[F].fromOption(mapping.get(ctx.roomId), NoRoomFoundForChatId)
                    _ <- voteStatus match {
                      case QuestPhaseStillVoting => F.unit
                      case FinishedVote(votes) =>
                        outgoing.sendToAll(PassFailVoteResults(votes.count(_ === QuestVote(true)), votes.count(_ === QuestVote(false))))
                    }
                  } yield ()).onError {
                    case t => Sync[F].delay(println(s"We encountered an error with PartyApprovalVote for ???,  ${t.getStackTrace}"))
                  }
              }}.handleErrorWith(t => F.delay(println(t)))
            }
          }.compile.drain
    }
  }

  def build[F[_]: Par](roomManager: RoomManager[F], roomIdGenerator: RoomIdGenerator[F])(implicit F: Concurrent[F]): F[EventManager[F]] =
    Ref.of[F, Map[RoomId, OutgoingManager[F]]](Map.empty).map { outgoingRef =>
      buildOutgoing(roomManager, roomIdGenerator, outgoingRef)
    }
}

trait OutgoingManager[F[_]] {
  def add(usernameWithSend: UsernameWithSend[F]): F[Unit]
  //broadcasts message to everyone but the nickname provided
  def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): F[Unit]
  def broadcastUserSpecific(nickname: Nickname, outgoingF: Nickname => F[OutgoingEvent]): F[Unit]
  def sendToAll(event: OutgoingEvent): F[Unit]
  def sendToAllUserSpecific(outgoingF: Nickname => F[OutgoingEvent]): F[Unit]
}

//needs tests to make sure it properly sends out the messages
object OutgoingManager {
  def build[F[_]: Par](usernameWithSend: UsernameWithSend[F])(implicit F: Concurrent[F]): F[OutgoingManager[F]] =
    Ref.of[F, List[UsernameWithSend[F]]](List(usernameWithSend)).map { ref =>
      new OutgoingManager[F] {
        def add(usernameWithSend: UsernameWithSend[F]): F[Unit] =
          ref.update(usernameWithSend :: _)

        def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): F[Unit] =
          broadcastUserSpecific(nickname, _ => F.pure(outgoingEvent))

        def broadcastUserSpecific(nickname: Nickname, outgoingF: Nickname => F[OutgoingEvent]): F[Unit] =
          ref.get.flatMap { l =>
            l.filter(_.nickname =!= nickname).parTraverse(u => outgoingF(u.nickname).flatMap(u.respond.enqueue1))
          }.void

        def sendToAll(event: OutgoingEvent): F[Unit] = sendToAllUserSpecific(_ => F.pure(event))

        def sendToAllUserSpecific(outgoingF: Nickname => F[OutgoingEvent]): F[Unit] =
          ref.get.flatMap(_.parTraverse(u => outgoingF(u.nickname).flatMap(event => u.respond.enqueue1(event))).void)
      }
    }
}