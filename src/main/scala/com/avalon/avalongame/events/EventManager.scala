package com.avalon.avalongame.events

import cats.implicits._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.effect.implicits._
import cats.temp.par._
import com.avalon.avalongame.common._
import com.avalon.avalongame.room.{RoomIdGenerator, RoomManager}
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

  def build[F[_]: Par](roomManager: RoomManager[F], roomIdGenerator: RoomIdGenerator[F])(implicit F: Concurrent[F]): F[EventManager[F]] =
    Ref.of[F, Map[RoomId, OutgoingManager[F]]](Map.empty).map { outgoingRef =>
      new EventManager[F] {

        //it's possible we could have two rooms with same Id, but we don't care right now.
        def interpret(respond: Queue[F, OutgoingEvent], events: Stream[F, IncomingEvent]): F[Unit] =
          Stream.eval(Ref.of[F, Option[ConnectionContext]](None)).flatMap { context =>
            events.evalMap {
              case CreateGame(nickname, config) => //can't do this if ConnectionContext exists
                (for {
                  roomId   <- roomIdGenerator.generate
                  _        <- roomManager.create(roomId, config)
                  room     <- roomManager.get(roomId)
                  _        <- room.addUser(User(nickname))
                  outgoing <- OutgoingManager.build[F](UsernameWithSend[F](nickname, respond))
                  _        <- outgoingRef.update(_ + (roomId -> outgoing))
                  _        <- context.update(_ => Some(ConnectionContext(nickname, roomId)))
                  _        <- respond.enqueue1(GameCreated(roomId))
                } yield ()).onError {
                  case t => Sync[F].delay(println(s"We encountered an error while creating game for $nickname,  ${t.getStackTrace}"))
                }

              case JoinGame(nickname, roomId) => //can't do this if ConnectionContext exists
                (for {
                  room     <- roomManager.get(roomId)
                  _        <- room.addUser(User(nickname))
                  mapping  <- outgoingRef.get
                  outgoing <- Sync[F].fromOption(mapping.get(roomId), NoRoomFoundForChatId)
                  _        <- outgoing.broadcast(nickname, UserJoined(nickname))
                  _        <- outgoing.add(UsernameWithSend(nickname, respond))
                  _        <- context.update(_ => Some(ConnectionContext(nickname, roomId)))
                  roomInfo <- room.info
                  _        <- respond.enqueue1(JoinedRoom(roomInfo))
                } yield ()).onError {
                  case t => Sync[F].delay(println(s"We encountered an error while joining game for $nickname,  ${t.getStackTrace}"))
                }

              case StartGame =>
                (for {
                  ctx           <- context.get.flatMap(c => F.fromOption(c, NoContext))
                  room          <- roomManager.get(ctx.roomId)
                  repr          <- room.startGame
                  outgoingEvent <- representationToGameCreated(ctx.nickname, repr)
                  mapping       <- outgoingRef.get
                  outgoing      <- Sync[F].fromOption(mapping.get(ctx.roomId), NoRoomFoundForChatId)
                  _             <- outgoing.broadcastUserSpecific(ctx.nickname, representationToGameCreated(_, repr).widen[OutgoingEvent])
                  _             <- respond.enqueue1(outgoingEvent)
                } yield ()).onError {
                  case t => Sync[F].delay(println(s"We encountered an error while starting game for ???,  ${t.getStackTrace}"))
                }

              case MissionLeaderProposal(players) =>
                (for {
                  ctx           <- context.get.flatMap(c => F.fromOption(c, NoContext))
                  room          <- roomManager.get(ctx.roomId)
                  proposal      <- room.proposeMission(ctx.nickname, players.map(User(_)))
                  outgoingEvent =  MissionProposalEvent(proposal.missionNumber, proposal.missionLeader, proposal.users.map(_.nickname))
                  mapping       <- outgoingRef.get
                  outgoing      <- Sync[F].fromOption(mapping.get(ctx.roomId), NoRoomFoundForChatId)
                  _             <- outgoing.sendToAll(outgoingEvent)
                } yield ()).onError {
                  case t => Sync[F].delay(println(s"We encountered an error with mission leader proposal for ???,  ${t.getStackTrace}"))
                }

              case MissionProposalVote(_, _) => F.unit
            }
          }.compile.drain
      }
    }
}

trait OutgoingManager[F[_]] {
  def add(usernameWithSend: UsernameWithSend[F]): F[Unit]
  //broadcasts message to everyone but the nickname provided
  def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): F[Unit]
  def broadcastUserSpecific(nickname: Nickname, outgoingF: Nickname => F[OutgoingEvent]): F[Unit]
  def sendToAll(event: OutgoingEvent): F[Unit]
}

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

        def sendToAll(event: OutgoingEvent): F[Unit] = ref.get.flatMap(_.parTraverse(u => u.respond.enqueue1(event))).void
      }
    }
}