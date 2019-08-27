package com.avalon.avalongame

import cats.implicits._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.effect.implicits._
import cats.temp.par._
import com.avalon.avalongame.events._
import fs2._
import fs2.concurrent.Queue

case class UsernameWithSend[F[_]](nickname: Nickname, respond: Queue[F, OutgoingEvent])

trait EventManager[F[_]] {
  def interpret(respond: Queue[F, OutgoingEvent], events: Stream[F, IncomingEvent]): F[Unit]
}

object EventManager {
  val r = scala.util.Random

  def build[F[_]: Par](roomManager: RoomManager[F], roomIdGenerator: RoomIdGenerator[F])(implicit F: Concurrent[F]): F[EventManager[F]] =
    Ref.of[F, Map[RoomId, OutgoingManager[F]]](Map.empty).map { ref =>
      new EventManager[F] {

        //it's possible we could have two rooms with same Id, but we don't care right now.
        def interpret(respond: Queue[F, OutgoingEvent], events: Stream[F, IncomingEvent]): F[Unit] =
          events.evalMap {
            case CreateGame(nickname, config) =>
              (for {
                roomId   <- roomIdGenerator.generate
                _        <- roomManager.create(roomId, config)
                room     <- roomManager.get(roomId)
                _        <- room.addUser(User(nickname))
                outgoing <- OutgoingManager.build[F](UsernameWithSend[F](nickname, respond))
                _        <- ref.update(_ + (roomId -> outgoing))
                _        <- respond.enqueue1(GameCreated(roomId))
              } yield ()).onError {
                case t => Sync[F].delay(println(s"We encountered an error while creating game for $nickname,  ${t.getStackTrace}"))
              }
            case JoinGame(nickname, roomId) =>
              (for {
                room     <- roomManager.get(roomId)
                _        <- room.addUser(User(nickname))
                mapping  <- ref.get
                outgoing <- Sync[F].fromOption(mapping.get(roomId), NoRoomFoundForChatId)
                _        <- outgoing.broadcast(nickname, UserJoined(nickname))
                _        <- outgoing.add(UsernameWithSend(nickname, respond))
                roomInfo <- room.info
                _        <- respond.enqueue1(JoinedRoom(roomInfo))
              } yield ()).onError {
                case t => Sync[F].delay(println(s"We encountered an error while joining game for $nickname,  ${t.getStackTrace}"))
              }

            case StartGame(roomId) => Sync[F].unit
          }.compile.drain
      }
    }
}

trait OutgoingManager[F[_]] {
  //broadcasts message to everyone but the nickname provided
  def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): F[Unit]
  def add(usernameWithSend: UsernameWithSend[F]): F[Unit]
}

object OutgoingManager {
  def build[F[_]: Par](usernameWithSend: UsernameWithSend[F])(implicit F: Concurrent[F]): F[OutgoingManager[F]] =
    Ref.of[F, List[UsernameWithSend[F]]](List(usernameWithSend)).map { ref =>
      new OutgoingManager[F] {
        override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): F[Unit] =
          ref.get.flatMap { l =>
            l.filter(_.nickname =!= nickname).parTraverse(_.respond.enqueue1(outgoingEvent))
          }.void

        def add(usernameWithSend: UsernameWithSend[F]): F[Unit] =
          ref.update(usernameWithSend :: _)
      }
    }
}