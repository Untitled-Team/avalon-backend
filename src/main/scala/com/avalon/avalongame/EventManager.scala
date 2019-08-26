package com.avalon.avalongame

import cats.implicits._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.effect.implicits._
import cats.temp.par._
import com.avalon.avalongame.events._
import fs2._
import fs2.concurrent.Queue

trait EventManager[F[_]] {
  def interpret(respond: Queue[F, OutgoingEvent], events: Stream[F, IncomingEvent]): F[Unit]
}

object EventManager {
  val r = scala.util.Random

  case class UsernameWithSend[F[_]](nickname: Nickname, respond: Queue[F, OutgoingEvent])

  def build[F[_]: Concurrent : Par](roomManager: RoomManager[F], roomIdGenerator: RoomIdGenerator[F]): F[EventManager[F]] =
    Ref.of[F, Map[RoomId, List[UsernameWithSend[F]]]](Map.empty).map { ref =>
      new EventManager[F] {

        //it's possible we could have two rooms with same Id, but we don't care right now.
        def interpret(respond: Queue[F, OutgoingEvent], events: Stream[F, IncomingEvent]): F[Unit] =
          events.evalMap {
            case CreateGame(nickname, config) =>
              (for {
                roomId <- roomIdGenerator.generate
                _      <- roomManager.create(roomId, config)
                room   <- roomManager.get(roomId)
                _      <- room.addUser(User(nickname))
                _      <- ref.update(_ + (roomId -> List(UsernameWithSend[F](nickname, respond))))
                _      <- respond.enqueue1(GameCreated(roomId))
              } yield ()).onError {
                case t => Sync[F].delay(println(s"We encountered an error while creating game ${t.getStackTrace}"))
              }
            case JoinGame(nickname, roomId) =>
              for {
                room     <- roomManager.get(roomId)
                _        <- room.addUser(User(nickname))
                mapping  <- ref.get
                outgoing <- Sync[F].fromOption(mapping.get(roomId), NoRoomFoundForChatId)
                _        <- outgoing.parTraverse(_.respond.enqueue1(UserJoined(nickname))) //could run things in parallel here
                _        <- ref.update(_ + (roomId -> (UsernameWithSend(nickname, respond) :: outgoing)))
                //need to use MVar here so we don't accidentally update it and erase a user!!!!!!!!!!!
                roomInfo <- room.info
                _        <- respond.enqueue1(JoinedRoom(roomInfo))
              } yield ()
          }.compile.drain
      }
    }
}