package com.avalon.avalongame

import cats.data.NonEmptyList
import cats.effect.{Bracket, Concurrent, Resource, Sync}
import cats.effect.concurrent.{MVar, Ref}
import cats.implicits._
import fs2._

import scala.util.control.NoStackTrace



sealed trait GameState

//final case class Username(value: String) extends AnyVal



//final case class Room(users: List[User])

trait RoomManager[F[_]] {
  def create(roomId: RoomId, config: GameConfig): F[Unit]
  def get(roomId: RoomId): F[Room[F]]
//  def addUser(roomId: RoomId, user: User): F[Unit]
//  def userEvents(user: User): Stream[F, Event]
}

object RoomManager {
  //timeouts on get/reads?
  //could back this by mules and have them auto expire if not updated in a while
  def build[F[_]](implicit F: Concurrent[F]): F[RoomManager[F]] =
    Ref.of[F, Map[RoomId, Room[F]]](Map.empty).map { ref =>
      new RoomManager[F] {
        //should fail if the room already existed?
        def create(roomId: RoomId, config: GameConfig): F[Unit] = Room.build(roomId, config).flatMap { room =>
          ref.update(rooms => if (rooms.get(roomId).isEmpty) rooms + (roomId -> room) else rooms)
        }

        def get(chatId: RoomId): F[Room[F]] =
          for {
            rooms <- ref.get
            room <- F.fromOption(rooms.get(chatId), NoRoomFoundForChatId)
          } yield room
      }
    }
}

trait Room[F[_]] {
  def info: F[RoomInfo]
  def users: F[List[User]]
  def addUser(user: User): F[Unit]
}

object Room {

  case class InternalRoom(users: List[User])
  //timeouts on get/reads?
  def build[F[_]](roomId: RoomId, config: GameConfig)(implicit F: Concurrent[F]): F[Room[F]] =
    MVar.of(InternalRoom(Nil)).map { mvar =>
      new Room[F] {
        def info: F[RoomInfo] = users.map(RoomInfo(_, config))

        def users: F[List[User]] = mvar.read.map(_.users)

        def addUser(user: User): F[Unit] =
          Sync[F].bracket(mvar.take.map(room => room.copy(user :: room.users)))(_ => Sync[F].unit)(mvar.put)
      }
    }
}