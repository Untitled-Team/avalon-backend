package com.avalon.avalongame

import cats.data.NonEmptyList
import cats.effect.{Bracket, Concurrent, Resource, Sync}
import cats.effect.concurrent.{MVar, Ref}
import cats.implicits._
import fs2._
import scala.util.control.NoStackTrace

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

  case object NotEnoughPlayers extends RuntimeException with NoStackTrace

  case class GameRepresentation(users: List[User],
                                state: GameState)


  case class InternalRoom(users: List[User])
  //timeouts on get/reads?
  def build[F[_]](roomId: RoomId, config: GameConfig)(implicit F: Concurrent[F]): F[Room[F]] =
    MVar.of(InternalRoom(Nil)).map { mvar =>
      new Room[F] {
        def info: F[RoomInfo] = users.map(RoomInfo(_, config))

        def users: F[List[User]] = mvar.read.map(_.users)

        def addUser(user: User): F[Unit] =
          mvar.take.flatMap(room => mvar.put(room.copy(user :: room.users)))

//        def startGame: F[GameRepresentation]
      }
    }
}

sealed trait GameState
case object Creation
case class MissionProposing(missionLeader: User) extends GameState
case class MissiongProposed(voters: NonEmptyList[User]) extends GameState