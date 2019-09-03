package com.avalon.avalongame

import cats.data.{NonEmptyList, State, StateT}
import cats.effect._
import cats.effect.concurrent.{MVar, Ref}
import cats.effect.implicits._
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
  def build[F[_]](randomAlg: RandomAlg[F])(implicit F: Concurrent[F]): F[RoomManager[F]] =
    Ref.of[F, Map[RoomId, Room[F]]](Map.empty).map { ref =>
      new RoomManager[F] {
        //should fail if the room already existed?
        def create(roomId: RoomId, config: GameConfig): F[Unit] = Room.build(randomAlg, roomId, config).flatMap { room =>
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

