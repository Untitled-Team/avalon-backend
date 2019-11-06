package com.avalon.avalongame.room

import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._
import com.avalon.avalongame.RandomAlg
import com.avalon.avalongame.common._

trait RoomManager[F[_]] {
  def create: F[RoomId]
  def get(roomId: RoomId): F[Room[F]]
}

object RoomManager {
  //need expiration for rooms
  def build[F[_]](randomAlg: RandomAlg[F], roomIdGenerator: RoomIdGenerator[F])(implicit F: Concurrent[F]): F[RoomManager[F]] =
    Ref.of[F, Map[RoomId, Room[F]]](Map.empty).map { ref =>
      new RoomManager[F] {
        val create: F[RoomId] =
          for {
            roomId <- roomIdGenerator.generate
            room <- Room.build(randomAlg, roomId)
            updated <-
              ref.modify { rooms =>
                if (rooms.get(roomId).isEmpty) (rooms + (roomId -> room), Some(()))
                else (rooms, Option.empty[Unit])
              }
            _ <- F.fromOption(updated, RoomAlreadyExists(roomId))
          } yield roomId

        def get(chatId: RoomId): F[Room[F]] =
          for {
            rooms <- ref.get
            room <- F.fromOption(rooms.get(chatId), NoRoomFoundForChatId)
          } yield room
      }
    }
}

