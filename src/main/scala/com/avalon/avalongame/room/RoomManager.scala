package com.avalon.avalongame.room

import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._
import com.avalon.avalongame.RandomAlg
import com.avalon.avalongame.common._

trait RoomManager[F[_]] {
  def create(roomId: RoomId): F[Unit]
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
        def create(roomId: RoomId): F[Unit] = Room.build(randomAlg, roomId).flatMap { room =>
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

