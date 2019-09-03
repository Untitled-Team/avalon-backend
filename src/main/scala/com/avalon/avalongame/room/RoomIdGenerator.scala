package com.avalon.avalongame.room

import cats.effect.Sync
import com.avalon.avalongame.common._

trait RoomIdGenerator[F[_]] {
  def generate: F[RoomId]
}

object RoomIdGenerator {
  def build[F[_]: Sync]: RoomIdGenerator[F] = new RoomIdGenerator[F] {
    val r = scala.util.Random

    override def generate: F[RoomId] = Sync[F].delay(RoomId(r.nextString(6)))
  }
}