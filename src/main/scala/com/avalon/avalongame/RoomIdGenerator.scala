package com.avalon.avalongame

import cats.effect.Sync

trait RoomIdGenerator[F[_]] {
  def generate: F[RoomId]
}

object RoomIdGenerator {
  def build[F[_]: Sync]: RoomIdGenerator[F] = new RoomIdGenerator[F] {
    val r = scala.util.Random

    override def generate: F[RoomId] = Sync[F].delay(RoomId(r.nextString(6)))
  }
}