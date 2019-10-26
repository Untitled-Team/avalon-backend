package com.avalon.avalongame.events

import cats.effect.IO
import com.avalon.avalongame.common._
import fs2.concurrent.Queue
import io.chrisdavenport.fuuid.FUUID

class MockOutgoingManager extends OutgoingManager[IO] {
  override def add(nickname: Nickname, respond: Queue[IO, OutgoingEvent]): IO[Unit] = ???
  override def remove(nickname: Nickname): IO[Unit] = ???
  override def disconnected(nickname: Nickname): IO[Unit] = ???
  override def reconnect(nickname: Nickname, id: FUUID, respond: Queue[IO, OutgoingEvent]): IO[Unit] = ???
  override def send(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = ???
  override def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): IO[Unit] = ???
  override def broadcastUserSpecific(nickname: Nickname, outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = ???
  override def sendToAll(event: OutgoingEvent): IO[Unit] = ???
  override def sendToAllUserSpecific(outgoingF: Nickname => IO[OutgoingEvent]): IO[Unit] = ???
}
