package com.avalon.avalongame
package events

import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.{Encoder, _}

sealed trait OutgoingEvent

case class GameCreated(roomId: RoomId) extends OutgoingEvent

object GameCreated {
  implicit val encoder: Encoder[GameCreated] = deriveEncoder
}

case class JoinedRoom(room: RoomInfo) extends OutgoingEvent

object JoinedRoom {
  implicit val encoder: Encoder[JoinedRoom] = deriveEncoder
}

case class UserJoined(nickname: Nickname) extends OutgoingEvent

object UserJoined {
  implicit val encoder: Encoder[UserJoined] = deriveEncoder
}


object OutgoingEventEncoder {
  implicit val encoder: Encoder[OutgoingEvent] = Encoder.instance {
    case g@GameCreated(_) => GameCreated.encoder.apply(g).deepMerge(Json.obj("event" := "GameCreated"))
    case j@JoinedRoom(_) => JoinedRoom.encoder.apply(j).deepMerge(Json.obj("event" := "JoinedRoom"))
    case u@UserJoined(_) => UserJoined.encoder.apply(u).deepMerge(Json.obj("event" := "UserJoined"))
  }
}