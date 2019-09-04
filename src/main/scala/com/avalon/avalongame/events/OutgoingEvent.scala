package com.avalon.avalongame.events

import com.avalon.avalongame.common._
import com.avalon.avalongame.room.{GameState, Missions}
import EventEncoders._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.{Encoder, _}

sealed trait OutgoingEvent

case class MoveToLobby(roomId: RoomId, players: List[Nickname]) extends OutgoingEvent

object MoveToLobby {
  implicit val encoder: Encoder[MoveToLobby] = deriveEncoder
}

case class ChangeInLobby(players: List[Nickname]) extends OutgoingEvent

object ChangeInLobby {
  implicit val encoder: Encoder[ChangeInLobby] = deriveEncoder
}


//roomInfo?
//case class JoinedRoom(room: RoomInfo) extends OutgoingEvent

//object JoinedRoom {
//  implicit val encoder: Encoder[JoinedRoom] = deriveEncoder
//}

//case class UserJoined(nickname: Nickname) extends OutgoingEvent

//object UserJoined {
//  implicit val encoder: Encoder[UserJoined] = deriveEncoder
//}

case class GameStarted(state: GameState, missions: Missions, playerRole: CharacterRole, users: List[Nickname]) extends OutgoingEvent
object GameStarted {
  implicit val encoder: Encoder[GameStarted] = deriveEncoder
}

case class TeamAssignmentEvent(missionNumber: Int, missionLeader: Nickname, players: List[Nickname]) extends OutgoingEvent
object TeamAssignmentEvent {
  implicit val encoder: Encoder[TeamAssignmentEvent] = deriveEncoder
}

object OutgoingEventEncoder {
  implicit val encoder: Encoder[OutgoingEvent] = Encoder.instance {
    case g@MoveToLobby(_, _)            => MoveToLobby.encoder.apply(g).deepMerge(Json.obj("action" := "MoveToLobby"))
    case j@ChangeInLobby(_)             => ChangeInLobby.encoder.apply(j).deepMerge(Json.obj("action" := "ChangeInLobby"))
//    case u@UserJoined(_)                => UserJoined.encoder.apply(u).deepMerge(Json.obj("action" := "UserJoined"))
    case g@GameStarted(_, _, _, _)      => GameStarted.encoder.apply(g).deepMerge(Json.obj("action" := "GameStarted"))
    case g@TeamAssignmentEvent(_, _, _) => TeamAssignmentEvent.encoder.apply(g).deepMerge(Json.obj("action" := "TeamAssignment"))
  }
}