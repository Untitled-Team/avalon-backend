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

case class GameStarted(state: GameState, missions: Missions, playerRole: Role, users: List[User]) extends OutgoingEvent
object GameStarted {
  implicit val encoder: Encoder[GameStarted] = deriveEncoder
}

case class MissionProposalEvent(missionNumber: Int, missionLeader: Nickname, players: List[Nickname]) extends OutgoingEvent
object MissionProposalEvent {
  implicit val encoder: Encoder[MissionProposalEvent] = deriveEncoder
}

object OutgoingEventEncoder {
  implicit val encoder: Encoder[OutgoingEvent] = Encoder.instance {
    case g@GameCreated(_) => GameCreated.encoder.apply(g).deepMerge(Json.obj("action" := "GameCreated"))
    case j@JoinedRoom(_) => JoinedRoom.encoder.apply(j).deepMerge(Json.obj("action" := "JoinedRoom")) //maybe reusable as Lobby?
    case u@UserJoined(_) => UserJoined.encoder.apply(u).deepMerge(Json.obj("action" := "UserJoined"))
    case g@GameStarted(_, _, _, _) => GameStarted.encoder.apply(g).deepMerge(Json.obj("action" := "GameStarted"))
    case g@MissionProposalEvent(_, _, _) => MissionProposalEvent.encoder.apply(g).deepMerge(Json.obj("action" := "MissionProposal"))
  }
}