package com.avalon.avalongame.events

import com.avalon.avalongame.common._
import com.avalon.avalongame.room._
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

case class PlayerInfo(character: Role, badGuys: Option[List[BadPlayerRole]]) extends OutgoingEvent

object PlayerInfo {
  implicit val encoder: Encoder[PlayerInfo] = deriveEncoder
}

//roomInfo?
//case class JoinedRoom(room: RoomInfo) extends OutgoingEvent

//object JoinedRoom {
//  implicit val encoder: Encoder[JoinedRoom] = deriveEncoder
//}

//case class GameStarted(state: GameState, missions: Missions, playerRole: CharacterRole, users: List[Nickname]) extends OutgoingEvent
//object GameStarted {
//  implicit val encoder: Encoder[GameStarted] = deriveEncoder
//}

case class TeamAssignmentPhase(missionNumber: Int, missionLeader: Nickname, missions: Missions) extends OutgoingEvent
object TeamAssignmentPhase {
  implicit val encoder: Encoder[TeamAssignmentPhase] = deriveEncoder
}

object OutgoingEventEncoder {
  implicit val encoder: Encoder[OutgoingEvent] = Encoder.instance {
    case g@MoveToLobby(_, _)            => MoveToLobby.encoder.apply(g).deepMerge(Json.obj("action" := "MoveToLobby"))
    case j@ChangeInLobby(_)             => ChangeInLobby.encoder.apply(j).deepMerge(Json.obj("action" := "ChangeInLobby"))
    case p@PlayerInfo(_, _)             => PlayerInfo.encoder.apply(p).deepMerge(Json.obj("action" := "PlayerInfo"))
//    case u@UserJoined(_)                => UserJoined.encoder.apply(u).deepMerge(Json.obj("action" := "UserJoined"))
//    case g@GameStarted(_, _, _, _)      => GameStarted.encoder.apply(g).deepMerge(Json.obj("action" := "GameStarted"))
    case g@TeamAssignmentPhase(_, _, _) => TeamAssignmentPhase.encoder.apply(g).deepMerge(Json.obj("action" := "TeamAssignmentPhase"))
  }
}