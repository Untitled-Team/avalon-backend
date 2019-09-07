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
  implicit val encoder: Encoder[PlayerInfo] = Encoder.instance { info =>
    Json.obj("character" := info.character, "badGuys" := info.badGuys.map(_.map(_.nickname)))
  }
}

case class TeamAssignmentPhase(missionNumber: Int, missionLeader: Nickname, missions: Missions) extends OutgoingEvent
object TeamAssignmentPhase {
  implicit val encoder: Encoder[TeamAssignmentPhase] = deriveEncoder
}

case class TeamAssignmentOutgoing(players: List[Nickname]) extends OutgoingEvent
object TeamAssignmentOutgoing {
  implicit val encoder: Encoder[TeamAssignmentOutgoing] = deriveEncoder
}

object OutgoingEventEncoder {
  implicit val encoder: Encoder[OutgoingEvent] = Encoder.instance {
    case g@MoveToLobby(_, _)            => MoveToLobby.encoder.apply(g).deepMerge(Json.obj("action" := "MoveToLobby"))
    case j@ChangeInLobby(_)             => ChangeInLobby.encoder.apply(j).deepMerge(Json.obj("action" := "ChangeInLobby"))
    case p@PlayerInfo(_, _)             => PlayerInfo.encoder.apply(p).deepMerge(Json.obj("action" := "PlayerInfo"))
    case g@TeamAssignmentPhase(_, _, _) => TeamAssignmentPhase.encoder.apply(g).deepMerge(Json.obj("action" := "TeamAssignmentPhase"))
    case g@TeamAssignmentOutgoing(_)    => TeamAssignmentOutgoing.encoder.apply(g).deepMerge(Json.obj("action" := "TeamAssignment"))
  }
}