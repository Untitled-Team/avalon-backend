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

case class ProposedParty(players: List[Nickname]) extends OutgoingEvent
object ProposedParty {
  implicit val encoder: Encoder[ProposedParty] = deriveEncoder
}

case object PartyApproved extends OutgoingEvent

case class PassFailVoteResults(passVotes: Int, failVotes: Int) extends OutgoingEvent
object PassFailVoteResults {
  implicit val encoder: Encoder[PassFailVoteResults] = deriveEncoder
}

case class AssassinVoteOutgoingEvent(assassin: Nickname, goodGuys: List[Nickname]) extends OutgoingEvent
object AssassinVoteOutgoingEvent {
  implicit val encoder: Encoder[AssassinVoteOutgoingEvent] = Encoder.instance { aVote =>
    Json.obj("assassinVoteData" := Json.obj("assassin" := aVote.assassin, "goodGuys" := aVote.goodGuys))
  }
}

case class GameOverOutgoingEvent(assassin: Nickname,
                                 assassinGuess: Option[Nickname],
                                 merlin: Nickname,
                                 goodGuys: List[GoodPlayerRole],
                                 badGuys: List[BadPlayerRole],
                                 winningTeam: Side) extends OutgoingEvent

object GameOverOutgoingEvent {
  implicit val encoder: Encoder[GameOverOutgoingEvent] = Encoder.instance { gameOver =>
    Json.obj("gameOverData" :=
      Json.obj(
        "assassin" := gameOver.assassin,
        "assassinGuess" :=  gameOver.assassinGuess,
        "merlin" :=  gameOver.merlin,
        "goodGuys" := gameOver.goodGuys.map(_.nickname),
        "badGuys" := gameOver.badGuys.map(_.nickname),
        "winningTeam" :=  gameOver.winningTeam
      )
    )
  }
}

object OutgoingEventEncoder {
  implicit val encoder: Encoder[OutgoingEvent] = Encoder.instance {
    case g@MoveToLobby(_, _)               => MoveToLobby.encoder.apply(g).deepMerge(Json.obj("action" := "MoveToLobby"))
    case j@ChangeInLobby(_)                => ChangeInLobby.encoder.apply(j).deepMerge(Json.obj("action" := "ChangeInLobby"))
    case p@PlayerInfo(_, _)                => PlayerInfo.encoder.apply(p).deepMerge(Json.obj("action" := "PlayerInfo"))
    case g@TeamAssignmentPhase(_, _, _)    => TeamAssignmentPhase.encoder.apply(g).deepMerge(Json.obj("action" := "TeamAssignmentPhase"))
    case g@ProposedParty(_)                => ProposedParty.encoder.apply(g).deepMerge(Json.obj("action" := "ProposedParty"))
    case PartyApproved                     => Json.obj("action" := "PartyApproved")
    case p@PassFailVoteResults(_, _)       => PassFailVoteResults.encoder.apply(p).deepMerge(Json.obj("action" := "PassFailVoteResults"))
    case a@AssassinVoteOutgoingEvent(_, _) => AssassinVoteOutgoingEvent.encoder.apply(a).deepMerge(Json.obj("action" := "AssassinVote"))
    case g@GameOverOutgoingEvent(_, _, _, _, _, _)      => GameOverOutgoingEvent.encoder.apply(g).deepMerge(Json.obj("action" := "GameOver"))
  }
}