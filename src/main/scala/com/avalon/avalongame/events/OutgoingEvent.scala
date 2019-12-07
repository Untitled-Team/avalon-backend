package com.avalon.avalongame.events

import com.avalon.avalongame.common._
import com.avalon.avalongame.room._
import EventEncoders._
import cats.effect.Sync
import cats.implicits._
import com.avalon.avalongame.RandomAlg
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.{Encoder, _}

sealed trait OutgoingEvent {
  def id: FUUID
}

case class MoveToLobby(roomId: RoomId, players: List[Nickname], id: FUUID) extends OutgoingEvent

object MoveToLobby {
  def make[F[_]: Sync](roomId: RoomId, players: List[Nickname])(implicit R: RandomAlg[F]): F[MoveToLobby] =
    R.fuuid.map(MoveToLobby(roomId, players, _))

  implicit val encoder: Encoder[MoveToLobby] = deriveEncoder
}

case class ChangeInLobby(players: List[Nickname], id: FUUID) extends OutgoingEvent

object ChangeInLobby {
  def make[F[_]: Sync](players: List[Nickname])(implicit R: RandomAlg[F]): F[ChangeInLobby] =
    R.fuuid.map(ChangeInLobby(players, _))

  implicit val encoder: Encoder[ChangeInLobby] = deriveEncoder
}

case class GameLeft(id: FUUID) extends OutgoingEvent

object GameLeft {
  def make[F[_]: Sync](implicit R: RandomAlg[F]): F[GameLeft] = R.fuuid.map(GameLeft(_))

  implicit val encoder: Encoder[GameLeft] = deriveEncoder
}

case class GameNoLongerExists(id: FUUID) extends OutgoingEvent

object GameNoLongerExists {
  def make[F[_]: Sync](implicit R: RandomAlg[F]): F[GameNoLongerExists] = R.fuuid.map(GameNoLongerExists(_))

  implicit val encoder: Encoder[GameNoLongerExists] = deriveEncoder
}

case class PlayerReadyAcknowledgement(id: FUUID) extends OutgoingEvent

object PlayerReadyAcknowledgement {
  def make[F[_]: Sync](implicit R: RandomAlg[F]): F[PlayerReadyAcknowledgement] = R.fuuid.map(PlayerReadyAcknowledgement(_))

  implicit val encoder: Encoder[PlayerReadyAcknowledgement] = deriveEncoder
}

case class PlayerInfo(character: Role, badGuys: Option[List[BadPlayerRole]], id: FUUID) extends OutgoingEvent

object PlayerInfo {
  def make[F[_]: Sync](character: Role, badGuys: Option[List[BadPlayerRole]])(implicit R: RandomAlg[F]): F[PlayerInfo] =
    R.fuuid.map(PlayerInfo(character, badGuys, _))

  implicit val encoder: Encoder[PlayerInfo] = Encoder.instance { info =>
    Json.obj("character" := info.character, "badGuys" := info.badGuys.map(_.map(_.nickname)), "id" := info.id)
  }
}

case class TeamAssignmentPhase(missionNumber: Int,
                               missionLeader: Nickname,
                               missions: Missions,
                               nextMissionLeader: Nickname,
                               proposalsLeft: Int,
                               id: FUUID) extends OutgoingEvent
object TeamAssignmentPhase {
  def make[F[_]: Sync](missionNumber: Int,
                       missionLeader: Nickname,
                       missions: Missions,
                       nextMissionLeader: Nickname,
                       proposalsLeft: Int
                      )(implicit R: RandomAlg[F]): F[TeamAssignmentPhase] =
    R.fuuid.map(TeamAssignmentPhase(missionNumber, missionLeader, missions, nextMissionLeader, proposalsLeft, _))

  implicit val encoder: Encoder[TeamAssignmentPhase] = deriveEncoder
}

case class ProposedParty(proposedParty: List[Nickname], id: FUUID) extends OutgoingEvent
object ProposedParty {
  def make[F[_]: Sync](proposedParty: List[Nickname])(implicit R: RandomAlg[F]): F[ProposedParty] =
    R.fuuid.map(ProposedParty(proposedParty, _))

  implicit val encoder: Encoder[ProposedParty] = deriveEncoder
}

case class PartyApprovalVoteAcknowledgement(id: FUUID) extends OutgoingEvent

object PartyApprovalVoteAcknowledgement {
  def make[F[_]: Sync](implicit R: RandomAlg[F]): F[PartyApprovalVoteAcknowledgement] = R.fuuid.map(PartyApprovalVoteAcknowledgement(_))

  implicit val encoder: Encoder[PartyApprovalVoteAcknowledgement] = deriveEncoder
}

case class PartyApproved(id: FUUID) extends OutgoingEvent

object PartyApproved {
  def make[F[_]: Sync](implicit R: RandomAlg[F]): F[PartyApproved] = R.fuuid.map(PartyApproved(_))

  implicit val encoder: Encoder[PartyApproved] = deriveEncoder
}

case class QuestVoteAcknowledgement(id: FUUID) extends OutgoingEvent

object QuestVoteAcknowledgement {
  def make[F[_]: Sync](implicit R: RandomAlg[F]): F[QuestVoteAcknowledgement] = R.fuuid.map(QuestVoteAcknowledgement(_))

  implicit val encoder: Encoder[QuestVoteAcknowledgement] = deriveEncoder
}

case class PassFailVoteResults(passVotes: Int, failVotes: Int, id: FUUID) extends OutgoingEvent
object PassFailVoteResults {
  def make[F[_]: Sync](passVotes: Int, failVotes: Int)(implicit R: RandomAlg[F]): F[PassFailVoteResults] =
    R.fuuid.map(PassFailVoteResults(passVotes, failVotes, _))

  implicit val encoder: Encoder[PassFailVoteResults] = deriveEncoder
}

case class QuestDisplayAcknowledgement(id: FUUID) extends OutgoingEvent

object QuestDisplayAcknowledgement {
  def make[F[_]: Sync](implicit R: RandomAlg[F]): F[QuestDisplayAcknowledgement] = R.fuuid.map(QuestDisplayAcknowledgement(_))
  implicit val encoder: Encoder[QuestDisplayAcknowledgement] = deriveEncoder
}

case class AssassinVoteOutgoingEvent(assassin: Nickname, goodGuys: List[Nickname], missions: Missions, id: FUUID) extends OutgoingEvent
object AssassinVoteOutgoingEvent {
  def make[F[_]: Sync](assassin: Nickname, goodGuys: List[Nickname], missions: Missions)(implicit R: RandomAlg[F]): F[AssassinVoteOutgoingEvent] =
    R.fuuid.map(AssassinVoteOutgoingEvent(assassin, goodGuys, missions, _))

  implicit val encoder: Encoder[AssassinVoteOutgoingEvent] = Encoder.instance { aVote =>
    Json.obj(
      "assassinVoteData" :=
        Json.obj(
          "assassin" := aVote.assassin,
          "goodGuys" := aVote.goodGuys,
          "id" := aVote.id),
      "missions" := aVote.missions)
  }
}

case class GameOverOutgoingEvent(assassin: Nickname,
                                 assassinGuess: Option[Nickname],
                                 merlin: Nickname,
                                 goodGuys: List[GoodPlayerRole],
                                 badGuys: List[BadPlayerRole],
                                 winningTeam: Side,
                                 id: FUUID) extends OutgoingEvent

object GameOverOutgoingEvent {
  def make[F[_]: Sync](assassin: Nickname,
                       assassinGuess: Option[Nickname],
                       merlin: Nickname,
                       goodGuys: List[GoodPlayerRole],
                       badGuys: List[BadPlayerRole],
                       winningTeam: Side)(implicit R: RandomAlg[F]): F[GameOverOutgoingEvent] =
    R.fuuid.map(GameOverOutgoingEvent(assassin, assassinGuess, merlin, goodGuys, badGuys, winningTeam, _))


  implicit val encoder: Encoder[GameOverOutgoingEvent] = Encoder.instance { gameOver =>
    Json.obj("gameOverData" :=
      Json.obj(
        "assassin" := gameOver.assassin,
        "assassinGuess" :=  gameOver.assassinGuess,
        "merlin" :=  gameOver.merlin,
        "goodGuys" := gameOver.goodGuys.map(_.nickname),
        "badGuys" := gameOver.badGuys.map(_.nickname),
        "winningTeam" :=  gameOver.winningTeam,
        "id" := gameOver.id
      )
    )
  }
}

object OutgoingEventEncoder {
  implicit val encoder: Encoder[OutgoingEvent] = Encoder.instance {
    case g@MoveToLobby(_, _, _)                  => MoveToLobby.encoder.apply(g).deepMerge(Json.obj("event" := "MoveToLobby"))
    case j@ChangeInLobby(_, _)                   => ChangeInLobby.encoder.apply(j).deepMerge(Json.obj("event" := "ChangeInLobby"))
    case g@GameLeft(_)                           => GameLeft.encoder.apply(g).deepMerge(Json.obj("event" := "GameLeft"))
    case g@GameNoLongerExists(_)                 => GameNoLongerExists.encoder.apply(g).deepMerge(Json.obj("event" := "GameNoLongerExists"))
    case p@PlayerReadyAcknowledgement(_)         => PlayerReadyAcknowledgement.encoder.apply(p).deepMerge(Json.obj("event" := "PlayerReadyAcknowledgement"))
    case p@PlayerInfo(_, _, _)                   => PlayerInfo.encoder.apply(p).deepMerge(Json.obj("event" := "PlayerInfo"))
    case g@TeamAssignmentPhase(_, _, _, _, _, _) => TeamAssignmentPhase.encoder.apply(g).deepMerge(Json.obj("event" := "TeamAssignmentPhase"))
    case g@ProposedParty(_, _)                   => ProposedParty.encoder.apply(g).deepMerge(Json.obj("event" := "ProposedParty"))
    case p@PartyApprovalVoteAcknowledgement(_)   => PartyApprovalVoteAcknowledgement.encoder.apply(p).deepMerge(Json.obj("event" := "PartyApprovalVoteAcknowledgement"))
    case p@PartyApproved(_)                      => PartyApproved.encoder.apply(p).deepMerge(Json.obj("event" := "PartyApproved"))
    case q@QuestVoteAcknowledgement(_)           => QuestVoteAcknowledgement.encoder.apply(q).deepMerge(Json.obj("event" := "QuestVoteAcknowledgement"))
    case p@PassFailVoteResults(_, _, _)          => PassFailVoteResults.encoder.apply(p).deepMerge(Json.obj("event" := "PassFailVoteResults"))
    case q@QuestDisplayAcknowledgement(_)        => QuestDisplayAcknowledgement.encoder.apply(q).deepMerge(Json.obj("event" := "QuestDisplayAcknowledgement"))
    case a@AssassinVoteOutgoingEvent(_, _, _, _) => AssassinVoteOutgoingEvent.encoder.apply(a).deepMerge(Json.obj("event" := "AssassinVote"))
    case g@GameOverOutgoingEvent(_, _, _, _, _, _, _) => GameOverOutgoingEvent.encoder.apply(g).deepMerge(Json.obj("event" := "GameOver"))
  }
}