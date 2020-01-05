package com.avalon.avalongame
package events

import com.avalon.avalongame.common._
import io.chrisdavenport.fuuid.circe._
import io.chrisdavenport.fuuid.FUUID
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.{Encoder, _}

sealed trait IncomingEvent
final case class CreateGame(nickname: Nickname) extends IncomingEvent

object CreateGame {
  implicit val decoder: Decoder[CreateGame] = deriveDecoder
}

final case class JoinGame(nickname: Nickname, roomId: RoomId) extends IncomingEvent

object JoinGame {
  implicit val decoder: Decoder[JoinGame] = deriveDecoder
}

case object LeaveGame extends IncomingEvent

case class Reconnect(nickname: Nickname, roomId: RoomId, lastMessageId: FUUID) extends IncomingEvent
object Reconnect {
  implicit val decoder: Decoder[Reconnect] = deriveDecoder
}

case class StartGame(config: Option[GameConfig]) extends IncomingEvent
object StartGame {
  implicit val decoder: Decoder[StartGame] = deriveDecoder
}

case class ProposeParty(proposedParty: List[Nickname]) extends IncomingEvent
object ProposeParty {
  implicit val decoder: Decoder[ProposeParty] = deriveDecoder
}

case class PartyApprovalVote(partyPassVote: TeamVote) extends IncomingEvent
object PartyApprovalVote {
  implicit val encoder: Encoder[PartyApprovalVote] = deriveEncoder
  implicit val decoder: Decoder[PartyApprovalVote] = deriveDecoder
}

case class QuestVoteEvent(questPassVote: QuestVote) extends IncomingEvent
object QuestVoteEvent {
  implicit val decoder: Decoder[QuestVoteEvent] = deriveDecoder
}

case class IncomingAssassinVote(guess: Nickname) extends IncomingEvent
object IncomingAssassinVote {
  implicit val decoder: Decoder[IncomingAssassinVote] = deriveDecoder
}

object IncomingEventDecoder {
  implicit val decoder: Decoder[IncomingEvent] = Decoder.instance { hcursor =>
    for {
      eventName <- hcursor.downField("event").as[String]
      decoded <- eventName match {
        case "CreateGame"           => CreateGame.decoder.decodeJson(hcursor.value)
        case "JoinGame"             => JoinGame.decoder.decodeJson(hcursor.value)
        case "LeaveGame"            => Right(LeaveGame)
        case "Reconnect"            => Reconnect.decoder.decodeJson(hcursor.value)
        case "StartGame"            => StartGame.decoder.decodeJson(hcursor.value)
        case "ProposeParty"         => ProposeParty.decoder.decodeJson(hcursor.value)
        case "PartyApprovalVote"    => PartyApprovalVote.decoder.decodeJson(hcursor.value)
        case "QuestVote"            => QuestVoteEvent.decoder.decodeJson(hcursor.value)
        case "AssassinVote"         => IncomingAssassinVote.decoder.decodeJson(hcursor.value)
        case e                      => Left(DecodingFailure(s"Invalid IncomingEvent event found: $e", hcursor.history))
      }
    } yield decoded
  }
}