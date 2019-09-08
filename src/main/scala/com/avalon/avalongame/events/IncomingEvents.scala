package com.avalon.avalongame
package events

import com.avalon.avalongame.common._
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

case object StartGame extends IncomingEvent

case object PlayerReady extends IncomingEvent

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

object IncomingEventDecoder {
  implicit val decoder: Decoder[IncomingEvent] = Decoder.instance { hcursor =>
    for {
      eventName <- hcursor.downField("action").as[String]
      decoded <- eventName match {
        case "CreateGame" => CreateGame.decoder.decodeJson(hcursor.value)
        case "JoinGame"   => JoinGame.decoder.decodeJson(hcursor.value)
        case "StartGame"  => Right(StartGame)
        case "PlayerReady" => Right(PlayerReady)
        case "ProposeParty" => ProposeParty.decoder.decodeJson(hcursor.value)
        case "PartyApprovalVote" => PartyApprovalVote.decoder.decodeJson(hcursor.value)
        case "QuestVote" => QuestVoteEvent.decoder.decodeJson(hcursor.value)
      }
    } yield decoded
  }
}