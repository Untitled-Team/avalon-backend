package com.avalon.avalongame
package events

import com.avalon.avalongame.common._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.{Encoder, _}

sealed trait IncomingEvent
final case class CreateGame(nickname: Nickname, config: GameConfig) extends IncomingEvent

object CreateGame {
  implicit val decoder: Decoder[CreateGame] = deriveDecoder
}

final case class JoinGame(nickname: Nickname, roomId: RoomId) extends IncomingEvent

object JoinGame {
  implicit val decoder: Decoder[JoinGame] = deriveDecoder
}

case object StartGame extends IncomingEvent

case class MissionLeaderProposal(players: List[Nickname]) extends IncomingEvent
object MissionLeaderProposal {
  implicit val decoder: Decoder[MissionLeaderProposal] = deriveDecoder
}

case class MissionProposalVote(nickname: Nickname, vote: Vote) extends IncomingEvent
object MissionProposalVote {
  implicit val encoder: Encoder[MissionProposalVote] = deriveEncoder
  implicit val decoder: Decoder[MissionProposalVote] = deriveDecoder
}

object IncomingEventDecoder {
  implicit val decoder: Decoder[IncomingEvent] = Decoder.instance { hcursor =>
    for {
      eventName <- hcursor.downField("action").as[String]
      decoded <- eventName match {
        case "CreateGame" => CreateGame.decoder.decodeJson(hcursor.value)
        case "JoinGame"   => JoinGame.decoder.decodeJson(hcursor.value)
        case "StartGame"  => Right(StartGame)
        case "MissionLeaderProposal" => MissionLeaderProposal.decoder.decodeJson(hcursor.value)
        case "MissionProposalVote" => MissionProposalVote.decoder.decodeJson(hcursor.value)
      }
    } yield decoded
  }
}