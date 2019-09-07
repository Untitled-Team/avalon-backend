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

case class TeamAssignment(players: List[Nickname]) extends IncomingEvent
object TeamAssignment {
  implicit val decoder: Decoder[TeamAssignment] = deriveDecoder
}

case class TeamAssignmentVote(nickname: Nickname, vote: TeamVote) extends IncomingEvent
object TeamAssignmentVote {
  implicit val encoder: Encoder[TeamAssignmentVote] = deriveEncoder
  implicit val decoder: Decoder[TeamAssignmentVote] = deriveDecoder
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
        case "TeamAssignment" => TeamAssignment.decoder.decodeJson(hcursor.value)
        case "TeamAssignmentVote" => TeamAssignmentVote.decoder.decodeJson(hcursor.value)
      }
    } yield decoded
  }
}