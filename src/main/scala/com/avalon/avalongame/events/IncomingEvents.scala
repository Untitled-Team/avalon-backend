package com.avalon.avalongame
package events

import com.avalon.avalongame.RoomId
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

final case class StartGame(roomId: RoomId) extends IncomingEvent
object StartGame {
  implicit val decoder: Decoder[StartGame] = deriveDecoder
}

object IncomingEventDecoder {
  implicit val decoder: Decoder[IncomingEvent] = Decoder.instance { hcursor =>
    for {
      eventName <- hcursor.downField("event").as[String]
      decoded <- eventName match {
        case "CreateGame" => CreateGame.decoder.decodeJson(hcursor.value)
        case "JoinGame"   => JoinGame.decoder.decodeJson(hcursor.value)
        case "StartGame"  => StartGame.decoder.decodeJson(hcursor.value)
      }
    } yield decoded
  }
}
