package com.avalon.avalongame
package common

import cats.data.NonEmptyList
import cats.{Eq, Show}
import io.circe.Decoder
import io.circe._
import io.circe.generic.semiauto._
import io.circe.Encoder
import io.circe._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.syntax._
import io.circe.generic.extras.semiauto.deriveUnwrappedDecoder
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder

import scala.util.control.NoStackTrace

sealed abstract case class RoomId(value: String)

object RoomId {
  def create(value: String): RoomId = new RoomId(value.toLowerCase){}

  implicit val eq: Eq[RoomId] = Eq.fromUniversalEquals
  implicit val show: Show[RoomId] = Show.show(_.value)
  implicit val decoder: Decoder[RoomId] = Decoder.decodeString.map(create)
  implicit val encoder: Encoder[RoomId] = Encoder.encodeString.contramap(_.value)
}

object RoomIdVar {
  def unapply(s: String): Option[RoomId] =
    Some(RoomId.create(s))
}

case class Nickname(value: String)

object Nickname {
  implicit val eq: Eq[Nickname] = Eq.fromUniversalEquals
  implicit val show: Show[Nickname] = Show.show(_.value)
  implicit val decoder: Decoder[Nickname] = deriveUnwrappedDecoder
  implicit val encoder: Encoder[Nickname] = deriveUnwrappedEncoder
}

final case class User(nickname: Nickname)

object User {
  implicit val encoder: Encoder[User] = deriveEncoder
}

case class GameConfig(merlin: Boolean, assassin: Boolean)

object GameConfig {
  implicit val decoder: Decoder[GameConfig] = deriveDecoder
  implicit val encoder: Encoder[GameConfig] = deriveEncoder
}

case class RoomInfo(players: List[Nickname], config: GameConfig)

object RoomInfo {
  implicit val encoder: Encoder[RoomInfo] = deriveEncoder
}

case object NoRoomFoundForChatId extends RuntimeException with NoStackTrace

case class TeamVote(value: Boolean) extends AnyVal
object TeamVote {
  implicit val decoder: Decoder[TeamVote] = deriveUnwrappedDecoder
  implicit val encoder: Encoder[TeamVote] = deriveUnwrappedEncoder
  implicit val eq: Eq[TeamVote] = Eq.fromUniversalEquals
}

case class QuestVote(value: Boolean) extends AnyVal
object QuestVote {
  implicit val decoder: Decoder[QuestVote] = deriveUnwrappedDecoder
  implicit val encoder: Encoder[QuestVote] = deriveUnwrappedEncoder
  implicit val eq: Eq[QuestVote] = Eq.fromUniversalEquals
}