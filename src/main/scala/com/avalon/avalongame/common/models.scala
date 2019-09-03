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

final case class RoomId(value: String) extends AnyVal

object RoomId {
  implicit val eq: Eq[RoomId] = Eq.fromUniversalEquals
  implicit val show: Show[RoomId] = Show.show(_.value)
  implicit val decoder: Decoder[RoomId] = deriveUnwrappedDecoder
  implicit val encoder: Encoder[RoomId] = deriveUnwrappedEncoder
}

object RoomIdVar {
  def unapply(s: String): Option[RoomId] =
    Some(RoomId(s))
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

case class RoomInfo(users: List[User], config: GameConfig)

object RoomInfo {
  implicit val encoder: Encoder[RoomInfo] = deriveEncoder
}

case object NoRoomFoundForChatId extends RuntimeException with NoStackTrace