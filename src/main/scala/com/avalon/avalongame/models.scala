package com.avalon.avalongame

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

//==================
// Errors
//==================
case object NoRoomFoundForChatId extends RuntimeException with NoStackTrace
case class InvalidMissionNumber(n: Int) extends RuntimeException with NoStackTrace
case class NotEnoughPlayers(playerCount: Int) extends RuntimeException with NoStackTrace

//==================
// Game stuff
//==================
sealed trait GameState
case object Lobby extends GameState
case class MissionProposing(missionNumber: Int, missionLeader: User) extends GameState
case class MissionVote(missionNumber: Int, missionLeader: User, users: List[User]) extends GameState
case class MissionProposed(voters: NonEmptyList[User]) extends GameState

object GameState {
  implicit val encoder: Encoder[GameState] = Encoder.instance {
    case Lobby => Json.obj("state" := "Lobby")
    case MissionProposing(mn, ml) => Json.obj("state" := "MissionProposing", "currentMission" := mn, "missionLeader" := ml.nickname)
    case MissionVote(mn, ml, users) =>
      Json.obj(
        "state" := "MissionVote",
        "currentMission" := mn,
        "missionLeader" := ml.nickname,
        "users" := users)
    case MissionProposed(voters) => Json.obj("state" := "MissionProposing", "voters" := voters)
  }
}

case class MissionProposal(missionNumber: Int, missionLeader: Nickname, users: List[User])

sealed abstract case class Mission(players: Option[List[User]], numberOfAdventurers: Int)

object Mission {
  def make(players: Option[List[User]], numberOfAdventurers: Int): Mission =
    new Mission(players, numberOfAdventurers){}

  implicit val encoder: Encoder[Mission] = Encoder.instance { m =>
    Json.obj("players" := m.players, "numberOfAdventurers" := m.numberOfAdventurers)
  }
}

sealed abstract case class Missions(one: Mission,
                                    two: Mission,
                                    three: Mission,
                                    four: Mission,
                                    five: Mission)
object Missions {
  def fromPlayers(players: Int): Either[Throwable, Missions] = players match {
    case 5  =>
      Right(
        new Missions(
          Mission.make(None, 2),
          Mission.make(None, 3),
          Mission.make(None, 2),
          Mission.make(None, 3),
          Mission.make(None, 3)){})
    case 6  =>
      Right(
        new Missions(
          Mission.make(None, 2),
          Mission.make(None, 3),
          Mission.make(None, 4),
          Mission.make(None, 3),
          Mission.make(None, 4)){})
    case 7  =>
      Right(
        new Missions(
          Mission.make(None, 2),
          Mission.make(None, 3),
          Mission.make(None, 3),
          Mission.make(None, 4),
          Mission.make(None, 4)){})
    case 8 | 9 | 10  =>
      Right(
        new Missions(
          Mission.make(None, 3),
          Mission.make(None, 4),
          Mission.make(None, 4),
          Mission.make(None, 5),
          Mission.make(None, 5)){})

    case _ => Left(NotEnoughPlayers(players))
  }

  def fromInt(missions: Missions, n: Int): Either[Throwable, Mission] =
    n match {
      case 1 => Right(missions.one)
      case 2 => Right(missions.two)
      case 3 => Right(missions.three)
      case 4 => Right(missions.four)
      case 5 => Right(missions.five)
      case _ => Left(InvalidMissionNumber(n))
    }

  implicit val encoder: Encoder[Missions] = Encoder.instance { m =>
    Json.obj(
      "one" := m.one,
      "two" := m.two,
      "three" := m.three,
      "four" := m.four,
      "five" := m.five)
  }
}


sealed trait Role
object Role {
  implicit val encoder: Encoder[Role] = Encoder.encodeString.contramap {
    case Assassin => "Assassin"
    case NormalBadGuy => "NormalBadGuy"
    case Merlin => "Merlin"
    case NormalGoodGuy => "NormalGoodGuy"
  }
}

sealed trait BadGuy extends Role
case object Assassin extends BadGuy
case object NormalBadGuy extends BadGuy

sealed trait GoodGuy extends Role
case object Merlin extends GoodGuy
case object NormalGoodGuy extends GoodGuy

sealed trait PlayerRole {
  def role: Role
}
case class GoodPlayerRole(nickname: Nickname, role: GoodGuy) extends PlayerRole
case class BadPlayerRole(nickname: Nickname, role: BadGuy) extends PlayerRole

case class GameRepresentation(state: GameState,
                              missions: Missions,
                              badGuys: List[BadPlayerRole],
                              goodGuys: List[GoodPlayerRole],
                              users: List[User])