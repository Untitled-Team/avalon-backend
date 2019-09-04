package com.avalon.avalongame.events

import com.avalon.avalongame.common._
import com.avalon.avalongame.room._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.generic.extras.semiauto.deriveUnwrappedDecoder
import io.circe.syntax._

object EventEncoders {
  implicit val gameStateEncoder: Encoder[GameState] = Encoder.instance {
    case Lobby => Json.obj("state" := "Lobby")
    case MissionProposing(mn, ml) => Json.obj("state" := "MissionProposing", "currentMission" := mn, "missionLeader" := ml)
    case MissionVoting(mn, ml, users, v) =>
      Json.obj(
        "state" := "MissionVoting",
        "currentMission" := mn,
        "missionLeader" := ml,
        "users" := users,
        "votes" := v)
    case MissionProposed(voters) => Json.obj("state" := "MissionProposing", "voters" := voters)
  }

  implicit val missionEncoder: Encoder[Mission] = Encoder.instance { m =>
    Json.obj("players" := m.players, "numberOfAdventurers" := m.numberOfAdventurers)
  }

  implicit val missionsEncoder: Encoder[Missions] = Encoder.instance { m =>
    Json.obj(
      "one" := m.one,
      "two" := m.two,
      "three" := m.three,
      "four" := m.four,
      "five" := m.five)
  }

  implicit val characterEncoder: Encoder[Role] = Encoder.encodeString.contramap {
    case Assassin => "Assassin"
    case NormalBadGuy => "NormalBadGuy"
    case Merlin => "Merlin"
    case NormalGoodGuy => "NormalGoodGuy"
  }

  implicit val badPlayerRoleEncoder: Encoder[BadPlayerRole] = Nickname.encoder.contramap(_.nickname)

  implicit val characterRoleEncoder: Encoder[CharacterRole] = Encoder.instance { m =>
    Json.obj(
      "character" := m.character,
      "badGuys" := m.badGuys)
  }
}
