package com.avalon.avalongame.events

import cats.implicits._
import com.avalon.avalongame.common._
import com.avalon.avalongame.room._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.generic.extras.semiauto.deriveUnwrappedDecoder
import io.circe.syntax._

object EventEncoders {
  implicit val missionEncoder: Encoder[Mission] = Encoder.instance { m =>
    Json.obj(
      "players" := m.players,
      "numberOfAdventurers" := m.numberOfAdventurers,
      "pass" := m.pass,
      "votes" := m.votes.map { vote =>
        Json.obj(
          "missionLeader" := vote.missionLeader,
        "successVotes" := vote.votes.filter(_.vote === TeamVote(true)).map(_.nickname),
        "failVotes" := vote.votes.filter(_.vote === TeamVote(false)).map(_.nickname))
      })
  }

  implicit val missionsEncoder: Encoder[Missions] = Encoder.instance { m =>
    Json.fromValues(List(m.one.asJson, m.two.asJson, m.three.asJson, m.four.asJson, m.five.asJson))
  }

  implicit val characterEncoder: Encoder[Role] = Encoder.encodeString.contramap {
    case Assassin => "Assassin"
    case Oberon => "Oberon"
    case Morgana => "Morgana"
    case Mordred => "Mordred"
    case NormalBadGuy => "NormalBadGuy"
    case Merlin => "Merlin"
    case Percival => "Percival"
    case NormalGoodGuy => "NormalGoodGuy"
  }

  implicit val badPlayerRoleEncoder: Encoder[BadPlayerRole] = Nickname.encoder.contramap(_.nickname)

  implicit val sideEncoder: Encoder[Side] = Encoder.instance {
    case BadGuys => Encoder.encodeString.apply("BadGuys")
    case GoodGuys => Encoder.encodeString.apply("GoodGuys")
  }
}
