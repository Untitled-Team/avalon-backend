package com.avalon.avalongame
package events

import cats.implicits._
import cats.effect.{ContextShift, IO}
import com.avalon.avalongame.common._
import com.mrdziuban.ScalacheckMagnolia._
import io.circe._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.implicits._
import org.scalatest.{FunSuite, Matchers, Status => _}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class IncomingEventSpec extends FunSuite with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {
  implicit val nicknameEncoder: Encoder[Nickname] = deriveUnwrappedEncoder

  test("make sure we can decode CreateGame event") {
    forAll { createGame: CreateGame =>
      val json = createGameJson(createGame)

      Right(createGame) should be(IncomingEventDecoder.decoder.decodeJson(json))
    }
  }

  test("make sure we can decode JoinGame event") {
    forAll { joinGame: JoinGame =>
      val json = joinGameJson(joinGame)

      Right(joinGame) should be(IncomingEventDecoder.decoder.decodeJson(json))
    }
  }

  test("make sure we can decode StartGame event") {
    Right(StartGame) should be(IncomingEventDecoder.decoder.decodeJson(startGameJson))
  }

  test("make sure we can decode MissionLeaderProposal event") {
    forAll { missionLeaderProposal: MissionLeaderProposal =>
      val json = missionLeaderProposalJson(missionLeaderProposal)

      Right(missionLeaderProposal) should be(IncomingEventDecoder.decoder.decodeJson(json))
    }
  }

  test("make sure we can decode MissionProposalVote event") {
    forAll { missionProposalVote: MissionProposalVote =>
      val json = missionProposalVoteJson(missionProposalVote)

      Right(missionProposalVote) should be(IncomingEventDecoder.decoder.decodeJson(json))
    }
  }

  val actionKey = "action"

  def createGameJson(createGame: CreateGame): Json =
    Json.obj(
      actionKey := "CreateGame",
      "nickname" := createGame.nickname,
      "config" := Json.obj(
        "merlin" := createGame.config.merlin,
        "assassin" := createGame.config.assassin
      )
    )

  def joinGameJson(joinGame: JoinGame): Json =
    Json.obj(
      actionKey := "JoinGame",
      "nickname" := joinGame.nickname,
      "roomId" := joinGame.roomId.value
    )

  val startGameJson: Json =
    Json.obj(
      actionKey := "StartGame",
    )

  def missionLeaderProposalJson(missionLeaderProposal: MissionLeaderProposal): Json =
    Json.obj(
      actionKey := "MissionLeaderProposal",
      "players" := missionLeaderProposal.players
    )

  def missionProposalVoteJson(missionProposalVote: MissionProposalVote): Json =
    Json.obj(
      actionKey := "MissionProposalVote",
      "nickname" := missionProposalVote.nickname,
      "vote" := missionProposalVote.vote.value
    )
}
