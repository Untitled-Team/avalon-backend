package com.avalon.avalongame
package events

import com.avalon.avalongame.Arbitraries._
import cats.implicits._
import cats.effect.{ContextShift, IO}
import com.avalon.avalongame.common._
import com.mrdziuban.ScalacheckMagnolia._
import io.chrisdavenport.fuuid.FUUID
import io.circe._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.syntax._
import io.chrisdavenport.fuuid.circe._
import org.http4s.circe._
import org.http4s.implicits._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FunSuite, Matchers, Status => _}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class IncomingEventSpec extends FunSuite with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {
  implicit val nicknameEncoder: Encoder[Nickname] = deriveUnwrappedEncoder

  implicit val fuuidArb: Arbitrary[FUUID] = Arbitrary(Gen.const(FUUID.randomFUUID[IO].unsafeRunSync()))

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
    forAll { startGame: StartGame =>
      val json = startGameJson(startGame)

      Right(startGame) should be(IncomingEventDecoder.decoder.decodeJson(json))
    }
  }

  test("make sure we can decode LeaveGame event") {
    Right(LeaveGame) should be(IncomingEventDecoder.decoder.decodeJson(leaveGameJson))
  }

  test("make sure we can decode Reconnect event") {
    forAll { reconnect: Reconnect =>
      val json = reconnectJson(reconnect)

      Right(reconnect) should be(IncomingEventDecoder.decoder.decodeJson(json))
    }
  }

  test("make sure we can decode ProposeParty event") {
    forAll { teamAssignment: ProposeParty =>
      val json = missionLeaderProposalJson(teamAssignment)

      Right(teamAssignment) should be(IncomingEventDecoder.decoder.decodeJson(json))
    }
  }

  test("make sure we can decode PartyApprovalVote event") {
    forAll { missionProposalVote: PartyApprovalVote =>
      val json = missionProposalVoteJson(missionProposalVote)

      Right(missionProposalVote) should be(IncomingEventDecoder.decoder.decodeJson(json))
    }
  }

  test("make sure we can decode QuestVote event") {
    forAll { questVote: QuestVoteEvent =>
      val json = questVoteJson(questVote)

      Right(questVote) should be(IncomingEventDecoder.decoder.decodeJson(json))
    }
  }

  test("make sure we can decode IncomingAssassinVote event") {
    forAll { incomingAssassinVote: IncomingAssassinVote =>
      val json = incomingAssassinVoteJson(incomingAssassinVote)

      Right(incomingAssassinVote) should be(IncomingEventDecoder.decoder.decodeJson(json))
    }
  }

  val actionKey = "event"

  def createGameJson(createGame: CreateGame): Json =
    Json.obj(
      actionKey := "CreateGame",
      "nickname" := createGame.nickname,
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

  val leaveGameJson: Json =
    Json.obj(
      actionKey := "LeaveGame",
    )

  def startGameJson(startGame: StartGame): Json =
    Json.obj(
      actionKey := "StartGame",
      "config" := startGame.config.map { config =>
        Json.obj(
          "percival" := config.percival,
          "morgana" := config.morgana,
          "mordred" := config.mordred,
          "oberon" := config.oberon,
        )
      }.getOrElse(Json.Null)
    )

  def reconnectJson(reconnect: Reconnect): Json =
    Json.obj(
      actionKey := "Reconnect",
      "nickname" := reconnect.nickname,
      "roomId" := reconnect.roomId.value,
      "lastMessageId" := reconnect.lastMessageId
    )

  def missionLeaderProposalJson(missionLeaderProposal: ProposeParty): Json =
    Json.obj(
      actionKey := "ProposeParty",
      "proposedParty" := missionLeaderProposal.proposedParty
    )

  def missionProposalVoteJson(missionProposalVote: PartyApprovalVote): Json =
    Json.obj(
      actionKey := "PartyApprovalVote",
      "partyPassVote" := missionProposalVote.partyPassVote.value
    )

  def questVoteJson(questVote: QuestVoteEvent): Json =
    Json.obj(
      actionKey := "QuestVote",
      "questPassVote" := questVote.questPassVote.value
    )

  def incomingAssassinVoteJson(incomingAssassinVote: IncomingAssassinVote): Json =
    Json.obj(
      actionKey := "AssassinVote",
      "guess" := incomingAssassinVote.guess.value
    )
}
