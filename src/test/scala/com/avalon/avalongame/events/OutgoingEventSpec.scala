package com.avalon.avalongame
package events

import cats.implicits._
import cats.effect.{ContextShift, IO}
import com.avalon.avalongame.common._
import com.avalon.avalongame.room._
import com.mrdziuban.ScalacheckMagnolia._
import EventEncoders._
import io.circe._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.implicits._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FunSuite, Matchers, Status => _}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class OutgoingEventSpec extends FunSuite with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {

  implicit val missionsArb: Arbitrary[Missions] = Arbitrary {
    Gen.chooseNum[Int](5, 10).map(n => IO.fromEither(Missions.fromPlayers(n)).unsafeRunSync())
  }

  implicit val playerRoleEventArb: Arbitrary[PlayerInfo] = Arbitrary {
    for {
      badGuys <- Gen.listOf[BadPlayerRole](Arbitrary.arbitrary[BadPlayerRole])
      role <- Arbitrary.arbitrary[Role]
      charRole = CharacterRole.fromRole(role, badGuys)
    } yield PlayerInfo(charRole.character, charRole.badGuys)
  }

  test("make sure we can encode GameCreated event") {
    forAll { gameCreated: MoveToLobby =>
      val json = gameCreatedJson(gameCreated)

      json should be(OutgoingEventEncoder.encoder(gameCreated))
    }
  }

  test("make sure we can encode ChangeInLobby event") {
    forAll { changeInLobby: ChangeInLobby =>
      val json = changeInLobbyJson(changeInLobby)

      json should be(OutgoingEventEncoder.encoder(changeInLobby))
    }
  }

  test("make sure we can encode PlayerInfoEvent event") {
    forAll { playerInfoEvent: PlayerInfo =>
      val json = playerRoleEventJson(playerInfoEvent)

      json should be(OutgoingEventEncoder.encoder(playerInfoEvent))
    }
  }

  test("make sure we can encode TeamAssignmentPhase event") {
    forAll { teamAssignmentEvent: TeamAssignmentPhase =>
      val json = teamAssignmentEventJson(teamAssignmentEvent)

      json should be(OutgoingEventEncoder.encoder(teamAssignmentEvent))
    }
  }

  test("make sure we can encode ProposedParty event") {
    forAll { proposedParty: ProposedParty =>
      val json = proposedPartyJson(proposedParty)

      json should be(OutgoingEventEncoder.encoder(proposedParty))
    }
  }

  test("make sure we can encode PartyApproved event") {
    partyApprovedJson should be(OutgoingEventEncoder.encoder(PartyApproved))
  }

  test("make sure we can encode PassFailVoteResults event") {
    forAll { passFailVoteResults: PassFailVoteResults =>
      val json = passFailVoteResultsJson(passFailVoteResults)

      json should be(OutgoingEventEncoder.encoder(passFailVoteResults))
    }
  }

  test("make sure we can encode AssassinVoteOutgoingEvent event") {
    forAll { assassinVoteOutgoingEvent: AssassinVoteOutgoingEvent =>
      val json = assassinVoteJson(assassinVoteOutgoingEvent)

      json should be(OutgoingEventEncoder.encoder(assassinVoteOutgoingEvent))
    }
  }

  def gameCreatedJson(moveToLobby: MoveToLobby): Json =
    Json.obj(
      "action" := "MoveToLobby",
      "roomId" := moveToLobby.roomId.value,
      "players" := moveToLobby.players
    )

  def changeInLobbyJson(joinedRoom: ChangeInLobby): Json =
    Json.obj(
      "action" := "ChangeInLobby",
      "players" := joinedRoom.players
    )

  def playerRoleEventJson(playerRoleEvent: PlayerInfo): Json =
    Json.obj(
      "action" := "PlayerInfo",
      "character" := playerRoleEvent.character,
      "badGuys" := playerRoleEvent.badGuys.map(_.map(_.nickname))
    )

  def teamAssignmentEventJson(missionProposalEvent: TeamAssignmentPhase): Json =
    Json.obj(
      "action" := "TeamAssignmentPhase",
      "missionNumber" := missionProposalEvent.missionNumber,
      "missionLeader" := missionProposalEvent.missionLeader,
      "missions" := missionProposalEvent.missions
    )

  def proposedPartyJson(proposedParty: ProposedParty): Json =
    Json.obj(
      "action" := "ProposedParty",
      "players" := proposedParty.players
    )

  val partyApprovedJson: Json =
    Json.obj("action" := "PartyApproved")

  def passFailVoteResultsJson(passFailVoteResults: PassFailVoteResults): Json =
    Json.obj(
      "action" := "PassFailVoteResults",
      "passVotes" := passFailVoteResults.passVotes,
      "failVotes" := passFailVoteResults.failVotes
    )

  def assassinVoteJson(assassinVote: AssassinVoteOutgoingEvent): Json =
    Json.obj(
      "action" := "AssassinVote",
      "assassinVoteData" := Json.obj(
        "assassin" := assassinVote.assassin,
        "goodGuys" := assassinVote.goodGuys
      )
    )
}
