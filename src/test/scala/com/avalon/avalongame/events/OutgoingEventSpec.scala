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

  implicit val characterRole: Arbitrary[CharacterRole] = Arbitrary {
    for {
      badGuys <- Gen.listOf[Nickname](Arbitrary.arbitrary[Nickname])
      role <- Arbitrary.arbitrary[Role]
    } yield CharacterRole.fromRole(role, badGuys)
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

//  test("make sure we can encode GameStarted event") {
//    forAll { gameStarted: GameStarted =>
//      val json = gameStartedJson(gameStarted)
//
//      json should be(OutgoingEventEncoder.encoder(gameStarted))
//    }
//  }

  test("make sure we can encode TeamAssignmentEvent event") {
    forAll { teamAssignmentEvent: TeamAssignmentEvent =>
      val json = teamAssignmentEventJson(teamAssignmentEvent)

      json should be(OutgoingEventEncoder.encoder(teamAssignmentEvent))
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

//  def gameStartedJson(gameStarted: GameStarted): Json =
//    Json.obj(
//      "action" := "GameStarted",
//      "state" := gameStarted.state, //need to test this separately
//      "missions" := gameStarted.missions,
//      "playerRole" := Json.obj(
//        "character" := gameStarted.playerRole.character,
//        "badGuys" := gameStarted.playerRole.badGuys
//      ),
//      "users" := gameStarted.users
//    )

  def teamAssignmentEventJson(missionProposalEvent: TeamAssignmentEvent): Json =
    Json.obj(
      "action" := "TeamAssignment",
      "missionNumber" := missionProposalEvent.missionNumber,
      "missionLeader" := missionProposalEvent.missionLeader,
      "players" := missionProposalEvent.players
    )
}
