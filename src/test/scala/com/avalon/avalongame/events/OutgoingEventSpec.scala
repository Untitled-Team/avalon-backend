package com.avalon.avalongame
package events

import cats.implicits._
import cats.effect.{ContextShift, IO}
import com.mrdziuban.ScalacheckMagnolia._
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

  test("make sure we can encode GameCreated event") {
    forAll { gameCreated: GameCreated =>
      val json = gameCreatedJson(gameCreated)

      json should be(OutgoingEventEncoder.encoder(gameCreated))
    }
  }

  test("make sure we can encode UserJoined event") {
    forAll { userJoined: UserJoined =>
      userJoinedJson(userJoined) should be(OutgoingEventEncoder.encoder(userJoined))
    }
  }

  test("make sure we can encode JoinedRoom event") {
    forAll { joinedRoom: JoinedRoom =>
      val json = joinedRoomJson(joinedRoom)

      json should be(OutgoingEventEncoder.encoder(joinedRoom))
    }
  }

  test("make sure we can encode GameStarted event") {
    forAll { gameStarted: GameStarted =>
      val json = gameStartedJson(gameStarted)

      json should be(OutgoingEventEncoder.encoder(gameStarted))
    }
  }

  def gameCreatedJson(gameCreated: GameCreated): Json =
    Json.obj(
      "action" := "GameCreated",
      "roomId" := gameCreated.roomId.value
    )

  def userJoinedJson(userJoined: UserJoined): Json =
    Json.obj(
      "action" := "UserJoined",
      "nickname" := userJoined.nickname.value)

  def joinedRoomJson(joinedRoom: JoinedRoom): Json =
    Json.obj(
      "action" := "JoinedRoom",
      "room" := Json.obj(
        "users" := Json.fromValues(joinedRoom.room.users.map(u => Json.obj("nickname" := u.nickname))),
        "config" := Json.obj(
          "merlin" := joinedRoom.room.config.merlin,
          "assassin" := joinedRoom.room.config.assassin,
        )
      )
    )

  def gameStartedJson(gameStarted: GameStarted): Json =
    Json.obj(
      "action" := "GameStarted",
      "state" := gameStarted.state, //need to test this separately
      "missions" := gameStarted.missions,
      "playerRole" := gameStarted.playerRole,
      "users" := gameStarted.users
    )
}
