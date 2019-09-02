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
import org.scalatest.{FunSuite, Matchers, Status => _}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class OutgoingEventSpec extends FunSuite with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {

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

  def gameCreatedJson(gameCreated: GameCreated): Json =
    Json.obj(
      "event" := "GameCreated",
      "roomId" := gameCreated.roomId.value
    )

  def userJoinedJson(userJoined: UserJoined): Json =
    Json.obj(
      "event" := "UserJoined",
      "nickname" := userJoined.nickname.value)

  def joinedRoomJson(joinedRoom: JoinedRoom): Json =
    Json.obj(
      "event" := "JoinedRoom",
      "room" := Json.obj(
        "users" := Json.fromValues(joinedRoom.room.users.map(u => Json.obj("nickname" := u.nickname))),
        "config" := Json.obj(
          "merlin" := joinedRoom.room.config.merlin,
          "assassin" := joinedRoom.room.config.assassin,
        )
      )
    )
}
