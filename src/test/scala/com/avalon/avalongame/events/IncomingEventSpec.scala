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
    forAll { startGame: StartGame =>
      val json = startGameJson(startGame)

      Right(startGame) should be(IncomingEventDecoder.decoder.decodeJson(json))
    }
  }

  val actionKey = "event"

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

  def startGameJson(startGame: StartGame): Json =
    Json.obj(
      actionKey := "StartGame",
      "roomId" := startGame.roomId.value
    )
}
