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

class EventEncodersSpec extends FunSuite with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {

  implicit val missionsArb: Arbitrary[Missions] = Arbitrary {
    Gen.chooseNum[Int](5, 10).map(n => IO.fromEither(Missions.fromPlayers(n)).unsafeRunSync())
  }

  test("make sure we can encode Missions correctly") {
    forAll { missions: Missions =>
      val json = missionsJson(missions)

      json should be(EventEncoders.missionsEncoder(missions))
    }
  }

  def missionsJson(missions: Missions): Json =
    Json.fromValues(
      List(missions.one, missions.two, missions.three, missions.four, missions.five).map { mission =>
        Json.obj(
          "players" := mission.players.map(_.map(_.value)),
          "numberOfAdventurers" := mission.numberOfAdventurers,
          "pass" := mission.pass.map(_.value),
          "votes" := mission.votes.map { vote =>
            Json.obj(
              "missionLeader" := vote.missionLeader,
              "success" := vote.votes.filter(_.vote == TeamVote(true)).map(_.nickname.value),
              "fail" := vote.votes.filter(_.vote == TeamVote(false)).map(_.nickname.value))
          }
        )
      }
    )
}