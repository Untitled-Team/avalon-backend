package com.avalon.avalongame
package events

import com.avalon.avalongame.Arbitraries._
import cats.implicits._
import cats.effect.{ContextShift, IO}
import com.avalon.avalongame.common._
import com.avalon.avalongame.room._
import com.mrdziuban.ScalacheckMagnolia._
import EventEncoders._
import cats.Eq
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.fuuid.circe._
import io.circe._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.implicits._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FunSuite, Matchers, Status => _}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class OutgoingEventSpec extends FunSuite with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {
  val constant = FUUID.randomFUUID[IO].unsafeRunSync()

  implicit val R: RandomAlg[IO] = new RandomAlg[IO] {
    def shuffle[A](l: List[A]): IO[List[A]] = ???
    def randomGet[A](l: List[A]): IO[A] = ???
    def clockwise[A: Eq](previous: A, l: List[A]): IO[A] = ???
    def fuuid: IO[FUUID] = IO.pure(constant)
  }

  implicit val fuuidArb: Arbitrary[FUUID] = Arbitrary(Gen.const(FUUID.randomFUUID[IO].unsafeRunSync()))

  implicit val missionsArb: Arbitrary[Missions] = Arbitrary {
    Gen.chooseNum[Int](5, 10).map(n => IO.fromEither(Missions.fromPlayers(n)).unsafeRunSync())
  }

  implicit val playerRoleEventArb: Arbitrary[PlayerInfo] = Arbitrary {
    for {
      badGuys <- Gen.listOf[BadPlayerRole](Arbitrary.arbitrary[BadPlayerRole])
      merlin <- Arbitrary.arbitrary[Nickname].map(GoodPlayerRole(_, Merlin))
      morgana <- Arbitrary.arbitrary[Nickname].map(BadPlayerRole(_, Morgana))
      role <- Arbitrary.arbitrary[Role]
      charRole = CharacterRole.fromRole(role, badGuys, merlin, Some(morgana))
    } yield PlayerInfo.make[IO](charRole.character, charRole.badGuys, Some(List(merlin, morgana))).unsafeRunSync()
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

  test("make sure we can encode GameLeft event") {
    gameLeftJson should be(OutgoingEventEncoder.encoder(GameLeft.make[IO].unsafeRunSync()))
  }

  test("make sure we can encode PlayerReadyAcknowledgement event") {
    val playerReadyAckJson: Json =
      Json.obj("event" := "PlayerReadyAcknowledgement", "id" := constant)

    playerReadyAckJson should be(OutgoingEventEncoder.encoder(PlayerReadyAcknowledgement.make[IO].unsafeRunSync()))
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

  test("make sure we can encode GameConfigEvent event") {
    forAll { gameConfigEvent: GameConfigEvent =>
      val json = gameConfigEventJson(gameConfigEvent)

      json should be(OutgoingEventEncoder.encoder(gameConfigEvent))
    }
  }

  test("make sure we can encode ProposedParty event") {
    forAll { proposedParty: ProposedParty =>
      val json = proposedPartyJson(proposedParty)

      json should be(OutgoingEventEncoder.encoder(proposedParty))
    }
  }

  test("make sure we can encode PartyApprovalVoteAcknowledgement event") {
    val playerApprovalVoteAckJson: Json =
      Json.obj("event" := "PartyApprovalVoteAcknowledgement", "id" := constant)

    playerApprovalVoteAckJson should be(OutgoingEventEncoder.encoder(PartyApprovalVoteAcknowledgement.make[IO].unsafeRunSync()))
  }

  //fix this!!!!!
  test("make sure we can encode PartyApproved event") {
    partyApprovedJson should be(OutgoingEventEncoder.encoder(PartyApproved.make[IO].unsafeRunSync()))
  }

  test("make sure we can encode PartyVotes event") {
    forAll { partyVotes: PartyVotes =>
      val json = partyVotesJson(partyVotes)

      json should be(OutgoingEventEncoder.encoder(partyVotes))
    }
  }

  test("make sure we can encode QuestVoteAcknowledgement event") {
    val questVoteAckJson: Json =
      Json.obj("event" := "QuestVoteAcknowledgement", "id" := constant)

    questVoteAckJson should be(OutgoingEventEncoder.encoder(QuestVoteAcknowledgement.make[IO].unsafeRunSync()))
  }

  test("make sure we can encode PassFailVoteResults event") {
    forAll { passFailVoteResults: PassFailVoteResults =>
      val json = passFailVoteResultsJson(passFailVoteResults)

      json should be(OutgoingEventEncoder.encoder(passFailVoteResults))
    }
  }

  test("make sure we can encode QuestDisplayAcknowledgement event") {
    val questDisplayAckJson: Json =
      Json.obj("event" := "QuestDisplayAcknowledgement", "id" := constant)

    questDisplayAckJson should be(OutgoingEventEncoder.encoder(QuestDisplayAcknowledgement.make[IO].unsafeRunSync()))
  }

  test("make sure we can encode AssassinVoteOutgoingEvent event") {
    forAll { assassinVoteOutgoingEvent: AssassinVoteOutgoingEvent =>
      val json = assassinVoteJson(assassinVoteOutgoingEvent)

      json should be(OutgoingEventEncoder.encoder(assassinVoteOutgoingEvent))
    }
  }

  test("make sure we can encode GameOver event") {
    forAll { gameOver: GameOverOutgoingEvent =>
      val json = gameOverJson(gameOver)

      json should be(OutgoingEventEncoder.encoder(gameOver))
    }
  }

  test("make sure we can encode GameNoLongerExists event") {
    val json = gameNoLongerExistsJson

    json should be(OutgoingEventEncoder.encoder(GameNoLongerExists(constant)))
  }

  def gameCreatedJson(moveToLobby: MoveToLobby): Json =
    Json.obj(
      "event" := "MoveToLobby",
      "roomId" := moveToLobby.roomId.value,
      "players" := moveToLobby.players,
      "id" := moveToLobby.id
    )

  def changeInLobbyJson(joinedRoom: ChangeInLobby): Json =
    Json.obj(
      "event" := "ChangeInLobby",
      "players" := joinedRoom.players,
      "id" := joinedRoom.id
    )

  val gameLeftJson: Json =
    Json.obj("event" := "GameLeft", "id" := constant.show)

  def playerRoleEventJson(playerRoleEvent: PlayerInfo): Json =
    Json.obj(
      "event" := "PlayerInfo",
      "character" := playerRoleEvent.character,
      "badGuys" := playerRoleEvent.badGuys.map(_.map(_.nickname)),
      "merlin" := playerRoleEvent.merlin.map(_.map(_.nickname)),
      "id" := playerRoleEvent.id
    )

  def teamAssignmentEventJson(missionProposalEvent: TeamAssignmentPhase): Json =
    Json.obj(
      "event" := "TeamAssignmentPhase",
      "missionNumber" := missionProposalEvent.missionNumber,
      "missionLeader" := missionProposalEvent.missionLeader,
      "missions" := missionProposalEvent.missions,
      "nextMissionLeader" := missionProposalEvent.nextMissionLeader,
      "proposalsLeft" := missionProposalEvent.proposalsLeft,
      "id" := missionProposalEvent.id
    )

  def gameConfigEventJson(gameConfig: GameConfigEvent): Json =
    Json.obj(
      "event" := "GameConfig",
      "config" := Json.obj(
        "percival" := gameConfig.config.percival,
        "morgana" := gameConfig.config.morgana,
        "mordred" := gameConfig.config.mordred,
        "oberon" := gameConfig.config.oberon
      ),
      "id" := gameConfig.id
    )

  def proposedPartyJson(proposedParty: ProposedParty): Json =
    Json.obj(
      "event" := "ProposedParty",
      "proposedParty" := proposedParty.proposedParty,
      "id" := proposedParty.id
    )

  val partyApprovedJson: Json =
    Json.obj("event" := "PartyApproved", "id" := constant.show)

  def partyVotesJson(partyVotes: PartyVotes): Json = {
    Json.obj(
      "event" := "PartyVotes",
      "approvals" := partyVotes.approvals,
      "denies" := partyVotes.denies,
      "id" := partyVotes.id
    )
  }

  def passFailVoteResultsJson(passFailVoteResults: PassFailVoteResults): Json =
    Json.obj(
      "event" := "PassFailVoteResults",
      "passVotes" := passFailVoteResults.passVotes,
      "failVotes" := passFailVoteResults.failVotes,
      "id" := passFailVoteResults.id
    )

  def assassinVoteJson(assassinVote: AssassinVoteOutgoingEvent): Json =
    Json.obj(
      "event" := "AssassinVote",
      "assassinVoteData" := Json.obj(
        "assassin" := assassinVote.assassin,
        "goodGuys" := assassinVote.goodGuys
      ),
      "missions" := assassinVote.missions,
      "id" := assassinVote.id
    )

  def gameOverJson(gameOver: GameOverOutgoingEvent): Json =
    Json.obj(
      "event" := "GameOver",
      "gameOverData" := Json.obj(
        "assassin" := gameOver.assassin,
        "assassinGuess" := gameOver.assassinGuess,
        "merlin" := gameOver.merlin,
        "goodGuys" := gameOver.goodGuys.map(_.nickname.value),
        "badGuys" := gameOver.badGuys.map(_.nickname.value),
        "winningTeam" := gameOver.winningTeam
      ),
      "id" := gameOver.id
    )

  val gameNoLongerExistsJson: Json =
    Json.obj("event" := "GameNoLongerExists", "id" := constant.show)

}
