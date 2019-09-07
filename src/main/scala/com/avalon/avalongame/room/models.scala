package com.avalon.avalongame.room

import cats.data.NonEmptyList
import com.avalon.avalongame.common._
import com.avalon.avalongame.events.TeamAssignmentVote

import scala.util.control.NoStackTrace

//==================
// Errors
//==================

case class PlayerCantVoteMoreThanOnce(nickname: Nickname) extends RuntimeException with NoStackTrace
case object GameHasStarted extends RuntimeException with NoStackTrace
case class NicknameAlreadyInUse(nickname: Nickname) extends RuntimeException with NoStackTrace
case class InvalidStateTransition(gameState: GameState, to: String, nickname: Nickname) extends RuntimeException with NoStackTrace
case class UserNotMissionLeader(nickname: Nickname) extends RuntimeException with NoStackTrace
case class InvalidUserCountForMission(n: Int) extends RuntimeException with NoStackTrace
case class InvalidMissionNumber(n: Int) extends RuntimeException with NoStackTrace
case class NotEnoughPlayers(playerCount: Int) extends RuntimeException with NoStackTrace
case object GameNotStarted extends RuntimeException with NoStackTrace
case class PlayerAlreadyReady(nickname: Nickname) extends RuntimeException with NoStackTrace

//==================
// Game stuff
//==================


case class MissionProposal(missionNumber: Int, missionLeader: Nickname, players: List[Nickname])
case class FailedTeamVote(votes: List[PlayerTeamVote])

sealed abstract case class Mission(players: Option[List[Nickname]], numberOfAdventurers: Int, failedVotes: List[FailedTeamVote])

object Mission {
  def make(players: Option[List[Nickname]], numberOfAdventurers: Int): Mission =
    new Mission(players, numberOfAdventurers, Nil){}

  def addFailedVote(mission: Mission, votes: List[PlayerTeamVote]): Mission =
    new Mission(mission.players, mission.numberOfAdventurers, mission.failedVotes :+ FailedTeamVote(votes)){}

  //maybe fail if we try to update this when the players have already been set??????
  def addQuesters(mission: Mission, players: List[Nickname]): Mission =
    new Mission(Some(players), mission.numberOfAdventurers, mission.failedVotes){}

}

sealed abstract case class Missions(one: Mission,
                                    two: Mission,
                                    three: Mission,
                                    four: Mission,
                                    five: Mission)
object Missions {
  def fromPlayers(players: Int): Either[Throwable, Missions] = players match {
    case 5  =>
      Right(
        new Missions(
          Mission.make(None, 2),
          Mission.make(None, 3),
          Mission.make(None, 2),
          Mission.make(None, 3),
          Mission.make(None, 3)){})
    case 6  =>
      Right(
        new Missions(
          Mission.make(None, 2),
          Mission.make(None, 3),
          Mission.make(None, 4),
          Mission.make(None, 3),
          Mission.make(None, 4)){})
    case 7  =>
      Right(
        new Missions(
          Mission.make(None, 2),
          Mission.make(None, 3),
          Mission.make(None, 3),
          Mission.make(None, 4),
          Mission.make(None, 4)){})
    case 8 | 9 | 10  =>
      Right(
        new Missions(
          Mission.make(None, 3),
          Mission.make(None, 4),
          Mission.make(None, 4),
          Mission.make(None, 5),
          Mission.make(None, 5)){})

    case _ => Left(NotEnoughPlayers(players))
  }

  //tests
  def fromInt(missions: Missions, n: Int): Either[Throwable, Mission] =
    n match {
      case 1 => Right(missions.one)
      case 2 => Right(missions.two)
      case 3 => Right(missions.three)
      case 4 => Right(missions.four)
      case 5 => Right(missions.five)
      case _ => Left(InvalidMissionNumber(n))
    }

  //tests
  def addFailedVote(missions: Missions, missionNumber: Int, failedVotes: List[PlayerTeamVote]): Either[Throwable, Missions] =
    missionNumber match {
      case 1  => Right(new Missions(Mission.addFailedVote(missions.one, failedVotes), missions.two, missions.three, missions.four, missions.five){})
      case 2  => Right(new Missions(missions.one, Mission.addFailedVote(missions.two, failedVotes), missions.three, missions.four, missions.five){})
      case 3  => Right(new Missions(missions.one, missions.two, Mission.addFailedVote(missions.three, failedVotes), missions.four, missions.five){})
      case 4  => Right(new Missions(missions.one, missions.two, missions.three, Mission.addFailedVote(missions.four, failedVotes), missions.five){})
      case 5  => Right(new Missions(missions.one, missions.two, missions.three, missions.four, Mission.addFailedVote(missions.five, failedVotes)){})
      case _  => Left(InvalidMissionNumber(missionNumber))
    }

  def addQuesters(missions: Missions, missionNumber: Int, players: List[Nickname]): Either[Throwable, Missions] =
    missionNumber match {
      case 1  => Right(new Missions(Mission.addQuesters(missions.one, players), missions.two, missions.three, missions.four, missions.five){})
      case 2  => Right(new Missions(missions.one, Mission.addQuesters(missions.two, players), missions.three, missions.four, missions.five){})
      case 3  => Right(new Missions(missions.one, missions.two, Mission.addQuesters(missions.three, players), missions.four, missions.five){})
      case 4  => Right(new Missions(missions.one, missions.two, missions.three, Mission.addQuesters(missions.four, players), missions.five){})
      case 5  => Right(new Missions(missions.one, missions.two, missions.three, missions.four, Mission.addQuesters(missions.five, players)){})
      case _  => Left(InvalidMissionNumber(missionNumber))
    }
}

sealed trait Role

sealed trait BadGuy extends Role
case object Assassin extends BadGuy
case object NormalBadGuy extends BadGuy

sealed trait GoodGuy extends Role
case object Merlin extends GoodGuy
case object NormalGoodGuy extends GoodGuy


sealed trait PlayerRole {
  def role: Role
}
case class GoodPlayerRole(nickname: Nickname, role: GoodGuy) extends PlayerRole
case class BadPlayerRole(nickname: Nickname, role: BadGuy) extends PlayerRole

sealed trait GameState
case object Lobby extends GameState
case class PlayersReadingRole(playersReady: List[Nickname]) extends GameState
case class MissionProposing(missionNumber: Int, missionLeader: Nickname) extends GameState
case class MissionVoting(missionNumber: Int, missionLeader: Nickname, users: List[Nickname], votes: List[PlayerTeamVote]) extends GameState
case class MissionProposed(voters: NonEmptyList[User]) extends GameState
case class QuestPhase(missionNumber: Int, questers: List[Nickname]) extends GameState

case class AllPlayerRoles(goodGuys: List[GoodPlayerRole], badGuys: List[BadPlayerRole])

case class GameRepresentation(state: GameState,
                              missions: Missions,
                              badGuys: List[BadPlayerRole],
                              goodGuys: List[GoodPlayerRole],
                              users: List[Nickname])

case class InternalRoom(players: List[Nickname], gameRepresentation: Option[GameRepresentation])

case class PlayerTeamVote(nickname: Nickname, vote: TeamVote)