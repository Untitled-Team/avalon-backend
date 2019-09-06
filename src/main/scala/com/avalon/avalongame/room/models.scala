package com.avalon.avalongame.room

import cats.data.NonEmptyList
import com.avalon.avalongame.common._
import com.avalon.avalongame.events.TeamAssignmentVote

import scala.util.control.NoStackTrace

//==================
// Errors
//==================

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


case class MissionProposal(missionNumber: Int, missionLeader: Nickname, users: List[Nickname])

sealed abstract case class Mission(players: Option[List[User]], numberOfAdventurers: Int)

object Mission {
  def make(players: Option[List[User]], numberOfAdventurers: Int): Mission =
    new Mission(players, numberOfAdventurers){}
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

  def fromInt(missions: Missions, n: Int): Either[Throwable, Mission] =
    n match {
      case 1 => Right(missions.one)
      case 2 => Right(missions.two)
      case 3 => Right(missions.three)
      case 4 => Right(missions.four)
      case 5 => Right(missions.five)
      case _ => Left(InvalidMissionNumber(n))
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
case class MissionVoting(missionNumber: Int, missionLeader: Nickname, users: List[Nickname], votes: List[TeamAssignmentVote]) extends GameState
case class MissionProposed(voters: NonEmptyList[User]) extends GameState

case class GameRepresentation(state: GameState,
                              missions: Missions,
                              badGuys: List[BadPlayerRole],
                              goodGuys: List[GoodPlayerRole],
                              users: List[Nickname])

case class InternalRoom(players: List[Nickname], gameRepresentation: Option[GameRepresentation])