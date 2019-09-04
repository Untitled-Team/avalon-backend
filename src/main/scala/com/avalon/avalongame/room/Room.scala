package com.avalon.avalongame.room

import cats.effect._
import cats.effect.concurrent.MVar
import cats.effect.implicits._
import cats.implicits._
import com.avalon.avalongame.RandomAlg
import com.avalon.avalongame.common._

import scala.util.control.NoStackTrace

trait Room[F[_]] {
  def info: F[RoomInfo]
  def players: F[List[Nickname]]
  def addUser(player: Nickname): F[Unit]
  def startGame: F[GameRepresentation]
  def proposeMission(nickname: Nickname, users: List[Nickname]): F[MissionProposal]
  def vote(nickname: Nickname, vote: Vote): F[Unit]
}

object Room {

  //timeouts on get/reads?
  def build[F[_]](randomAlg: RandomAlg[F], roomId: RoomId, config: GameConfig)(implicit F: Concurrent[F]): F[Room[F]] =
    MVar.of(InternalRoom(Nil, None)).map { mvar =>
      new Room[F] {
        def info: F[RoomInfo] = players.map(RoomInfo(_, config))

        def players: F[List[Nickname]] = mvar.read.map(_.players)

        //need to enforce that this is only possible when no game representation?
        def addUser(player: Nickname): F[Unit] =
          mvar.take.flatMap { room =>
            if (room.players.contains(player))
              mvar.put(room) >> F.raiseError(NicknameAlreadyInUse(player))
            else
              mvar.put(room.copy(room.players :+ player))
          }

        def startGame: F[GameRepresentation] =
          mvar.take.flatMap { room =>
            (for {
              missions      <- F.fromEither(Missions.fromPlayers(room.players.size))
              roles         <- Utils.assignRoles(room.players, randomAlg.shuffle)
              missionLeader <- randomAlg.randomGet(room.players)
              repr          =  GameRepresentation(MissionProposing(1, missionLeader), missions, roles.badGuys, roles.goodGuys, room.players)
              _             <- mvar.put(room.copy(gameRepresentation = Some(repr)))
            } yield repr)
              .guaranteeCase {
                case ExitCase.Error(_) | ExitCase.Canceled => mvar.put(room)
                case ExitCase.Completed => F.unit
              }
          }

        //should verify the users provided are actual users in the game as well......
        //maybe we can store each previous state so we have a full track record of everything that happened
        def proposeMission(nickname: Nickname, users: List[Nickname]): F[MissionProposal] =
          mvar.take.flatMap { room =>
            (for {
              repr <- F.fromOption(room.gameRepresentation, GameNotStarted)
              proposal <- repr.state match {
                case m@MissionProposing(_, _) => F.pure(m)
                case _ => F.raiseError(InvalidStateTransition(repr.state, "proposeMission", nickname))
              }
              _ <-
                if (proposal.missionLeader === nickname) F.unit
                else F.raiseError(UserNotMissionLeader(nickname))
              currentMission <- F.fromEither(Missions.fromInt(repr.missions, proposal.missionNumber))
              _ <-
                if (currentMission.numberOfAdventurers === users.size) F.unit
                else F.raiseError(InvalidUserCountForMission(users.size))
              _ <- mvar.put {
                room.copy(gameRepresentation = Some(repr.copy(state = MissionVoting(proposal.missionNumber, nickname, users, Nil))))
              }
            } yield MissionProposal(proposal.missionNumber, proposal.missionLeader, users))
              .guaranteeCase {
                case ExitCase.Error(_) | ExitCase.Canceled => mvar.put(room)
                case ExitCase.Completed => F.unit
              }
          }

        def vote(nickname: Nickname, vote: Vote): F[Unit] = F.unit
//          mvar.take.flatMap { room =>
//
//          }
      }
    }
}
