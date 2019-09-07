package com.avalon.avalongame.room

import cats.effect._
import cats.effect.concurrent.MVar
import cats.effect.implicits._
import cats.implicits._
import com.avalon.avalongame.RandomAlg
import com.avalon.avalongame.common._

sealed trait PlayerReadyEnum
case class NotReadyYet(nicknames: List[Nickname]) extends PlayerReadyEnum
case class AllReady(missionNumber: Int, missionLeader: Nickname, missions: Missions) extends PlayerReadyEnum

trait Room[F[_]] {
  def players: F[List[Nickname]]
  def addUser(player: Nickname): F[Unit]
  def startGame: F[AllPlayerRoles]
  def playerReady(nickname: Nickname): F[PlayerReadyEnum]
  def proposeMission(nickname: Nickname, users: List[Nickname]): F[MissionProposal]
  def teamVote(nickname: Nickname, vote: TeamVote): F[TeamVoteEnum]
}

object Room {

  //timeouts on get/reads?
  def build[F[_]](randomAlg: RandomAlg[F], roomId: RoomId)(implicit F: Concurrent[F]): F[Room[F]] =
    MVar.of(InternalRoom(Nil, None)).map { mvar =>
      new Room[F] {
        def state: F[Option[GameState]] = mvar.read.map(_.gameRepresentation.map(_.state))

        def players: F[List[Nickname]] = mvar.read.map(_.players)

        def addUser(player: Nickname): F[Unit] =
          mvar.take.flatMap { room =>
            if (room.players.contains(player))
              mvar.put(room) >> F.raiseError(NicknameAlreadyInUse(player))
            else if(room.gameRepresentation.nonEmpty)
              mvar.put(room) >> F.raiseError(GameHasStarted)
            else
              mvar.put(room.copy(room.players :+ player))
          }

        def startGame: F[AllPlayerRoles] =
          mvar.take.flatMap { room =>
            (for {
              missions      <- F.fromEither(Missions.fromPlayers(room.players.size))
              roles         <- Utils.assignRoles(room.players, randomAlg.shuffle)
              repr          =  GameRepresentation(PlayersReadingRole(Nil), missions, roles.badGuys, roles.goodGuys, room.players)
              _             <- mvar.put(room.copy(gameRepresentation = Some(repr)))
            } yield AllPlayerRoles(repr.goodGuys, repr.badGuys))
              .guaranteeCase {
                case ExitCase.Error(_) | ExitCase.Canceled => mvar.put(room)
                case ExitCase.Completed => F.unit
              }
          }

        def playerReady(nickname: Nickname): F[PlayerReadyEnum] =
          mvar.take.flatMap { room =>
            (for {
              repr  <- F.fromOption(room.gameRepresentation, GameNotStarted)
              state: PlayersReadingRole <- repr.state match {
                case m@PlayersReadingRole(_) => F.pure(m)
                case _ => F.raiseError(InvalidStateTransition(repr.state, "playerReady", nickname))
              }
              _ <- if (state.playersReady.contains(nickname)) F.raiseError(PlayerAlreadyReady(nickname)) else F.unit
              updatedState = state.copy(state.playersReady :+ nickname)
              enum: PlayerReadyEnum <-
                if (updatedState.playersReady.size === room.players.size) {
                  for {
                    missionLeader <- randomAlg.randomGet(room.players)
                    updatedRepr   =  repr.copy(state = MissionProposing(1, missionLeader))
                    _             <- mvar.put(room.copy(gameRepresentation = Some(updatedRepr)))
                  } yield AllReady(1, missionLeader, repr.missions)
                }
                else {
                  val updatedRepr = repr.copy(state = updatedState)
                  mvar.put(room.copy(gameRepresentation = Some(updatedRepr))).map(_ => NotReadyYet(room.players diff updatedState.playersReady))
                }
            } yield enum)
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
                case _ => F.raiseError[MissionProposing](InvalidStateTransition(repr.state, "proposeMission", nickname))
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

        def teamVote(nickname: Nickname, vote: TeamVote): F[TeamVoteEnum] =
          mvar.take.flatMap { room =>
            (for {
              repr <- F.fromOption(room.gameRepresentation, GameNotStarted)
              proposal <- repr.state match {
                case m@MissionVoting(_, _, _, _) => F.pure(m)
                case _ => F.raiseError[MissionVoting](InvalidStateTransition(repr.state, "teamVote", nickname))
              }
              updatedState <-
                if (proposal.votes.map(_.nickname).contains(nickname))
                  F.raiseError[MissionVoting](PlayerCantVoteMoreThanOnce(nickname))
                else F.pure(proposal.copy(votes = proposal.votes :+ PlayerTeamVote(nickname, vote)))

              result: TeamVoteEnum =
                if (updatedState.votes.size === room.players.size) //more precise check that all names appear
                  if (updatedState.votes.filter(_.vote === TeamVote(false)).size === 1) // fix
                    FailedVote(updatedState.votes)
                  else SuccessfulVote(updatedState.votes)
                else
                  StillVoting

              updatedRepr: GameRepresentation <- result match {
                case StillVoting => F.pure(repr.copy(state = updatedState))
                case FailedVote(votes) =>
                  F.fromEither(Missions.addFailedVote(repr.missions, proposal.missionNumber, votes)).map { m =>
                    repr.copy(
                      state = MissionProposing(proposal.missionNumber, proposal.missionLeader),
                      missions = m)
                  }
                case SuccessfulVote(votes) =>
                  F.fromEither(Missions.addQuesters(repr.missions, proposal.missionNumber, proposal.users)).map { m =>
                    repr.copy(
                      state = QuestPhase(proposal.missionNumber, proposal.users),
                      missions = m)
                  }
              }
              _ <- mvar.put(room.copy(gameRepresentation = Some(updatedRepr)))
            } yield result)
              .guaranteeCase {
                case ExitCase.Error(_) | ExitCase.Canceled => mvar.put(room)
                case ExitCase.Completed => F.unit
              }
          }
      }
    }
}

sealed trait TeamVoteEnum
case object StillVoting extends TeamVoteEnum
case class FailedVote(votes: List[PlayerTeamVote]) extends TeamVoteEnum
case class SuccessfulVote(votes: List[PlayerTeamVote]) extends TeamVoteEnum
