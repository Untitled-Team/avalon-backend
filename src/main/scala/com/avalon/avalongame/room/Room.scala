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
  def questVote(nickname: Nickname, vote: QuestVote): F[QuestVotingEnum]
  def questResultsSeen(nickname: Nickname): F[AfterQuest]
}

object Room {

  private[room] def buildPrivate[F[_]](randomAlg: RandomAlg[F],
                                       roomId: RoomId, //this could maybe all be simplified with a Semaphore of size 1
                                       mvar: MVar[F, InternalRoom])(implicit F: Concurrent[F]): Room[F] =
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
                case _ => F.raiseError[PlayersReadingRole](InvalidStateTransition(repr.state, "playerReady", nickname))
              }
              _ <- if (state.playersReady.contains(nickname)) F.raiseError[Unit](PlayerAlreadyReady(nickname)) else F.unit
              updatedState = state.copy(state.playersReady :+ nickname)
              enum <-
                if (updatedState.playersReady.size === room.players.size)
                  (for {
                    missionLeader <- randomAlg.randomGet(room.players)
                    updatedRepr   =  repr.copy(state = MissionProposing(1, missionLeader))
                    _             <- mvar.put(room.copy(gameRepresentation = Some(updatedRepr)))
                  } yield AllReady(1, missionLeader, repr.missions)).widen[PlayerReadyEnum]
                else {
                  val updatedRepr = repr.copy(state = updatedState)
                  mvar.put(room.copy(gameRepresentation = Some(updatedRepr)))
                    .map(_ => NotReadyYet(room.players diff updatedState.playersReady))
                    .widen[PlayerReadyEnum]
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
                else F.raiseError[Unit](UserNotMissionLeader(nickname))

              currentMission <- F.fromEither(Missions.fromInt(repr.missions, proposal.missionNumber)) //function for this?

              _ <-
                if (currentMission.numberOfAdventurers === users.size) F.unit
                else F.raiseError[Unit](InvalidUserCountForMission(users.size))

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

              result <-
                if (updatedState.votes.size === room.players.size)
                  if (updatedState.votes.count(_.vote === TeamVote(false)) >= updatedState.votes.count(_.vote === TeamVote(true)))
                    (for {
                      updatedMissions <- F.fromEither(Missions.addFinishedTeamVote(repr.missions, proposal.missionNumber, updatedState.votes))
                      newMissionLeader <- randomAlg.randomGet(room.players.filter(_ =!= proposal.missionLeader))
                    } yield FailedVote(newMissionLeader, proposal.missionNumber, updatedState.votes, updatedMissions)).widen[TeamVoteEnum]

                  else F.pure(SuccessfulVote(updatedState.votes)).widen[TeamVoteEnum]
                else
                  F.pure(TeamPhaseStillVoting).widen[TeamVoteEnum]

              updatedRepr <- result match {
                case TeamPhaseStillVoting => F.pure(repr.copy(state = updatedState))
                case FailedVote(ml, _, _, missions) =>
                  F.pure {
                    repr.copy(
                      state = MissionProposing(proposal.missionNumber, ml),//need to cycle mission leaders better
                      missions = missions)
                  }
                case SuccessfulVote(votes) =>
                  F.fromEither(Missions.addFinishedTeamVote(repr.missions, proposal.missionNumber, votes)).flatMap { m =>
                    F.fromEither(Missions.addQuesters(m, proposal.missionNumber, proposal.users)).map { updatedMissions =>
                      repr.copy(
                        state = QuestPhase(proposal.missionNumber, proposal.missionLeader, proposal.users, Nil),
                        missions = updatedMissions)
                    }
                  }
              }
              _ <- mvar.put(room.copy(gameRepresentation = Some(updatedRepr)))
            } yield result)
              .guaranteeCase {
                case ExitCase.Error(_) | ExitCase.Canceled => mvar.put(room)
                case ExitCase.Completed => F.unit
              }
          }

        def questVote(nickname: Nickname, vote: QuestVote): F[QuestVotingEnum] =
          mvar.take.flatMap { room =>
            (for {
              repr <- F.fromOption(room.gameRepresentation, GameNotStarted)
              questPhase <- repr.state match {
                case m@QuestPhase(_, _, _, _) => F.pure(m)
                case _ => F.raiseError[QuestPhase](InvalidStateTransition(repr.state, "questVote", nickname))
              }

              _ <- if (questPhase.questers.contains(nickname)) F.unit else F.raiseError[Unit](PlayerNotPartOfQuest(nickname))

              //good player cannot choose false
              validPlayerVote = if (repr.goodGuys.map(_.nickname).contains(nickname)) QuestVote(true) else vote

              updatedState <-
                if (questPhase.votes.map(_.nickname).contains(nickname))
                  F.raiseError[QuestPhase](PlayerCantVoteMoreThanOnce(nickname))
                else F.pure(questPhase.copy(votes = questPhase.votes :+ PlayerQuestVote(nickname, validPlayerVote)))

              currentNumberOfAdventurers <- F.fromEither(Missions.fromInt(repr.missions, questPhase.missionNumber)).map(_.numberOfAdventurers)

              currentMissionPassStatus = !updatedState.votes.map(_.vote).contains(QuestVote(false))
              updatedRepr <-
                if (updatedState.votes.size === currentNumberOfAdventurers)
                  F.fromEither[Missions](
                    Missions.completeMission(repr.missions, questPhase.missionNumber, QuestVote(currentMissionPassStatus)))
                    .map[GameRepresentation] { updatedMissions =>
                      if (Missions.failedQuests(updatedMissions) === 3)
                        repr.copy(state = QuestResultsViewing(BadGuysWin, Nil), missions = updatedMissions)
                      else if (Missions.successfulQuests(updatedMissions) === 3)
                        repr.copy(state = QuestResultsViewing(AssassinNeedsToVote, Nil), missions = updatedMissions)
                      else repr.copy(state = QuestResultsViewing(NextMission(questPhase.missionLeader), Nil), missions = updatedMissions)
                    }
                else
                  F.pure(repr.copy(state = updatedState))


              result <-
                if (updatedState.votes.size === currentNumberOfAdventurers)
                  F.pure(FinishedVote(updatedState.votes.map(_.vote))).widen[QuestVotingEnum]
                else F.pure(QuestPhaseStillVoting).widen[QuestVotingEnum]

              _ <- mvar.put(room.copy(gameRepresentation = Some(updatedRepr)))
            } yield result)
              .guaranteeCase {
                case ExitCase.Error(_) | ExitCase.Canceled => mvar.put(room)
                case ExitCase.Completed => F.unit
              }
          }

        def questResultsSeen(nickname: Nickname): F[AfterQuest] =
          mvar.take.flatMap { room =>
            (for {
              repr <- F.fromOption(room.gameRepresentation, GameNotStarted)
              questResultsViewing <- repr.state match {
                case q@QuestResultsViewing(_, _) => F.pure(q)
                case _ => F.raiseError[QuestResultsViewing](InvalidStateTransition(repr.state, "questResultsSeen", nickname))
              }

              updatedState <-
                if (questResultsViewing.viewed.contains(nickname))
                  F.raiseError[QuestResultsViewing](PlayerAlreadyViewedQuestResults(nickname))
                else F.pure(questResultsViewing.copy(viewed = questResultsViewing.viewed :+ nickname))

              resultAndUpdatedRepr <-
                if (updatedState.viewed.size === repr.users.size)
                  updatedState.info match {
                    case NextMission(previousLeader) =>
                      (for {
                        nextMissionLeader <- randomAlg.randomGet(room.players.filter(_ =!= previousLeader))
                        currentMission <- F.fromEither[Mission](Missions.currentMission(repr.missions))
                      } yield
                        (GameContinues(nextMissionLeader, currentMission.number, repr.missions),
                          repr.copy(state = MissionProposing(currentMission.number, nextMissionLeader)))).widen[(AfterQuest, GameRepresentation)]
                    case AssassinNeedsToVote => F.pure {
                      (AssassinVote(repr.goodGuys), repr.copy(state = AssassinVoteState))
                    }.widen[(AfterQuest, GameRepresentation)]
                    case BadGuysWin => F.pure {
                      (BadGuyVictory, repr.copy(BadSideWins))
                    }.widen[(AfterQuest, GameRepresentation)]
                  }
                else
                  F.pure((StillViewingQuestResults, repr.copy(state = updatedState))).widen[(AfterQuest, GameRepresentation)]

              (result, updatedRepr) = resultAndUpdatedRepr

              _ <- mvar.put(room.copy(gameRepresentation = Some(updatedRepr)))
            } yield result)
              .guaranteeCase {
                case ExitCase.Error(_) | ExitCase.Canceled => mvar.put(room)
                case ExitCase.Completed => F.unit
              }
          }
      }

  //timeouts on get/reads?
  def build[F[_]](randomAlg: RandomAlg[F], roomId: RoomId)(implicit F: Concurrent[F]): F[Room[F]] =
    MVar.of(InternalRoom(Nil, None)).map(buildPrivate(randomAlg, roomId, _))
}

sealed trait TeamVoteEnum
case object TeamPhaseStillVoting extends TeamVoteEnum
case class FailedVote(newMissionLeader: Nickname, missionNumber: Int, votes: List[PlayerTeamVote], missions: Missions) extends TeamVoteEnum
case class SuccessfulVote(votes: List[PlayerTeamVote]) extends TeamVoteEnum


sealed trait QuestVotingEnum
case object QuestPhaseStillVoting extends QuestVotingEnum
case class FinishedVote(votes: List[QuestVote]) extends QuestVotingEnum


sealed trait AfterQuest
case object StillViewingQuestResults extends AfterQuest
case class AssassinVote(goodGuys: List[GoodPlayerRole]) extends AfterQuest
case object BadGuyVictory extends AfterQuest
case class GameContinues(missionLeader: Nickname, missionNumber: Int, missions: Missions) extends AfterQuest