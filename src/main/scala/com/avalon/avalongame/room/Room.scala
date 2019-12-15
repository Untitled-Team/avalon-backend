package com.avalon.avalongame.room

import cats.effect._
import cats.effect.concurrent.MVar
import cats.effect.implicits._
import cats.implicits._
import com.avalon.avalongame.RandomAlg
import com.avalon.avalongame.common._

case class AllReady(missionNumber: Int, missionLeader: Nickname, missions: Missions, nextMissionLeader: Nickname, proposalsLeft: Int)

trait Room[F[_]] {
  def players: F[List[Nickname]]
  def addUser(player: Nickname): F[Unit]
  def removePlayer(player: Nickname): F[Unit]
  def startGame: F[StartGameInfo]
  def proposeMission(nickname: Nickname, users: List[Nickname]): F[MissionProposal]
  def teamVote(nickname: Nickname, vote: TeamVote): F[Either[GameOver, TeamVoteEnum]]
  def questVote(nickname: Nickname, vote: QuestVote): F[QuestVotingEnum]
  def questResultsSeen(nickname: Nickname): F[AfterQuest]
  def assassinVote(assassin: Nickname, guess: Nickname): F[GameOver]
}

object Room {

  //this could all be simplified with sempahore(1)
  private[room] def buildPrivate[F[_]](randomAlg: RandomAlg[F],
                                       roomId: RoomId,
                                       mvar: MVar[F, InternalRoom])(implicit F: Concurrent[F]): Room[F] =
      new Room[F] {
        def state: F[Option[GameState]] = mvar.read.map(_.gameRepresentation.map(_.state))

        def players: F[List[Nickname]] = mvar.read.map(_.players)

        def addUser(player: Nickname): F[Unit] =
          mvar.take.flatMap { room =>
            if (room.players.size >= 10)
              F.raiseError(RoomIsFull(roomId))
            else if (room.players.contains(player))
              mvar.put(room) >> F.raiseError(NicknameAlreadyInUse(player))
            else if(room.gameRepresentation.nonEmpty)
              mvar.put(room) >> F.raiseError(GameHasStarted)
            else
              mvar.put(room.copy(players = room.players :+ player))
          }

        def removePlayer(player: Nickname): F[Unit] =
          mvar.take.flatMap { room =>
            if(room.gameRepresentation.nonEmpty)
              mvar.put(room) >> F.raiseError(GameHasStarted)
            else if (!room.players.contains(player))
              mvar.put(room) >> F.raiseError(UserIsntInGame(player))
            else
              mvar.put(room.copy(players = room.players.filter(_ =!= player)))
          }

        def startGame: F[StartGameInfo] =
          mvar.take.flatMap { room =>
            (for {
              missions          <- F.fromEither(Missions.fromPlayers(room.players.size))
              roles             <- Utils.assignRoles(room.players, randomAlg.shuffle)
              missionLeader     <- randomAlg.randomGet(room.players)
              repr              =  GameRepresentation(MissionProposing(1, missionLeader, 5), missions, roles.badGuys, roles.goodGuys, room.players)
              _                 <- mvar.put(room.copy(gameRepresentation = Some(repr)))
              nextMissionLeader <- randomAlg.clockwise(missionLeader, room.players)
              ready             =  AllReady(1, missionLeader, repr.missions, nextMissionLeader, 5)
            } yield StartGameInfo(AllPlayerRoles(repr.goodGuys, repr.badGuys), ready))
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
                case m@MissionProposing(_, _, _) => F.pure(m)
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
                room.copy(gameRepresentation = Some(repr.copy(state = MissionVoting(proposal.missionNumber, nickname, users, Nil, proposal.votesLeft))))
              }

              nextMissionLeader <- randomAlg.clockwise(proposal.missionLeader, room.players)
            } yield MissionProposal(proposal.missionNumber, proposal.missionLeader, users, proposal.votesLeft - 1, nextMissionLeader))
              .guaranteeCase {
                case ExitCase.Error(_) | ExitCase.Canceled => mvar.put(room)
                case ExitCase.Completed => F.unit
              }
          }

        def teamVote(nickname: Nickname, vote: TeamVote): F[Either[GameOver, TeamVoteEnum]] =
          mvar.take.flatMap { room =>
            (for {
              repr <- F.fromOption(room.gameRepresentation, GameNotStarted)
              proposal <- repr.state match {
                case m@MissionVoting(_, _, _, _, _) => F.pure(m)
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
                      updatedMissions <- F.fromEither(
                        Missions.addFinishedTeamVote(repr.missions, proposal.missionNumber, proposal.missionLeader, updatedState.votes))
                      newMissionLeader <- randomAlg.clockwise(proposal.missionLeader, room.players)
                      nextMissionleader <- randomAlg.clockwise(newMissionLeader, room.players)
                    } yield
                      FailedVote(
                        newMissionLeader,
                        proposal.missionNumber,
                        updatedState.votes,
                        updatedMissions,
                        nextMissionleader,
                        proposal.votesLeft - 1)).widen[TeamVoteEnum]

                  else F.pure(SuccessfulVote(updatedState.votes)).widen[TeamVoteEnum]
                else
                  F.pure(TeamPhaseStillVoting).widen[TeamVoteEnum]

              updatedRepr <- result match {
                case TeamPhaseStillVoting => F.pure(repr.copy(state = updatedState))
                case FailedVote(ml, _, _, missions, _, votesLeft) =>
                  F.pure {
                    repr.copy(
                      state = MissionProposing(proposal.missionNumber, ml, votesLeft),//need to cycle mission leaders better
                      missions = missions)
                  }
                case SuccessfulVote(votes) =>
                  F.fromEither(Missions.addFinishedTeamVote(repr.missions, proposal.missionNumber, proposal.missionLeader, votes)).flatMap { m =>
                    F.fromEither(Missions.addQuesters(m, proposal.missionNumber, proposal.users)).map { updatedMissions =>
                      repr.copy(
                        state = QuestPhase(proposal.missionNumber, proposal.missionLeader, proposal.users, Nil),
                        missions = updatedMissions)
                    }
                  }
              }

              gameOverCheck: Either[GameOver, TeamVoteEnum] <-
                result match {
                  case FailedVote(_, _, _, _, _, _) =>
                    if ((proposal.votesLeft - 1) === 0)
                      badGuysWinGameOver(repr).map(Left(_))
                    else F.pure(Right(result))
                  case _ => F.pure(Right(result))
                }

              _ <- mvar.put(room.copy(gameRepresentation = Some(updatedRepr)))
            } yield gameOverCheck).widen[Either[GameOver, TeamVoteEnum]]
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

              currentMissionNumber <- F.fromEither(Missions.currentMission(repr.missions))

              currentMissionPassStatus =
                if (room.players.size >= 7 && currentMissionNumber.number === 4) !(updatedState.votes.map(_.vote).count(_ === QuestVote(false)) >= 2)
                else !updatedState.votes.map(_.vote).contains(QuestVote(false))

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
                        missionLeader <- randomAlg.clockwise(previousLeader, room.players)
                        nextMissionLeader <- randomAlg.clockwise(missionLeader, room.players)
                        currentMission <- F.fromEither[Mission](Missions.currentMission(repr.missions))
                      } yield
                        (GameContinues(missionLeader, currentMission.number, repr.missions, nextMissionLeader, 5), //better way of starting it over at 0?
                          repr.copy(state = MissionProposing(currentMission.number, missionLeader, 5)))).widen[(AfterQuest, GameRepresentation)]
                    case AssassinNeedsToVote => //better type check here???
                      F.fromOption(repr.badGuys.find(_.role == Assassin), NoPlayerIsTheAssassinSomehow).map { assassin =>
                        (AssassinVote(assassin.nickname, repr.goodGuys, repr.missions), repr.copy(state = AssassinVoteState))
                      }.widen[(AfterQuest, GameRepresentation)]
                    case BadGuysWin =>
                      (for {
                        assassin <- F.fromOption(repr.badGuys.find(_.role == Assassin), NoPlayerIsTheAssassinSomehow)
                        merlin   <- F.fromOption(repr.goodGuys.find(_.role == Merlin), NoPlayerIsMerlinSomehow)
                      } yield
                        (BadGuyVictory(assassin, None, merlin, repr.goodGuys, repr.badGuys, BadGuys), repr.copy(BadSideWins)))
                        .widen[(AfterQuest, GameRepresentation)]
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

        def assassinVote(assassin: Nickname, guess: Nickname): F[GameOver] =
          mvar.take.flatMap { room =>
            (for {
              repr <- F.fromOption(room.gameRepresentation, GameNotStarted)
              _ <- repr.state match {
                case AssassinVoteState => F.unit
                case _ => F.raiseError[Unit](InvalidStateTransition(repr.state, "assassinVote", assassin))
              }

              actualAssassin <- F.fromOption(repr.badGuys.find(_.role == Assassin), NoPlayerIsTheAssassinSomehow)
              merlin <- F.fromOption(repr.goodGuys.find(_.role == Merlin), NoPlayerIsMerlinSomehow)

              _ <- if (assassin === actualAssassin.nickname) F.unit else F.raiseError[Unit](PlayerIsNotTheAssassin(assassin))

              winningSide = if (merlin.nickname === guess) BadGuys else GoodGuys

              updatedRepr = winningSide match {
                case BadGuys => repr.copy(state = BadSideWins)
                case GoodGuys => repr.copy(state = GoodSideWins)
              }

              result = GameOver(actualAssassin, Some(guess), merlin, repr.goodGuys, repr.badGuys, winningSide)

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


  def badGuysWinGameOver[F[_]](repr: GameRepresentation)(implicit F: Sync[F]): F[GameOver] =
    for {
      actualAssassin <- F.fromOption(repr.badGuys.find(_.role == Assassin), NoPlayerIsTheAssassinSomehow)
      merlin <- F.fromOption(repr.goodGuys.find(_.role == Merlin), NoPlayerIsMerlinSomehow)
    } yield GameOver(actualAssassin, None, merlin, repr.goodGuys, repr.badGuys, BadGuys)
}

sealed trait TeamVoteEnum
case object TeamPhaseStillVoting extends TeamVoteEnum
case class FailedVote(newMissionLeader: Nickname,
                      missionNumber: Int,
                      votes: List[PlayerTeamVote],
                      missions: Missions,
                      nextMissionLeader: Nickname,
                      proposalsLeft: Int) extends TeamVoteEnum
case class SuccessfulVote(votes: List[PlayerTeamVote]) extends TeamVoteEnum


sealed trait QuestVotingEnum
case object QuestPhaseStillVoting extends QuestVotingEnum
case class FinishedVote(votes: List[QuestVote]) extends QuestVotingEnum


sealed trait AfterQuest
case object StillViewingQuestResults extends AfterQuest
case class AssassinVote(assassin: Nickname, goodGuys: List[GoodPlayerRole], missions: Missions) extends AfterQuest
case class BadGuyVictory(assassin: BadPlayerRole,
                         assassinGuess: Option[Nickname],
                         merlin: GoodPlayerRole,
                         goodGuys: List[GoodPlayerRole],
                         badGuys: List[BadPlayerRole],
                         winningTeam: Side) extends AfterQuest
//votesLeft is something we maybe don't need to include here because we know it's 5...
case class GameContinues(missionLeader: Nickname, missionNumber: Int, missions: Missions, nextMissionLeader: Nickname, votesLeft: Int) extends AfterQuest