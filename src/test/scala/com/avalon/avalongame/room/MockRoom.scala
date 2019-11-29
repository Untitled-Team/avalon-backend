package com.avalon.avalongame.room

import cats.effect.IO
import com.avalon.avalongame.common._

trait MockRoom extends Room[IO] {
  def players: IO[List[Nickname]] = ???
  def addUser(player: Nickname): IO[Unit] = ???
  def startGame: IO[StartGameInfo] = ???
  def proposeMission(nickname: Nickname, players: List[Nickname]): IO[MissionProposal] = ???
  def teamVote(nickname: Nickname, vote: TeamVote): IO[Either[GameOver, TeamVoteEnum]] = ???
  def questVote(nickname: Nickname, vote: QuestVote): IO[QuestVotingEnum] = ???
  def questResultsSeen(nickname: Nickname): IO[AfterQuest] = ???
  def assassinVote(assassin: Nickname, guess: Nickname): IO[GameOver] = ???
  def removePlayer(player: Nickname): IO[Unit] = ???
}