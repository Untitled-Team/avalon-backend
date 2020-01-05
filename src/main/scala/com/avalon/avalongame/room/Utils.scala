package com.avalon.avalongame.room

import cats.data.StateT
import cats.effect.Sync
import cats.implicits._
import com.avalon.avalongame.common._
import scala.util.control.NoStackTrace


case object TooFewOfPlayers extends RuntimeException with NoStackTrace

object Utils {

  case class Roles(badGuys: List[BadPlayerRole], goodGuys: List[GoodPlayerRole])
  private[room] case class NumberOfBadGuys(value: Int) extends AnyVal
  private[room] case class NumberOfGoodGuys(value: Int) extends AnyVal

  private[room] case class PlayerRoleState(users: List[Nickname], badGuysNumber: NumberOfBadGuys, goodGuys: NumberOfGoodGuys)

  private[room] def getPlayers[F[_]](gc: GameConfig)(implicit F: Sync[F]): StateT[F, PlayerRoleState, Roles] =
    for {
      assassin <- pickBadSpecialCharacter(Assassin)
      oberon <-
        if (gc.oberon.value)
          pickOptionalBadSpecialCharacter(Oberon)
        else StateT[F, PlayerRoleState, Option[BadPlayerRole]](s => F.pure(s, None))

      morgana <-
        if (gc.morgana.value)
          pickOptionalBadSpecialCharacter(Morgana)
        else StateT[F, PlayerRoleState, Option[BadPlayerRole]](s => F.pure(s, None))

      mordred <-
        if (gc.mordred.value)
          pickOptionalBadSpecialCharacter(Mordred)
        else StateT[F, PlayerRoleState, Option[BadPlayerRole]](s => F.pure(s, None))

      normalBadGuy <- StateT[F, PlayerRoleState, List[BadPlayerRole]] { s =>
        F.pure(s.users.take(s.badGuysNumber.value).map(u => BadPlayerRole(u, NormalBadGuy)))
          .map(bprs => (s.copy(users = s.users.drop(s.badGuysNumber.value), badGuysNumber = NumberOfBadGuys(0)), bprs) )
      }

      merlin <- pickGoodSpecialCharacter(Merlin)

      percival <-
        if (gc.percival.value)
          pickOptionalGoodSpecialCharacter(Percival)
        else StateT[F, PlayerRoleState, Option[GoodPlayerRole]](s => F.pure(s, None))

      goodGuys <- StateT[F, PlayerRoleState, List[GoodPlayerRole]] { s =>
        F.pure(s.users.take(s.goodGuys.value).map(u => GoodPlayerRole(u, NormalGoodGuy)))
          .map(gprs => (s.copy(users = s.users.drop(s.goodGuys.value), goodGuys = NumberOfGoodGuys(0)), gprs) )
      }
    } yield Roles(
      List(Some(assassin), oberon, morgana, mordred).flatten ::: normalBadGuy,
      List(Some(merlin), percival).flatten ::: goodGuys)

  def pickBadSpecialCharacter[F[_]](badGuy: BadGuy)(implicit F: Sync[F]): StateT[F, PlayerRoleState, BadPlayerRole] =
    StateT[F, PlayerRoleState, BadPlayerRole] { s =>
      F.fromOption(s.users.headOption.map(u => BadPlayerRole(u, badGuy)), TooFewOfPlayers)
        .map(bpr => (s.copy(users = s.users.drop(1), badGuysNumber = NumberOfBadGuys(s.badGuysNumber.value - 1)), bpr))
    }

  def pickOptionalBadSpecialCharacter[F[_]](badGuy: BadGuy)(implicit F: Sync[F]): StateT[F, PlayerRoleState, Option[BadPlayerRole]] =
    StateT[F, PlayerRoleState, Option[BadPlayerRole]] { s =>
      if (s.badGuysNumber.value > 0)
        F.fromOption(s.users.headOption.map(u => BadPlayerRole(u, badGuy)), TooFewOfPlayers)
          .map(bpr => (s.copy(users = s.users.drop(1), badGuysNumber = NumberOfBadGuys(s.badGuysNumber.value - 1)), Some(bpr)))
      else F.pure( (s, None) )
    }

  def pickGoodSpecialCharacter[F[_]](goodGuy: GoodGuy)(implicit F: Sync[F]): StateT[F, PlayerRoleState, GoodPlayerRole] =
    StateT[F, PlayerRoleState, GoodPlayerRole] { s =>
      F.fromOption(s.users.headOption.map(u => GoodPlayerRole(u, goodGuy)), TooFewOfPlayers)
        .map(gpr => (s.copy(users = s.users.drop(1), goodGuys = NumberOfGoodGuys(s.goodGuys.value - 1)), gpr))
    }

  def pickOptionalGoodSpecialCharacter[F[_]](goodGuy: GoodGuy)(implicit F: Sync[F]): StateT[F, PlayerRoleState, Option[GoodPlayerRole]] =
    StateT[F, PlayerRoleState, Option[GoodPlayerRole]] { s =>
      if (s.goodGuys.value > 0)
        F.fromOption(s.users.headOption.map(u => GoodPlayerRole(u, goodGuy)), TooFewOfPlayers)
          .map(gpr => (s.copy(users = s.users.drop(1), goodGuys = NumberOfGoodGuys(s.goodGuys.value - 1)), Some(gpr)))
      else F.pure( (s, None) )
    }

  private [room] def players[F[_]](users: List[Nickname], gc: GameConfig)(implicit F: Sync[F]): F[PlayerRoleState] =
    users.size match {
      case 5  =>
        F.pure(PlayerRoleState(users, NumberOfBadGuys(2), NumberOfGoodGuys(3)))
      case 6  =>
        F.pure(PlayerRoleState(users, NumberOfBadGuys(2), NumberOfGoodGuys(4)))
      case 7  =>
        F.pure(PlayerRoleState(users, NumberOfBadGuys(3), NumberOfGoodGuys(4)))
      case 8  =>
        F.pure(PlayerRoleState(users, NumberOfBadGuys(3), NumberOfGoodGuys(5)))
      case 9  =>
        F.pure(PlayerRoleState(users, NumberOfBadGuys(3), NumberOfGoodGuys(6)))
      case 10 =>
        F.pure(PlayerRoleState(users, NumberOfBadGuys(4), NumberOfGoodGuys(6)))
    }

  def ensureValidConfig[F[_]](gameConfig: GameConfig,
                              userCount: Int,
                              badGuysNumber: NumberOfBadGuys,
                              goodGuys: NumberOfGoodGuys)(implicit F: Sync[F]): F[Unit] = {
    val oberon = if (gameConfig.oberon.value) 1 else 0
    val morgana = if (gameConfig.morgana.value) 1 else 0
    val mordred = if (gameConfig.mordred.value) 1 else 0

    val percival = if (gameConfig.percival.value) 1 else 0

    val totalBadGuys = oberon + morgana + mordred + 1 //assassin

    val totalSpecialCharacters = oberon + morgana + mordred + percival + 2 //assassin + merlin

    for {
      _ <- if (totalBadGuys > badGuysNumber.value) F.raiseError(TooManyBadSpecialCharacters) else F.unit
      _ <- if (totalSpecialCharacters > userCount) F.raiseError(TooManySpecialCharacters) else F.unit //maybe unnecessary
    } yield ()
  }


  //will have to use a config at some point
  def assignRoles[F[_]](users: List[Nickname],
                        gc: GameConfig,
                        shuffle: List[Nickname] => F[List[Nickname]])(implicit F: Sync[F]): F[Roles] =
    for {
      shuffled <- shuffle(users)
      prs      <- players(shuffled, gc)
      _        <- ensureValidConfig(gc, users.size, prs.badGuysNumber, prs.goodGuys)
      run      <- getPlayers(gc).runA(prs)
    } yield run
}