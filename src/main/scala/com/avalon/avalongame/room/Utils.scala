package com.avalon.avalongame.room

import cats.data.StateT
import cats.effect.Sync
import cats.implicits._
import com.avalon.avalongame.common._
import scala.util.control.NoStackTrace


case object TooFewOfPlayers extends RuntimeException with NoStackTrace

object Utils {

  case class Roles(badGuys: List[BadPlayerRole], goodGuys: List[GoodPlayerRole])
  private[this] case class NumberOfBadGuys(value: Int) extends AnyVal
  private[this] case class NumberOfGoodGuys(value: Int) extends AnyVal

  def getPlayers[F[_]](users: List[Nickname],
                       shuffle: List[Nickname] => F[List[Nickname]],
                       badGuysNumber: NumberOfBadGuys,
                       goodGuysWinner: NumberOfGoodGuys)(implicit F: Sync[F]): StateT[F, List[Nickname], Roles] =
    for {
      assassin <- StateT[F, List[Nickname], BadPlayerRole] { s =>
        F.fromOption(s.headOption.map(u => BadPlayerRole(u, Assassin)), TooFewOfPlayers).map( (s.drop(1), _) )
      }
      normalBadGuy <- StateT[F, List[Nickname], List[BadPlayerRole]] { s =>
        F.pure(s.take(badGuysNumber.value).map(u => BadPlayerRole(u, NormalBadGuy))).map( (s.drop(badGuysNumber.value), _) )
      }
      merlin <- StateT[F, List[Nickname], GoodPlayerRole] { s =>
        F.fromOption(s.headOption.map(u => GoodPlayerRole(u, Merlin)), TooFewOfPlayers).map( (s.drop(1), _) )
      }
      goodGuys <- StateT[F, List[Nickname], List[GoodPlayerRole]] { s =>
        F.pure(s.take(goodGuysWinner.value).map(u => GoodPlayerRole(u, NormalGoodGuy))).map( (Nil, _) )
      }
    } yield Roles(List(assassin) ::: normalBadGuy, merlin :: goodGuys)

  //will have to use a config at some point
  def assignRoles[F[_]](users: List[Nickname], shuffle: List[Nickname] => F[List[Nickname]])(implicit F: Sync[F]): F[Roles] = {

    val players: StateT[F, List[Nickname], Roles] =
      users.size match {
        case 5  =>
          getPlayers(users, shuffle, NumberOfBadGuys(1), NumberOfGoodGuys(2))
        case 6  =>
          getPlayers(users, shuffle, NumberOfBadGuys(1), NumberOfGoodGuys(3))
        case 7  =>
          getPlayers(users, shuffle, NumberOfBadGuys(2), NumberOfGoodGuys(3))
        case 8  =>
          getPlayers(users, shuffle, NumberOfBadGuys(2), NumberOfGoodGuys(4))
        case 9  =>
          getPlayers(users, shuffle, NumberOfBadGuys(2), NumberOfGoodGuys(5))
        case 10 =>
          getPlayers(users, shuffle, NumberOfBadGuys(3), NumberOfGoodGuys(5))
      }

    shuffle(users).flatMap(players.runA)
  }
}