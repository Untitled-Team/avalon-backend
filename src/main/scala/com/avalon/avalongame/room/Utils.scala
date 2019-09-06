package com.avalon.avalongame.room

import cats.data.StateT
import cats.effect.Sync
import cats.implicits._
import com.avalon.avalongame.common._
//import com.avalon.avalongame.events.GameStarted

import scala.util.control.NoStackTrace


case object TooFewOfPlayers extends RuntimeException with NoStackTrace

object Utils {

  //will have to use a config at some point
  //this can be refactored for sure
  def assignRoles[F[_]](users: List[Nickname], shuffle: List[Nickname] => F[List[Nickname]])(implicit F: Sync[F]): F[Roles] = {

    val players: StateT[F, List[Nickname], Roles] =
      users.size match {
        case 5  =>
          for {
            assassin <- StateT[F, List[Nickname], BadPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => BadPlayerRole(u, Assassin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[Nickname], BadPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => BadPlayerRole(u, NormalBadGuy)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            merlin <- StateT[F, List[Nickname], GoodPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => GoodPlayerRole(u, Merlin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[Nickname], List[GoodPlayerRole]] { s =>
              F.pure(s.take(2).map(u => GoodPlayerRole(u, NormalGoodGuy))).map( (Nil, _) )
            }
          } yield Roles(List(assassin, normalBadGuy), merlin :: goodGuys)

        case 6  =>
          for {
            assassin <- StateT[F, List[Nickname], BadPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => BadPlayerRole(u, Assassin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[Nickname], BadPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => BadPlayerRole(u, NormalBadGuy)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            merlin <- StateT[F, List[Nickname], GoodPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => GoodPlayerRole(u, Merlin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[Nickname], List[GoodPlayerRole]] { s =>
              F.pure(s.take(3).map(u => GoodPlayerRole(u, NormalGoodGuy))).map( (Nil, _) )
            }
          } yield Roles(List(assassin, normalBadGuy), merlin :: goodGuys)
        case 7  =>
          for {
            assassin <- StateT[F, List[Nickname], BadPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => BadPlayerRole(u, Assassin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[Nickname], List[BadPlayerRole]] { s =>
              F.pure(s.take(2).map(u => BadPlayerRole(u, NormalBadGuy))).map( (s.drop(2), _) )
            }
            merlin <- StateT[F, List[Nickname], GoodPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => GoodPlayerRole(u, Merlin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[Nickname], List[GoodPlayerRole]] { s =>
              F.pure(s.take(3).map(u => GoodPlayerRole(u, NormalGoodGuy))).map( (Nil, _) )
            }
          } yield Roles(assassin :: normalBadGuy, merlin :: goodGuys)
        case 8  =>
          for {
            assassin <- StateT[F, List[Nickname], BadPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => BadPlayerRole(u, Assassin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[Nickname], List[BadPlayerRole]] { s =>
              F.pure(s.take(2).map(u => BadPlayerRole(u, NormalBadGuy))).map( (s.drop(2), _) )
            }
            merlin <- StateT[F, List[Nickname], GoodPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => GoodPlayerRole(u, Merlin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[Nickname], List[GoodPlayerRole]] { s =>
              F.pure(s.take(4).map(u => GoodPlayerRole(u, NormalGoodGuy))).map( (Nil, _) )
            }
          } yield Roles(assassin :: normalBadGuy, merlin :: goodGuys)
        case 9  =>
          for {
            assassin <- StateT[F, List[Nickname], BadPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => BadPlayerRole(u, Assassin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[Nickname], List[BadPlayerRole]] { s =>
              F.pure(s.take(2).map(u => BadPlayerRole(u, NormalBadGuy))).map( (s.drop(2), _) )
            }
            merlin <- StateT[F, List[Nickname], GoodPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => GoodPlayerRole(u, Merlin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[Nickname], List[GoodPlayerRole]] { s =>
              F.pure(s.take(5).map(u => GoodPlayerRole(u, NormalGoodGuy))).map( (Nil, _) )
            }
          } yield Roles(assassin :: normalBadGuy, merlin :: goodGuys)
        case 10 =>
          for {
            assassin <- StateT[F, List[Nickname], BadPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => BadPlayerRole(u, Assassin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[Nickname], List[BadPlayerRole]] { s =>
              F.pure(s.take(3).map(u => BadPlayerRole(u, NormalBadGuy))).map( (s.drop(3), _) )
            }
            merlin <- StateT[F, List[Nickname], GoodPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => GoodPlayerRole(u, Merlin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[Nickname], List[GoodPlayerRole]] { s =>
              F.pure(s.take(5).map(u => GoodPlayerRole(u, NormalGoodGuy))).map( (Nil, _) )
            }
          } yield Roles(assassin :: normalBadGuy, merlin :: goodGuys)
      }

    shuffle(users).flatMap(players.runA)
  }

  case class Roles(badGuys: List[BadPlayerRole], goodGuys: List[GoodPlayerRole])

}