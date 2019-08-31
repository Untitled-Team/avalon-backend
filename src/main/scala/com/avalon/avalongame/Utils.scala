package com.avalon.avalongame

import cats.data.StateT
import cats.effect.Sync
import cats.implicits._

import scala.util.control.NoStackTrace


case object TooFewOfPlayers extends RuntimeException with NoStackTrace

object Utils {

  //will have to use a config at some point
  def assignRoles[F[_]](users: List[User], shuffle: List[User] => F[List[User]])(implicit F: Sync[F]): F[Roles] = {

//    import scala.util.control.NoStackTrace
//
////    val shuffled: List[User] = scala.util.Random.shuffle(users)

    val players: StateT[F, List[User], Roles] =
      users.size match {
        case 5  =>
          for {
            assassin <- StateT[F, List[User], BadGuy] { s =>
              F.fromOption(s.headOption.map(u => Assassin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[User], BadGuy] { s =>
              F.fromOption(s.headOption.map(u => NormalBadGuy(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            merlin <- StateT[F, List[User], GoodGuy] { s =>
              F.fromOption(s.headOption.map(u => Merlin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[User], List[GoodGuy]] { s =>
              F.pure(s.take(2).map(u => NormalGoodGuy(u.nickname))).map( (Nil, _) )
            }
          } yield Roles(List(assassin, normalBadGuy), merlin :: goodGuys)

        case 6  =>
          for {
            assassin <- StateT[F, List[User], BadGuy] { s =>
              F.fromOption(s.headOption.map(u => Assassin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[User], BadGuy] { s =>
              F.fromOption(s.headOption.map(u => NormalBadGuy(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            merlin <- StateT[F, List[User], GoodGuy] { s =>
              F.fromOption(s.headOption.map(u => Merlin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[User], List[GoodGuy]] { s =>
              F.pure(s.take(3).map(u => NormalGoodGuy(u.nickname))).map( (Nil, _) )
            }
          } yield Roles(List(assassin, normalBadGuy), merlin :: goodGuys)
        case 7  =>
          for {
            assassin <- StateT[F, List[User], BadGuy] { s =>
              F.fromOption(s.headOption.map(u => Assassin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[User], List[BadGuy]] { s =>
              F.pure(s.take(2).map(u => NormalBadGuy(u.nickname))).map( (s.drop(2), _) )
            }
            merlin <- StateT[F, List[User], GoodGuy] { s =>
              F.fromOption(s.headOption.map(u => Merlin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[User], List[GoodGuy]] { s =>
              F.pure(s.take(3).map(u => NormalGoodGuy(u.nickname))).map( (Nil, _) )
            }
          } yield Roles(assassin :: normalBadGuy, merlin :: goodGuys)
        case 8  =>
          for {
            assassin <- StateT[F, List[User], BadGuy] { s =>
              F.fromOption(s.headOption.map(u => Assassin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[User], List[BadGuy]] { s =>
              F.pure(s.take(2).map(u => NormalBadGuy(u.nickname))).map( (s.drop(2), _) )
            }
            merlin <- StateT[F, List[User], GoodGuy] { s =>
              F.fromOption(s.headOption.map(u => Merlin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[User], List[GoodGuy]] { s =>
              F.pure(s.take(4).map(u => NormalGoodGuy(u.nickname))).map( (Nil, _) )
            }
          } yield Roles(assassin :: normalBadGuy, merlin :: goodGuys)
        case 9  =>
          for {
            assassin <- StateT[F, List[User], BadGuy] { s =>
              F.fromOption(s.headOption.map(u => Assassin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[User], List[BadGuy]] { s =>
              F.pure(s.take(2).map(u => NormalBadGuy(u.nickname))).map( (s.drop(2), _) )
            }
            merlin <- StateT[F, List[User], GoodGuy] { s =>
              F.fromOption(s.headOption.map(u => Merlin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[User], List[GoodGuy]] { s =>
              F.pure(s.take(5).map(u => NormalGoodGuy(u.nickname))).map( (Nil, _) )
            }
          } yield Roles(assassin :: normalBadGuy, merlin :: goodGuys)
        case 10 =>
          for {
            assassin <- StateT[F, List[User], BadGuy] { s =>
              F.fromOption(s.headOption.map(u => Assassin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[User], List[BadGuy]] { s =>
              F.pure(s.take(3).map(u => NormalBadGuy(u.nickname))).map( (s.drop(3), _) )
            }
            merlin <- StateT[F, List[User], GoodGuy] { s =>
              F.fromOption(s.headOption.map(u => Merlin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[User], List[GoodGuy]] { s =>
              F.pure(s.take(5).map(u => NormalGoodGuy(u.nickname))).map( (Nil, _) )
            }
          } yield Roles(assassin :: normalBadGuy, merlin :: goodGuys)
      }

    shuffle(users).flatMap(players.runA)
  }

  case class Roles(badGuys: List[BadGuy], goodGuys: List[GoodGuy])

}