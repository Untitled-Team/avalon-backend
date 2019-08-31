package com.avalon.avalongame

import cats.data.StateT
import cats.effect.Sync
import cats.implicits._

import scala.util.control.NoStackTrace


case object TooFewOfPlayers extends RuntimeException with NoStackTrace

object Utils {

  def generateGoodGuysAndBadGuys[F[_]](users: List[User])(implicit F: Sync[F]): F[Players] = {

    import scala.util.control.NoStackTrace

    val shuffled: List[User] = scala.util.Random.shuffle(users)

    val players: StateT[F, List[User], Players] =
      users.size match {
        case 5  =>
          for {
            assassin <- StateT[F, List[User], BadGuy] { s =>
              F.fromOption(users.headOption.map(u => Assassin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[User], BadGuy] { s =>
              F.fromOption(users.headOption.map(u => NormalBadGuy(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            merlin <- StateT[F, List[User], GoodGuy] { s =>
              F.fromOption(users.headOption.map(u => Merlin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[User], List[GoodGuy]] { _ =>
              F.pure(users.take(2).map(u => NormalGoodGuy(u.nickname))).map( (Nil, _) )
            }
          } yield Players(List(assassin, normalBadGuy), merlin :: goodGuys)

        case 6  =>
          for {
            assassin <- StateT[F, List[User], BadGuy] { s =>
              F.fromOption(users.headOption.map(u => Assassin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[User], BadGuy] { s =>
              F.fromOption(users.headOption.map(u => NormalBadGuy(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            merlin <- StateT[F, List[User], GoodGuy] { s =>
              F.fromOption(users.headOption.map(u => Merlin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[User], List[GoodGuy]] { _ =>
              F.pure(users.take(3).map(u => NormalGoodGuy(u.nickname))).map( (Nil, _) )
            }
          } yield Players(List(assassin, normalBadGuy), merlin :: goodGuys)
        case 7  =>
          for {
            assassin <- StateT[F, List[User], BadGuy] { s =>
              F.fromOption(users.headOption.map(u => Assassin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[User], List[BadGuy]] { s =>
              F.pure(users.take(2).map(u => NormalBadGuy(u.nickname))).map( (s.drop(1), _) )
            }
            merlin <- StateT[F, List[User], GoodGuy] { s =>
              F.fromOption(users.headOption.map(u => Merlin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[User], List[GoodGuy]] { _ =>
              F.pure(users.take(3).map(u => NormalGoodGuy(u.nickname))).map( (Nil, _) )
            }
          } yield Players(assassin :: normalBadGuy, merlin :: goodGuys)
        case 8  =>
          for {
            assassin <- StateT[F, List[User], BadGuy] { s =>
              F.fromOption(users.headOption.map(u => Assassin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[User], List[BadGuy]] { s =>
              F.pure(users.take(2).map(u => NormalBadGuy(u.nickname))).map( (s.drop(1), _) )
            }
            merlin <- StateT[F, List[User], GoodGuy] { s =>
              F.fromOption(users.headOption.map(u => Merlin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[User], List[GoodGuy]] { _ =>
              F.pure(users.take(4).map(u => NormalGoodGuy(u.nickname))).map( (Nil, _) )
            }
          } yield Players(assassin :: normalBadGuy, merlin :: goodGuys)
        case 9  =>
          for {
            assassin <- StateT[F, List[User], BadGuy] { s =>
              F.fromOption(users.headOption.map(u => Assassin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[User], List[BadGuy]] { s =>
              F.pure(users.take(2).map(u => NormalBadGuy(u.nickname))).map( (s.drop(1), _) )
            }
            merlin <- StateT[F, List[User], GoodGuy] { s =>
              F.fromOption(users.headOption.map(u => Merlin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[User], List[GoodGuy]] { _ =>
              F.pure(users.take(5).map(u => NormalGoodGuy(u.nickname))).map( (Nil, _) )
            }
          } yield Players(assassin :: normalBadGuy, merlin :: goodGuys)
        case 10 =>
          for {
            assassin <- StateT[F, List[User], BadGuy] { s =>
              F.fromOption(users.headOption.map(u => Assassin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[User], List[BadGuy]] { s =>
              F.pure(users.take(3).map(u => NormalBadGuy(u.nickname))).map( (s.drop(1), _) )
            }
            merlin <- StateT[F, List[User], GoodGuy] { s =>
              F.fromOption(users.headOption.map(u => Merlin(u.nickname)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[User], List[GoodGuy]] { _ =>
              F.pure(users.take(5).map(u => NormalGoodGuy(u.nickname))).map( (Nil, _) )
            }
          } yield Players(assassin :: normalBadGuy, merlin :: goodGuys)
      }

    players.runA(shuffled)
  }

  case class Players(badGuys: List[BadGuy], goodGuys: List[GoodGuy])

}