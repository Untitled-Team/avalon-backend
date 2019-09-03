package com.avalon.avalongame

import cats.data.StateT
import cats.effect.Sync
import cats.implicits._
import com.avalon.avalongame.events.{GameCreated, GameStarted}

import scala.util.control.NoStackTrace


case object TooFewOfPlayers extends RuntimeException with NoStackTrace

object Utils {

  case class NoRoleForNickname(nickname: Nickname) extends RuntimeException with NoStackTrace

  def representationToGameCreated[F[_]](nickname: Nickname, repr: GameRepresentation)(implicit F: Sync[F]): F[GameStarted] = {
    val playerRole: F[PlayerRole] = F.fromOption(
      repr.goodGuys.find(_.nickname === nickname) orElse repr.badGuys.find(_.nickname === nickname),
      NoRoleForNickname(nickname))


    playerRole.map { pr =>
      val charRole = CharacterRole.fromRole(pr.role, repr.badGuys.map(_.nickname))
      GameStarted(repr.state, repr.missions, charRole, repr.users)
    }
  }

  //will have to use a config at some point
  //this can be refactored for sure
  def assignRoles[F[_]](users: List[User], shuffle: List[User] => F[List[User]])(implicit F: Sync[F]): F[Roles] = {

    val players: StateT[F, List[User], Roles] =
      users.size match {
        case 5  =>
          for {
            assassin <- StateT[F, List[User], BadPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => BadPlayerRole(u.nickname, Assassin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[User], BadPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => BadPlayerRole(u.nickname, NormalBadGuy)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            merlin <- StateT[F, List[User], GoodPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => GoodPlayerRole(u.nickname, Merlin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[User], List[GoodPlayerRole]] { s =>
              F.pure(s.take(2).map(u => GoodPlayerRole(u.nickname, NormalGoodGuy))).map( (Nil, _) )
            }
          } yield Roles(List(assassin, normalBadGuy), merlin :: goodGuys)

        case 6  =>
          for {
            assassin <- StateT[F, List[User], BadPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => BadPlayerRole(u.nickname, Assassin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[User], BadPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => BadPlayerRole(u.nickname, NormalBadGuy)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            merlin <- StateT[F, List[User], GoodPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => GoodPlayerRole(u.nickname, Merlin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[User], List[GoodPlayerRole]] { s =>
              F.pure(s.take(3).map(u => GoodPlayerRole(u.nickname, NormalGoodGuy))).map( (Nil, _) )
            }
          } yield Roles(List(assassin, normalBadGuy), merlin :: goodGuys)
        case 7  =>
          for {
            assassin <- StateT[F, List[User], BadPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => BadPlayerRole(u.nickname, Assassin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[User], List[BadPlayerRole]] { s =>
              F.pure(s.take(2).map(u => BadPlayerRole(u.nickname, NormalBadGuy))).map( (s.drop(2), _) )
            }
            merlin <- StateT[F, List[User], GoodPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => GoodPlayerRole(u.nickname, Merlin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[User], List[GoodPlayerRole]] { s =>
              F.pure(s.take(3).map(u => GoodPlayerRole(u.nickname, NormalGoodGuy))).map( (Nil, _) )
            }
          } yield Roles(assassin :: normalBadGuy, merlin :: goodGuys)
        case 8  =>
          for {
            assassin <- StateT[F, List[User], BadPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => BadPlayerRole(u.nickname, Assassin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[User], List[BadPlayerRole]] { s =>
              F.pure(s.take(2).map(u => BadPlayerRole(u.nickname, NormalBadGuy))).map( (s.drop(2), _) )
            }
            merlin <- StateT[F, List[User], GoodPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => GoodPlayerRole(u.nickname, Merlin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[User], List[GoodPlayerRole]] { s =>
              F.pure(s.take(4).map(u => GoodPlayerRole(u.nickname, NormalGoodGuy))).map( (Nil, _) )
            }
          } yield Roles(assassin :: normalBadGuy, merlin :: goodGuys)
        case 9  =>
          for {
            assassin <- StateT[F, List[User], BadPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => BadPlayerRole(u.nickname, Assassin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[User], List[BadPlayerRole]] { s =>
              F.pure(s.take(2).map(u => BadPlayerRole(u.nickname, NormalBadGuy))).map( (s.drop(2), _) )
            }
            merlin <- StateT[F, List[User], GoodPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => GoodPlayerRole(u.nickname, Merlin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[User], List[GoodPlayerRole]] { s =>
              F.pure(s.take(5).map(u => GoodPlayerRole(u.nickname, NormalGoodGuy))).map( (Nil, _) )
            }
          } yield Roles(assassin :: normalBadGuy, merlin :: goodGuys)
        case 10 =>
          for {
            assassin <- StateT[F, List[User], BadPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => BadPlayerRole(u.nickname, Assassin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            normalBadGuy <- StateT[F, List[User], List[BadPlayerRole]] { s =>
              F.pure(s.take(3).map(u => BadPlayerRole(u.nickname, NormalBadGuy))).map( (s.drop(3), _) )
            }
            merlin <- StateT[F, List[User], GoodPlayerRole] { s =>
              F.fromOption(s.headOption.map(u => GoodPlayerRole(u.nickname, Merlin)), TooFewOfPlayers).map( (s.drop(1), _) )
            }
            goodGuys <- StateT[F, List[User], List[GoodPlayerRole]] { s =>
              F.pure(s.take(5).map(u => GoodPlayerRole(u.nickname, NormalGoodGuy))).map( (Nil, _) )
            }
          } yield Roles(assassin :: normalBadGuy, merlin :: goodGuys)
      }

    shuffle(users).flatMap(players.runA)
  }

  case class Roles(badGuys: List[BadPlayerRole], goodGuys: List[GoodPlayerRole])

}