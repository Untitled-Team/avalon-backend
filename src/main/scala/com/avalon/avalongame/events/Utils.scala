package com.avalon.avalongame.events

import cats.effect._
import cats.implicits._
import com.avalon.avalongame.RandomAlg
import com.avalon.avalongame.common._
import com.avalon.avalongame.room._

object Utils {
//  def representationToGameCreated[F[_]](nickname: Nickname, repr: GameRepresentation)(implicit F: Sync[F]): F[GameStarted] = {
//    val playerRole: F[PlayerRole] = F.fromOption(
//      repr.goodGuys.find(_.nickname === nickname) orElse repr.badGuys.find(_.nickname === nickname),
//      NoRoleForNickname(nickname))
//
//
//    playerRole.map { pr =>
//      val charRole = CharacterRole.fromRole(pr.role, repr.badGuys.map(_.nickname))
//      GameStarted(repr.state, repr.missions, charRole, repr.users)
//    }
//  }

  def playerRole[F[_]: RandomAlg](nickname: Nickname, repr: AllPlayerRoles)(implicit F: Sync[F]): F[PlayerInfo] = {
    val playerRole = F.fromOption(
      repr.goodGuys.find(_.nickname === nickname) orElse repr.badGuys.find(_.nickname === nickname),
      NoRoleForNickname(nickname))

    playerRole.flatMap { pr =>
      val charRole = CharacterRole.fromRole(pr.role, repr.badGuys)

      PlayerInfo.make(charRole.character, charRole.badGuys)
    }
  }
}
