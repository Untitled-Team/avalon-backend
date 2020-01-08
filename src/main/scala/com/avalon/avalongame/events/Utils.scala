package com.avalon.avalongame.events

import cats.effect._
import cats.implicits._
import com.avalon.avalongame.RandomAlg
import com.avalon.avalongame.common._
import com.avalon.avalongame.room._

object Utils {
  def playerRole[F[_]: RandomAlg](nickname: Nickname, repr: AllPlayerRoles)(implicit F: Sync[F]): F[PlayerInfo] =
    for {
      pr <- F.fromOption(
        repr.goodGuys.find(_.nickname === nickname) orElse repr.badGuys.find(_.nickname === nickname),
        NoRoleForNickname(nickname))
      merlin   <- F.fromOption(repr.goodGuys.find(_.role == Merlin), new RuntimeException("No Merlin found"))
      morgana  =  repr.badGuys.find(_.role == Morgana)
      charRole =  CharacterRole.fromRole(pr.role, repr.badGuys, merlin, morgana)
      result   <- PlayerInfo.make(charRole.character, charRole.badGuys, charRole.merlin)
    } yield result
}
