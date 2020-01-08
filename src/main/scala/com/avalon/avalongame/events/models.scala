package com.avalon.avalongame.events

import com.avalon.avalongame.common._
import com.avalon.avalongame.room._
import scala.util.control.NoStackTrace

sealed abstract case class CharacterRole(character: Role, badGuys: Option[List[BadPlayerRole]], merlin: Option[List[PlayerRole]] = None)

object CharacterRole {
  def fromRole(role: Role, badGuys: List[BadPlayerRole], merlin: GoodPlayerRole, morgana: Option[BadPlayerRole]): CharacterRole = {
    val withoutOberon = badGuys.filterNot(_.role == Oberon)
    val withoutMordred = badGuys.filterNot(_.role == Mordred)

    role match {
      case Assassin      => new CharacterRole(Assassin, Some(withoutOberon)){}
      case Oberon        => new CharacterRole(Oberon, None){}
      case Morgana       => new CharacterRole(Morgana, Some(withoutOberon)){}
      case Mordred       => new CharacterRole(Mordred, Some(withoutOberon)){}
      case NormalBadGuy  => new CharacterRole(NormalBadGuy, Some(withoutOberon)){}
      case Merlin        => new CharacterRole(Merlin, Some(withoutMordred)){}
      case Percival      => new CharacterRole(Percival, None, Some(List(Some(merlin), morgana).flatten)){}
      case NormalGoodGuy => new CharacterRole(NormalGoodGuy, None){}
    }
  }
}

case class NicknameNotFoundInRoom(nickname: Nickname) extends RuntimeException with NoStackTrace
case object ContextExistsAlready extends RuntimeException with NoStackTrace
case class NoRoleForNickname(nickname: Nickname) extends RuntimeException with NoStackTrace
case class NoOutgoingEventContextExistsForUser(nickname: Nickname) extends RuntimeException with NoStackTrace