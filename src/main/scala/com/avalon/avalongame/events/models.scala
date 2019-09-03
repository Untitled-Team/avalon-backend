package com.avalon.avalongame.events

import com.avalon.avalongame.common._
import com.avalon.avalongame.room._
import scala.util.control.NoStackTrace

sealed abstract case class CharacterRole(character: Role, badGuys: Option[List[Nickname]])

object CharacterRole {
  def fromRole(role: Role, badGuys: List[Nickname]): CharacterRole =
    role match {
      case Assassin      => new CharacterRole(Assassin, Some(badGuys)){}
      case NormalBadGuy  => new CharacterRole(NormalBadGuy, Some(badGuys)){}
      case Merlin        => new CharacterRole(Merlin, Some(badGuys)){}
      case NormalGoodGuy => new CharacterRole(NormalGoodGuy, None){}
    }
}

case class NoRoleForNickname(nickname: Nickname) extends RuntimeException with NoStackTrace