package com.avalon.avalongame

import cats.data.{NonEmptyList, State, StateT}
import cats.effect._
import cats.effect.concurrent.{MVar, Ref}
import cats.effect.implicits._
import cats.implicits._
import fs2._

import scala.util.control.NoStackTrace

trait RoomManager[F[_]] {
  def create(roomId: RoomId, config: GameConfig): F[Unit]
  def get(roomId: RoomId): F[Room[F]]
//  def addUser(roomId: RoomId, user: User): F[Unit]
//  def userEvents(user: User): Stream[F, Event]
}

object RoomManager {
  //timeouts on get/reads?
  //could back this by mules and have them auto expire if not updated in a while
  def build[F[_]](randomAlg: RandomAlg[F])(implicit F: Concurrent[F]): F[RoomManager[F]] =
    Ref.of[F, Map[RoomId, Room[F]]](Map.empty).map { ref =>
      new RoomManager[F] {
        //should fail if the room already existed?
        def create(roomId: RoomId, config: GameConfig): F[Unit] = Room.build(randomAlg, roomId, config).flatMap { room =>
          ref.update(rooms => if (rooms.get(roomId).isEmpty) rooms + (roomId -> room) else rooms)
        }

        def get(chatId: RoomId): F[Room[F]] =
          for {
            rooms <- ref.get
            room <- F.fromOption(rooms.get(chatId), NoRoomFoundForChatId)
          } yield room
      }
    }
}

trait Room[F[_]] {
  def info: F[RoomInfo]
  def users: F[List[User]]
  def addUser(user: User): F[Unit]
}

object Room {

  case class InternalRoom(users: List[User], gameRepresentation: Option[GameRepresentation])
  //timeouts on get/reads?
  def build[F[_]](randomAlg: RandomAlg[F], roomId: RoomId, config: GameConfig)(implicit F: Concurrent[F]): F[Room[F]] =
    MVar.of(InternalRoom(Nil, None)).map { mvar =>
      new Room[F] {
        def info: F[RoomInfo] = users.map(RoomInfo(_, config))

        def users: F[List[User]] = mvar.read.map(_.users)

        def addUser(user: User): F[Unit] =
          mvar.take.flatMap(room => mvar.put(room.copy(user :: room.users)))

        def startGame: F[GameRepresentation] =
          mvar.take.flatMap { room =>
            (for {
              missions      <- F.fromEither(Missions.fromPlayers(room.users.size))
              roles         <- Utils.assignRoles(room.users, randomAlg.shuffle)
              missionLeader <- randomAlg.randomGet(room.users)
            } yield GameRepresentation(MissionProposing(1, missionLeader), missions, roles.badGuys, roles.goodGuys, room.users))
              .guaranteeCase {
                case ExitCase.Error(_) | ExitCase.Canceled => mvar.put(room)
                case ExitCase.Completed => F.unit
              }
          }
      }
    }
}

sealed abstract case class Mission(players: Option[List[User]], numberOfAdventurers: Int)

object Mission {
  def make(players: Option[List[User]], numberOfAdventurers: Int): Mission =
    new Mission(players, numberOfAdventurers){}
}

sealed abstract case class Missions(one: Mission,
                                    two: Mission,
                                    three: Mission,
                                    four: Mission,
                                    five: Mission)
object Missions {
  def fromPlayers(players: Int): Either[Throwable, Missions] = players match {
    case 5  =>
      Right(
        new Missions(
          Mission.make(None, 2),
          Mission.make(None, 3),
          Mission.make(None, 2),
          Mission.make(None, 3),
          Mission.make(None, 3)){})
    case 6  =>
      Right(
        new Missions(
          Mission.make(None, 2),
          Mission.make(None, 3),
          Mission.make(None, 4),
          Mission.make(None, 3),
          Mission.make(None, 4)){})
    case 7  =>
      Right(
        new Missions(
          Mission.make(None, 2),
          Mission.make(None, 3),
          Mission.make(None, 3),
          Mission.make(None, 4),
          Mission.make(None, 4)){})
    case 8 | 9 | 10  =>
      Right(
        new Missions(
          Mission.make(None, 3),
          Mission.make(None, 4),
          Mission.make(None, 4),
          Mission.make(None, 5),
          Mission.make(None, 5)){})
  }
}

sealed trait BadGuy
case class Assassin(nickname: Nickname) extends BadGuy
case class NormalBadGuy(nickname: Nickname) extends BadGuy

sealed trait GoodGuy
case class Merlin(nickname: Nickname) extends GoodGuy
case class NormalGoodGuy(nickname: Nickname) extends GoodGuy

case object NotEnoughPlayers extends RuntimeException with NoStackTrace

case class GameRepresentation(state: GameState,
                              missions: Missions,
                              badGuys: List[BadGuy],
                              goodGuys: List[GoodGuy],
                              users: List[User]) {
  //    def addBadGuys()
}

//  val missions =

sealed trait GameState
case object Lobby extends GameState
case class MissionProposing(missionNumber: Int, missionLeader: User) extends GameState
case class MissionProposed(voters: NonEmptyList[User]) extends GameState