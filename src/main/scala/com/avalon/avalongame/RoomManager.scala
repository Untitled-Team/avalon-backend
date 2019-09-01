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
  def startGame: F[GameRepresentation]
}


case class NicknameAlreadyInUse(nickname: Nickname) extends RuntimeException with NoStackTrace
object Room {

  case class InternalRoom(users: List[User], gameRepresentation: Option[GameRepresentation])
  //timeouts on get/reads?
  def build[F[_]](randomAlg: RandomAlg[F], roomId: RoomId, config: GameConfig)(implicit F: Concurrent[F]): F[Room[F]] =
    MVar.of(InternalRoom(Nil, None)).map { mvar =>
      new Room[F] {
        def info: F[RoomInfo] = users.map(RoomInfo(_, config))

        def users: F[List[User]] = mvar.read.map(_.users)

        //need to enforce that this is only possible in certain states!!!!!
        def addUser(user: User): F[Unit] =
          mvar.take.flatMap { room =>
            if (room.users.map(_.nickname).contains(user.nickname))
              mvar.put(room) >> F.raiseError(NicknameAlreadyInUse(user.nickname))
            else
              mvar.put(room.copy(room.users :+ user))
          }

        def startGame: F[GameRepresentation] =
          mvar.take.flatMap { room =>
            (for {
              missions      <- F.fromEither(Missions.fromPlayers(room.users.size))
              roles         <- Utils.assignRoles(room.users, randomAlg.shuffle)
              missionLeader <- randomAlg.randomGet(room.users)
              repr          =  GameRepresentation(MissionProposing(1, missionLeader), missions, roles.badGuys, roles.goodGuys, room.users)
              _             <- mvar.put(room.copy(gameRepresentation = Some(repr)))
            } yield repr)
              .guaranteeCase {
                case ExitCase.Error(_) | ExitCase.Canceled => mvar.put(room)
                case ExitCase.Completed => F.unit
              }
          }
      }
    }
}



//  val missions =

