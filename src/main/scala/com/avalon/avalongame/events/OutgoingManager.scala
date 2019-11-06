package com.avalon.avalongame.events

import cats.implicits._
import cats.effect._
import cats.effect.concurrent.{Ref, Semaphore}
import cats.temp.par._
import com.avalon.avalongame.common._
import io.chrisdavenport.fuuid._
import fs2.concurrent.Queue

trait OutgoingManager[F[_]] {
  def add(nickname: Nickname, respond: Queue[F, OutgoingEvent]): F[Unit]
  def remove(nickname: Nickname): F[Unit]
  def send(nickname: Nickname, outgoingEvent: OutgoingEvent): F[Unit]
  def disconnected(nickname: Nickname): F[Unit]
  def reconnect(nickname: Nickname, lastMessageId: FUUID, respond: Queue[F, OutgoingEvent]): F[Unit]
  def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): F[Unit]
  def broadcastUserSpecific(nickname: Nickname, outgoingF: Nickname => F[OutgoingEvent]): F[Unit]
  def sendToAll(event: OutgoingEvent): F[Unit]
  def sendToAllUserSpecific(outgoingF: Nickname => F[OutgoingEvent]): F[Unit]
}

object OutgoingManager {
  private[events] def buildPrivate[F[_]: Par](sem: Semaphore[F],
                                              ref: Ref[F, List[OutgoingConnectionContext[F]]])(implicit F: Concurrent[F]): OutgoingManager[F] =
    new OutgoingManager[F] {
      def add(nickname: Nickname, respond: Queue[F, OutgoingEvent]): F[Unit] =
        sem.withPermit(ref.update(OutgoingConnectionContext(nickname, Some(respond), Nil) :: _))

      def remove(nickname: Nickname): F[Unit] =
        sem.withPermit(ref.update(_.filter(_.nickname =!= nickname)))

      def disconnected(nickname: Nickname): F[Unit] =
        sem.withPermit {
          ref.update(
            _.map(ctx =>
              if (nickname === ctx.nickname) ctx.copy(eventsSinceDisconnect = Some(Nil), respond = Option.empty[Queue[F, OutgoingEvent]]) else ctx))
        }

      def reconnect(nickname: Nickname, lastMessageId: FUUID, respond: Queue[F, OutgoingEvent]): F[Unit] =
        sem.withPermit {
          for {
            connections <- ref.get
            c <- F.fromOption(connections.find(_.nickname === nickname), NoOutgoingEventContextExistsForUser(nickname))
            eventsToSend = c.events.dropWhile(_.id =!= lastMessageId).drop(1)

            //modify here and fail if we couldn't update?
            _ <- ref.update(_.map(ctx => if (nickname === ctx.nickname) ctx.copy(respond = Some(respond), eventsSinceDisconnect = None) else ctx))
            _ <- eventsToSend.parTraverse_(event => respond.enqueue1(event))
          } yield {}
        }

      def send(nickname: Nickname, outgoingEvent: OutgoingEvent): F[Unit] =
        sem.withPermit {
          for {
            connections <- ref.get
            c <- F.fromOption(connections.find(_.nickname === nickname), NoOutgoingEventContextExistsForUser(nickname))
            _ <- updateContext(nickname, ref, outgoingEvent)
            _ <- c.respond.traverse_(_.enqueue1(outgoingEvent))
          } yield ()
        }

      def broadcast(nickname: Nickname, outgoingEvent: OutgoingEvent): F[Unit] =
        broadcastUserSpecific(nickname, _ => F.pure(outgoingEvent))

      def broadcastUserSpecific(nickname: Nickname, outgoingF: Nickname => F[OutgoingEvent]): F[Unit] =
        sem.withPermit {
          for {
            l <- ref.get
            _ <- l.filter(_.nickname =!= nickname).parTraverse { u =>
              outgoingF(u.nickname)
                .flatTap(event => u.respond.traverse_(_.enqueue1(event)))
                .flatTap(outgoingEvent => updateContext(u.nickname, ref, outgoingEvent))
            }
          } yield ()
        }

      def sendToAll(event: OutgoingEvent): F[Unit] = sendToAllUserSpecific(_ => F.pure(event))

      def sendToAllUserSpecific(outgoingF: Nickname => F[OutgoingEvent]): F[Unit] =
        sem.withPermit {
          ref.get.flatMap(_.parTraverse_ { u =>
            outgoingF(u.nickname)
              .flatTap(event => u.respond.traverse_(_.enqueue1(event)))
              .flatTap(event => updateContext(u.nickname, ref, event))
          })
        }
    }

  def updateContext[F[_]: Sync](nickname: Nickname, ref: Ref[F, List[OutgoingConnectionContext[F]]], outgoingEvent: OutgoingEvent): F[Unit] =
    ref.update(_.map { ctx =>
      if (nickname === ctx.nickname) {
        val disconnectEvents = ctx.eventsSinceDisconnect.map(_ :+ outgoingEvent)
        ctx.copy(events = ctx.events :+ outgoingEvent, eventsSinceDisconnect = disconnectEvents)
      }
      else ctx
    })

  //don't really need to provide the usernameWithSend when we build. Would make mocking easier if we don't
  def build[F[_]: Par](implicit F: Concurrent[F]): F[OutgoingManager[F]] =
    Semaphore[F](1).flatMap { sem =>
      Ref.of[F, List[OutgoingConnectionContext[F]]](Nil).map { ref =>
        buildPrivate(sem, ref)
      }
    }
}