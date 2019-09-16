package com.avalon.avalongame.events

import cats.Eq
import cats.effect.concurrent.{Ref, Semaphore}
import cats.effect.{ContextShift, IO, Timer}
import com.avalon.avalongame.common._
import com.avalon.avalongame.RandomAlg
import com.mrdziuban.ScalacheckMagnolia._
import fs2.concurrent.Queue
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration._

class OutgoingManagerSpec extends WordSpec with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {

  implicit val cs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)
  implicit val t: Timer[IO] = IO.timer(scala.concurrent.ExecutionContext.Implicits.global)

  val mockRandomAlg: RandomAlg[IO] = new RandomAlg[IO] {
    override def shuffle[A](l: List[A]): IO[List[A]] = IO.pure(l)
    override def randomGet[A](l: List[A]): IO[A] = IO(l.head) //oops
    override def clockwise[A: Eq](previous: A, l: List[A]): IO[A] = IO(l.head)
  }

  "add" should {
    "correctly add a user to the manager with empty events" in {
      forAll { nickname: Nickname =>

        val sem: Semaphore[IO] = Semaphore[IO](1).unsafeRunSync()
        val ref: Ref[IO, List[OutgoingConnectionContext[IO]]] =
          Ref.of[IO, List[OutgoingConnectionContext[IO]]](Nil).unsafeRunSync()

        val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
        val outgoingManager: OutgoingManager[IO] =
          OutgoingManager.buildPrivate[IO](sem, ref)

        outgoingManager.add(nickname, userQueue).unsafeRunSync()

        ref.get.unsafeRunSync() should be(List(OutgoingConnectionContext(nickname, Some(userQueue), Nil)))
      }
    }
  }

  "remove" should {
    "correctly remove a user to the manager" in {
      forAll { nickname: Nickname =>

        val sem: Semaphore[IO] = Semaphore[IO](1).unsafeRunSync()
        val ref: Ref[IO, List[OutgoingConnectionContext[IO]]] =
          Ref.of[IO, List[OutgoingConnectionContext[IO]]](Nil).unsafeRunSync()

        val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
        val outgoingManager: OutgoingManager[IO] =
          OutgoingManager.buildPrivate[IO](sem, ref)

        outgoingManager.add(nickname, userQueue).unsafeRunSync()

        ref.get.unsafeRunSync() should be(List(OutgoingConnectionContext(nickname, Some(userQueue), Nil)))

        outgoingManager.remove(nickname).unsafeRunSync()

        ref.get.unsafeRunSync() should be(Nil)
      }
    }
  }

  "disconnected" should {
    "correctly set the disconnect events to Some(Nil)" in {
      forAll { nickname: Nickname =>

        val sem: Semaphore[IO] = Semaphore[IO](1).unsafeRunSync()
        val ref: Ref[IO, List[OutgoingConnectionContext[IO]]] =
          Ref.of[IO, List[OutgoingConnectionContext[IO]]](Nil).unsafeRunSync()

        val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
        val outgoingManager: OutgoingManager[IO] =
          OutgoingManager.buildPrivate[IO](sem, ref)

        outgoingManager.add(nickname, userQueue).unsafeRunSync()

        ref.get.unsafeRunSync() should be(List(OutgoingConnectionContext(nickname, Some(userQueue), Nil)))

        outgoingManager.disconnected(nickname).unsafeRunSync()

        ref.get.unsafeRunSync() should be(List(OutgoingConnectionContext(nickname, None, Nil, Some(Nil))))
      }
    }
  }

  "send" should { //one test for sending event _only_ to the user
    "correctly send event to user" in {
      forAll { nickname: Nickname =>

        val sem: Semaphore[IO] = Semaphore[IO](1).unsafeRunSync()
        val ref: Ref[IO, List[OutgoingConnectionContext[IO]]] =
          Ref.of[IO, List[OutgoingConnectionContext[IO]]](Nil).unsafeRunSync()

        val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
        val outgoingManager: OutgoingManager[IO] =
          OutgoingManager.buildPrivate[IO](sem, ref)

        outgoingManager.add(nickname, userQueue).unsafeRunSync()
        outgoingManager.send(nickname, PartyApproved).unsafeRunSync()

        val ctx = ref.get.unsafeRunSync().find(_.nickname === nickname).get
        ctx.events should be(List(PartyApproved))
        ctx.eventsSinceDisconnect should be(None)

        userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(PartyApproved)
      }
    }

    "correctly append events to list for the user's events" in {
      forAll { nickname: Nickname =>

        val sem: Semaphore[IO] = Semaphore[IO](1).unsafeRunSync()
        val ref: Ref[IO, List[OutgoingConnectionContext[IO]]] =
          Ref.of[IO, List[OutgoingConnectionContext[IO]]](Nil).unsafeRunSync()

        val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
        val outgoingManager: OutgoingManager[IO] =
          OutgoingManager.buildPrivate[IO](sem, ref)

        outgoingManager.add(nickname, userQueue).unsafeRunSync()
        outgoingManager.send(nickname, PartyApproved).unsafeRunSync()
        outgoingManager.send(nickname, PassFailVoteResults(3, 3)).unsafeRunSync()

        val ctx = ref.get.unsafeRunSync().find(_.nickname === nickname).get
        ctx.events should be(List(PartyApproved, PassFailVoteResults(3, 3)))
        ctx.eventsSinceDisconnect should be(None)

        userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(PartyApproved)
        userQueue.dequeue1.timeout(1 second).unsafeRunSync() should be(PassFailVoteResults(3, 3))
      }
    }

    "accumulate events for a disconnected user" in {
      forAll { nickname: Nickname =>

        val sem: Semaphore[IO] = Semaphore[IO](1).unsafeRunSync()
        val ref: Ref[IO, List[OutgoingConnectionContext[IO]]] =
          Ref.of[IO, List[OutgoingConnectionContext[IO]]](Nil).unsafeRunSync()

        val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
        val outgoingManager: OutgoingManager[IO] =
          OutgoingManager.buildPrivate[IO](sem, ref)

        outgoingManager.add(nickname, userQueue).unsafeRunSync()
        outgoingManager.disconnected(nickname).unsafeRunSync()

        outgoingManager.send(nickname, PartyApproved).unsafeRunSync()
        outgoingManager.send(nickname, PassFailVoteResults(3, 3)).unsafeRunSync()

        val ctx = ref.get.unsafeRunSync().find(_.nickname === nickname).get
        ctx.events should be(List(PartyApproved, PassFailVoteResults(3, 3)))
        ctx.eventsSinceDisconnect should be(Some(List(PartyApproved, PassFailVoteResults(3, 3))))
      }
    }
  }

  "broadcastUserSpecific" should {
    "send message to everyone but the nickname provided" in {
      forAll { roomId: RoomId =>

        val nickname = Nickname(java.util.UUID.randomUUID().toString)
        val nickname2 = Nickname(java.util.UUID.randomUUID().toString)

        val sem: Semaphore[IO] = Semaphore[IO](1).unsafeRunSync()
        val ref: Ref[IO, List[OutgoingConnectionContext[IO]]] =
          Ref.of[IO, List[OutgoingConnectionContext[IO]]](Nil).unsafeRunSync()

        val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
        val userQueue2 = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
        val outgoingManager: OutgoingManager[IO] =
          OutgoingManager.buildPrivate[IO](sem, ref)

        outgoingManager.add(nickname, userQueue).unsafeRunSync()
        outgoingManager.add(nickname2, userQueue2).unsafeRunSync()
        outgoingManager.broadcastUserSpecific(nickname, n => IO.pure(MoveToLobby(roomId, List(n)))).unsafeRunSync()

        val ctx = ref.get.unsafeRunSync().find(_.nickname === nickname).get
        val ctx2 = ref.get.unsafeRunSync().find(_.nickname === nickname2).get

        ctx.events should be(Nil)
        ctx2.events should be(List(MoveToLobby(roomId, List(nickname2))))
        ctx.eventsSinceDisconnect should be(None)
        ctx2.eventsSinceDisconnect should be(None)

        userQueue.tryDequeue1.unsafeRunSync() should be(None)
        userQueue2.dequeue1.timeout(1 second).unsafeRunSync() should be(MoveToLobby(roomId, List(nickname2)))
      }
    }
  }

  "sendToAllUserSpecific" should {
    "send message to everyone" in {
      forAll { roomId: RoomId =>

        val nickname = Nickname(java.util.UUID.randomUUID().toString)
        val nickname2 = Nickname(java.util.UUID.randomUUID().toString)

        val sem: Semaphore[IO] = Semaphore[IO](1).unsafeRunSync()
        val ref: Ref[IO, List[OutgoingConnectionContext[IO]]] =
          Ref.of[IO, List[OutgoingConnectionContext[IO]]](Nil).unsafeRunSync()

        val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
        val userQueue2 = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
        val outgoingManager: OutgoingManager[IO] =
          OutgoingManager.buildPrivate[IO](sem, ref)

        outgoingManager.add(nickname, userQueue).unsafeRunSync()
        outgoingManager.add(nickname2, userQueue2).unsafeRunSync()
        outgoingManager.sendToAllUserSpecific(n => IO.pure(MoveToLobby(roomId, List(n)))).unsafeRunSync()

        val ctx = ref.get.unsafeRunSync().find(_.nickname === nickname).get
        val ctx2 = ref.get.unsafeRunSync().find(_.nickname === nickname2).get

        ctx.events should be(List(MoveToLobby(roomId, List(nickname))))
        ctx2.events should be(List(MoveToLobby(roomId, List(nickname2))))
        ctx.eventsSinceDisconnect should be(None)
        ctx2.eventsSinceDisconnect should be(None)

        userQueue.tryDequeue1.unsafeRunSync() should be(Some(MoveToLobby(roomId, List(nickname))))
        userQueue2.tryDequeue1.unsafeRunSync() should be(Some(MoveToLobby(roomId, List(nickname2))))
      }
    }

    "accumulate messages for the users who are disconnected at the time of sending" in {
      forAll { roomId: RoomId =>

        val nickname = Nickname(java.util.UUID.randomUUID().toString)
        val nickname2 = Nickname(java.util.UUID.randomUUID().toString)

        val sem: Semaphore[IO] = Semaphore[IO](1).unsafeRunSync()
        val ref: Ref[IO, List[OutgoingConnectionContext[IO]]] =
          Ref.of[IO, List[OutgoingConnectionContext[IO]]](Nil).unsafeRunSync()

        val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
        val userQueue2 = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
        val outgoingManager: OutgoingManager[IO] =
          OutgoingManager.buildPrivate[IO](sem, ref)

        outgoingManager.add(nickname, userQueue).unsafeRunSync()
        outgoingManager.add(nickname2, userQueue2).unsafeRunSync()
        outgoingManager.disconnected(nickname).unsafeRunSync()
        outgoingManager.disconnected(nickname2).unsafeRunSync()

        outgoingManager.sendToAllUserSpecific(n => IO.pure(MoveToLobby(roomId, List(n)))).unsafeRunSync()

        val ctx = ref.get.unsafeRunSync().find(_.nickname === nickname).get
        val ctx2 = ref.get.unsafeRunSync().find(_.nickname === nickname2).get

        ctx.events should be(List(MoveToLobby(roomId, List(nickname))))
        ctx2.events should be(List(MoveToLobby(roomId, List(nickname2))))
        ctx.eventsSinceDisconnect should be(Some(List(MoveToLobby(roomId, List(nickname)))))
        ctx2.eventsSinceDisconnect should be(Some(List(MoveToLobby(roomId, List(nickname2)))))
      }
    }
  }

  "disconnect" should {
    "accumulate messages to be sent once user reconnects" in {
      forAll { roomId: RoomId =>

        val nickname = Nickname(java.util.UUID.randomUUID().toString)

        val sem: Semaphore[IO] = Semaphore[IO](1).unsafeRunSync()
        val ref: Ref[IO, List[OutgoingConnectionContext[IO]]] =
          Ref.of[IO, List[OutgoingConnectionContext[IO]]](Nil).unsafeRunSync()

        val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
        val outgoingManager: OutgoingManager[IO] =
          OutgoingManager.buildPrivate[IO](sem, ref)

        outgoingManager.add(nickname, userQueue).unsafeRunSync()
        outgoingManager.disconnected(nickname).unsafeRunSync()

        val ctx = ref.get.unsafeRunSync().find(_.nickname === nickname).get

        ctx.events should be(Nil)
        ctx.eventsSinceDisconnect should be(Some(Nil))
        ctx.respond should be(None)
      }
    }
  }

  "reconnect" should {
    "send all accumulated messages to the user" in {
      forAll { roomId: RoomId =>

        val nickname = Nickname(java.util.UUID.randomUUID().toString)

        val sem: Semaphore[IO] = Semaphore[IO](1).unsafeRunSync()
        val ref: Ref[IO, List[OutgoingConnectionContext[IO]]] =
          Ref.of[IO, List[OutgoingConnectionContext[IO]]](Nil).unsafeRunSync()

        val userQueue = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
        val userQueueReconnect = Queue.bounded[IO, OutgoingEvent](10).unsafeRunSync()
        val outgoingManager: OutgoingManager[IO] =
          OutgoingManager.buildPrivate[IO](sem, ref)

        outgoingManager.add(nickname, userQueue).unsafeRunSync()
        outgoingManager.disconnected(nickname).unsafeRunSync()
        outgoingManager.send(nickname, PartyApproved).unsafeRunSync()
        outgoingManager.send(nickname, PartyApproved).unsafeRunSync()
        outgoingManager.send(nickname, PartyApproved).unsafeRunSync()
        outgoingManager.send(nickname, PartyApproved).unsafeRunSync()

        outgoingManager.reconnect(nickname, userQueueReconnect).unsafeRunSync()

        val ctx = ref.get.unsafeRunSync().find(_.nickname === nickname).get

        ctx.events should be(List(PartyApproved, PartyApproved, PartyApproved, PartyApproved))
        ctx.eventsSinceDisconnect should be(None)

        userQueue.tryDequeue1.unsafeRunSync() should be(None)
        userQueueReconnect.tryDequeue1.unsafeRunSync() should be(Some(PartyApproved))
        userQueueReconnect.tryDequeue1.unsafeRunSync() should be(Some(PartyApproved))
        userQueueReconnect.tryDequeue1.unsafeRunSync() should be(Some(PartyApproved))
        userQueueReconnect.tryDequeue1.unsafeRunSync() should be(Some(PartyApproved))
      }
    }
  }
}