package com.avalon.avalongame.events

import cats.Eq
import cats.effect.{ContextShift, IO, Sync}
import com.avalon.avalongame.RandomAlg
import com.avalon.avalongame.common.GameConfig.{MorganaConfig, OberonConfig}
import com.avalon.avalongame.common._
import com.avalon.avalongame.room._
import io.chrisdavenport.fuuid.FUUID
import org.scalatest.{FunSuite, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ModelsSpec extends FunSuite with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {

  implicit val cs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)

  val merlin = GoodPlayerRole(Nickname("merlin"), Merlin)
  val assassin = BadPlayerRole(Nickname("BadGuy"), Assassin)
  val badGuys = List(assassin)

  val mockRandomAlg: RandomAlg[IO] = new RandomAlg[IO] {
    override def shuffle[A](l: List[A]): IO[List[A]] = IO.pure(l)
    override def randomGet[A](l: List[A]): IO[A] = IO(l.head) //oops
    override def clockwise[A: Eq](previous: A, l: List[A]): IO[A] = IO(l.head)
    def fuuid: IO[FUUID] = ???
  }

  test("CharacterRole should not list bad guys for NormalGoodGuy") {
    val role = CharacterRole.fromRole(NormalGoodGuy, badGuys, merlin, None)

    role.badGuys should be(None)
    role.character should be(NormalGoodGuy)
    role.merlin should be(None)
  }

}