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
  val oberon = BadPlayerRole(Nickname("Oberon"), Oberon)
  val morgana = BadPlayerRole(Nickname("Morgana"), Morgana)
  val mordred = BadPlayerRole(Nickname("Mordred"), Mordred)
  val normalBadGuy = BadPlayerRole(Nickname("normalBadGuy"), NormalBadGuy)
  val badGuys = List(assassin, normalBadGuy, oberon, mordred, morgana)
  val badGuysNoOberon = badGuys.filterNot(_.role == Oberon)
  val badGuysNoMordred = badGuys.filterNot(_.role == Mordred)

  val mockRandomAlg: RandomAlg[IO] = new RandomAlg[IO] {
    override def shuffle[A](l: List[A]): IO[List[A]] = IO.pure(l)
    override def randomGet[A](l: List[A]): IO[A] = IO(l.head) //oops
    override def clockwise[A: Eq](previous: A, l: List[A]): IO[A] = IO(l.head)
    def fuuid: IO[FUUID] = ???
  }

  test("CharacterRole should list bad guys for NormalBadGuy, oberon should not be revealed") {
    val role = CharacterRole.fromRole(NormalBadGuy, badGuys, merlin, None)

    role.badGuys should be(Some(badGuysNoOberon))
    role.character should be(NormalBadGuy)
    role.merlin should be(None)
  }

  test("CharacterRole should list bad guys for Assassin, oberon should not be revealed") {
    val role = CharacterRole.fromRole(Assassin, badGuys, merlin, None)

    role.badGuys should be(Some(badGuysNoOberon))
    role.character should be(Assassin)
    role.merlin should be(None)
  }

  test("CharacterRole should not list bad guys for Oberon") {
    val role = CharacterRole.fromRole(Oberon, badGuys, merlin, None)

    role.badGuys should be(None)
    role.character should be(Oberon)
    role.merlin should be(None)
  }

  test("CharacterRole should list bad guys for Morgana, oberon should not be revealed") {
    val role = CharacterRole.fromRole(Morgana, badGuys, merlin, None)

    role.badGuys should be(Some(badGuysNoOberon))
    role.character should be(Morgana)
    role.merlin should be(None)
  }

  test("CharacterRole should list bad guys for Mordred, oberon should not be revealed") {
    val role = CharacterRole.fromRole(Mordred, badGuys, merlin, None)

    role.badGuys should be(Some(badGuysNoOberon))
    role.character should be(Mordred)
    role.merlin should be(None)
  }

  test("CharacterRole should not list bad guys for NormalGoodGuy") {
    val role = CharacterRole.fromRole(NormalGoodGuy, badGuys, merlin, None)

    role.badGuys should be(None)
    role.character should be(NormalGoodGuy)
    role.merlin should be(None)
  }

  test("CharacterRole should not list bad guys for Percival, but should list Morgana as Merlin") {
    val role = CharacterRole.fromRole(Percival, badGuys, merlin, Some(morgana))

    role.badGuys should be(None)
    role.character should be(Percival)
    role.merlin should be(Some(List(merlin, morgana)))
  }

  test("CharacterRole should list bad guys for Merlin, but should not list Mordred") {
    val role = CharacterRole.fromRole(Merlin, badGuys, merlin, Some(morgana))

    role.badGuys should be(Some(badGuysNoMordred))
    role.character should be(Merlin)
    role.merlin should be(None)
  }
}