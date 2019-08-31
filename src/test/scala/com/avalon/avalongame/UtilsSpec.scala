package com.avalon.avalongame

import cats.effect.{ContextShift, IO}
import org.scalatest.{FunSuite, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class UtilsSpec extends FunSuite with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {
  import com.mrdziuban.ScalacheckMagnolia._

  implicit val cs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)

  test("Should assign 5 players to roles") {

    val taylor = Nickname("Taylor")
    val chris = Nickname("Chris")
    val carter = Nickname("Carter")
    val nick = Nickname("Nick")
    val cory = Nickname("Cory")

    val users: List[User] =
      List(
        User(taylor),
        User(chris),
        User(carter),
        User(nick),
        User(cory))

    val result = Utils.assignRoles[IO](users, u => IO.pure(u)).unsafeRunSync()

    (result.badGuys ++ result.goodGuys).size should be(users.size)
    result.badGuys should contain allOf(Assassin(taylor), NormalBadGuy(chris))
    result.goodGuys should contain allOf(Merlin(carter), NormalGoodGuy(nick), NormalGoodGuy(cory))
  }

  test("Should assign 6 players to roles") {

    val taylor = Nickname("Taylor")
    val chris = Nickname("Chris")
    val carter = Nickname("Carter")
    val nick = Nickname("Nick")
    val cory = Nickname("Cory")
    val matt = Nickname("Matt")

    val users: List[User] =
      List(
        User(taylor),
        User(chris),
        User(carter),
        User(nick),
        User(cory),
        User(matt))

    val result = Utils.assignRoles[IO](users, u => IO.pure(u)).unsafeRunSync()

    (result.badGuys ++ result.goodGuys).size should be(users.size)
    result.badGuys should contain allOf(Assassin(taylor), NormalBadGuy(chris))
    result.goodGuys should contain allOf(Merlin(carter), NormalGoodGuy(nick), NormalGoodGuy(cory), NormalGoodGuy(matt))
  }

  test("Should assign 7 players to roles") {

    val taylor = Nickname("Taylor")
    val chris = Nickname("Chris")
    val carter = Nickname("Carter")
    val nick = Nickname("Nick")
    val cory = Nickname("Cory")
    val matt = Nickname("Matt")
    val liran = Nickname("Liran")

    val users: List[User] =
      List(
        User(taylor),
        User(chris),
        User(liran),
        User(carter),
        User(nick),
        User(cory),
        User(matt))

    val result = Utils.assignRoles[IO](users, u => IO.pure(u)).unsafeRunSync()

    (result.badGuys ++ result.goodGuys).size should be(users.size)
    result.badGuys should contain allOf(Assassin(taylor), NormalBadGuy(chris), NormalBadGuy(liran))
    result.goodGuys should contain allOf(Merlin(carter), NormalGoodGuy(nick), NormalGoodGuy(cory), NormalGoodGuy(matt))
  }

  test("Should assign 8 players to roles") {

    val taylor = Nickname("Taylor")
    val chris = Nickname("Chris")
    val carter = Nickname("Carter")
    val nick = Nickname("Nick")
    val cory = Nickname("Cory")
    val matt = Nickname("Matt")
    val liran = Nickname("Liran")
    val brian = Nickname("Brian")

    val users: List[User] =
      List(
        User(taylor),
        User(chris),
        User(liran),
        User(carter),
        User(nick),
        User(cory),
        User(matt),
        User(brian))

    val result = Utils.assignRoles[IO](users, u => IO.pure(u)).unsafeRunSync()

    (result.badGuys ++ result.goodGuys).size should be(users.size)
    result.badGuys should contain allOf(Assassin(taylor), NormalBadGuy(chris), NormalBadGuy(liran))
    result.goodGuys should contain allOf(Merlin(carter), NormalGoodGuy(nick), NormalGoodGuy(cory), NormalGoodGuy(matt), NormalGoodGuy(brian))
  }

  test("Should assign 9 players to roles") {

    val taylor = Nickname("Taylor")
    val chris = Nickname("Chris")
    val carter = Nickname("Carter")
    val nick = Nickname("Nick")
    val cory = Nickname("Cory")
    val matt = Nickname("Matt")
    val liran = Nickname("Liran")
    val brian = Nickname("Brian")
    val austin = Nickname("Austin")

    val users: List[User] =
      List(
        User(taylor),
        User(chris),
        User(liran),
        User(carter),
        User(nick),
        User(cory),
        User(matt),
        User(brian),
        User(austin))

    val result = Utils.assignRoles[IO](users, u => IO.pure(u)).unsafeRunSync()

    (result.badGuys ++ result.goodGuys).size should be(users.size)
    result.badGuys should contain allOf(Assassin(taylor), NormalBadGuy(chris), NormalBadGuy(liran))
    result.goodGuys should contain allOf(
      Merlin(carter),
      NormalGoodGuy(nick),
      NormalGoodGuy(cory),
      NormalGoodGuy(matt),
      NormalGoodGuy(brian),
      NormalGoodGuy(austin))
  }

  test("Should assign 10 players to roles") {

    val taylor = Nickname("Taylor")
    val chris = Nickname("Chris")
    val carter = Nickname("Carter")
    val nick = Nickname("Nick")
    val cory = Nickname("Cory")
    val matt = Nickname("Matt")
    val liran = Nickname("Liran")
    val brian = Nickname("Brian")
    val austin = Nickname("Austin")
    val justin = Nickname("Justin")

    val users: List[User] =
      List(
        User(taylor),
        User(chris),
        User(liran),
        User(justin),
        User(carter),
        User(nick),
        User(cory),
        User(matt),
        User(brian),
        User(austin))

    val result = Utils.assignRoles[IO](users, u => IO.pure(u)).unsafeRunSync()

    (result.badGuys ++ result.goodGuys).size should be(users.size)
    result.badGuys should contain allOf(Assassin(taylor), NormalBadGuy(chris), NormalBadGuy(liran), NormalBadGuy(justin))
    result.goodGuys should contain allOf(
      Merlin(carter),
      NormalGoodGuy(nick),
      NormalGoodGuy(cory),
      NormalGoodGuy(matt),
      NormalGoodGuy(brian),
      NormalGoodGuy(austin))
  }
}