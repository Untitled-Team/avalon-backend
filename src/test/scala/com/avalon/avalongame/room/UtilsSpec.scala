package com.avalon.avalongame.room

import cats.effect.{ContextShift, IO}
import com.avalon.avalongame.common.GameConfig.{MorganaConfig, OberonConfig}
import com.avalon.avalongame.common._
import org.scalatest.{FunSuite, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class UtilsSpec extends FunSuite with Matchers with ScalaCheckPropertyChecks with enumeratum.ScalacheckInstances {

  implicit val cs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)

  test("Should assign 5 players to roles") {

    val taylor = Nickname("Taylor")
    val chris = Nickname("Chris")
    val carter = Nickname("Carter")
    val nick = Nickname("Nick")
    val cory = Nickname("Cory")

    val users: List[Nickname] =
      List(
        taylor,
        chris,
        carter,
        nick,
        cory)

    val result = Utils.assignRoles[IO](users, GameConfig.default, u => IO.pure(u)).unsafeRunSync()

    (result.badGuys ++ result.goodGuys).size should be(users.size)
    result.badGuys should contain allOf(BadPlayerRole(taylor, Assassin), BadPlayerRole(chris, NormalBadGuy))
    result.goodGuys should contain allOf(GoodPlayerRole(carter, Merlin), GoodPlayerRole(nick, NormalGoodGuy), GoodPlayerRole(cory, NormalGoodGuy))
  }

  test("Should assign 5 players to roles, includes Oberon") {

    val taylor = Nickname("Taylor")
    val chris = Nickname("Chris")
    val carter = Nickname("Carter")
    val nick = Nickname("Nick")
    val cory = Nickname("Cory")

    val users: List[Nickname] =
      List(
        taylor,
        chris,
        carter,
        nick,
        cory)

    val result = Utils.assignRoles[IO](
      users,
      GameConfig.default.copy(oberon = OberonConfig(true)),
      u => IO.pure(u)).unsafeRunSync()

    (result.badGuys ++ result.goodGuys).size should be(users.size)
    result.badGuys should contain allOf(BadPlayerRole(taylor, Assassin), BadPlayerRole(chris, Oberon))
    result.goodGuys should contain allOf(GoodPlayerRole(carter, Merlin), GoodPlayerRole(nick, NormalGoodGuy), GoodPlayerRole(cory, NormalGoodGuy))
  }

  test("Fail if too many bad special characters set to true") {

    val taylor = Nickname("Taylor")
    val chris = Nickname("Chris")
    val carter = Nickname("Carter")
    val nick = Nickname("Nick")
    val cory = Nickname("Cory")

    val users: List[Nickname] =
      List(
        taylor,
        chris,
        carter,
        nick,
        cory)

    val result = Utils.assignRoles[IO](
      users,
      GameConfig.default.copy(oberon = OberonConfig(true), morgana = MorganaConfig(true)),
      u => IO.pure(u)).attempt.unsafeRunSync()

    result should be(Left(TooManyBadSpecialCharacters))
  }

  test("Should assign 6 players to roles") {

    val taylor = Nickname("Taylor")
    val chris = Nickname("Chris")
    val carter = Nickname("Carter")
    val nick = Nickname("Nick")
    val cory = Nickname("Cory")
    val matt = Nickname("Matt")

    val users: List[Nickname] =
      List(
        User(taylor),
        User(chris),
        User(carter),
        User(nick),
        User(cory),
        User(matt)).map(_.nickname)

    val result = Utils.assignRoles[IO](users, GameConfig.default, u => IO.pure(u)).unsafeRunSync()

    (result.badGuys ++ result.goodGuys).size should be(users.size)
    result.badGuys should contain allOf(BadPlayerRole(taylor, Assassin), BadPlayerRole(chris, NormalBadGuy))
    result.goodGuys should contain allOf(
      GoodPlayerRole(carter, Merlin),
      GoodPlayerRole(nick, NormalGoodGuy),
      GoodPlayerRole(cory, NormalGoodGuy),
      GoodPlayerRole(matt, NormalGoodGuy))
  }

  test("Should assign 7 players to roles") {

    val taylor = Nickname("Taylor")
    val chris = Nickname("Chris")
    val carter = Nickname("Carter")
    val nick = Nickname("Nick")
    val cory = Nickname("Cory")
    val matt = Nickname("Matt")
    val liran = Nickname("Liran")

    val users: List[Nickname] =
      List(
        User(taylor),
        User(chris),
        User(liran),
        User(carter),
        User(nick),
        User(cory),
        User(matt)).map(_.nickname)

    val result = Utils.assignRoles[IO](users, GameConfig.default, u => IO.pure(u)).unsafeRunSync()

    (result.badGuys ++ result.goodGuys).size should be(users.size)
    result.badGuys should contain allOf(BadPlayerRole(taylor, Assassin), BadPlayerRole(chris, NormalBadGuy), BadPlayerRole(liran, NormalBadGuy))
    result.goodGuys should contain allOf(
      GoodPlayerRole(carter, Merlin),
      GoodPlayerRole(nick, NormalGoodGuy),
      GoodPlayerRole(cory, NormalGoodGuy),
      GoodPlayerRole(matt, NormalGoodGuy))
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

    val users: List[Nickname] =
      List(
        User(taylor),
        User(chris),
        User(liran),
        User(carter),
        User(nick),
        User(cory),
        User(matt),
        User(brian)).map(_.nickname)

    val result = Utils.assignRoles[IO](users, GameConfig.default, u => IO.pure(u)).unsafeRunSync()

    (result.badGuys ++ result.goodGuys).size should be(users.size)
    result.badGuys should contain allOf(BadPlayerRole(taylor, Assassin), BadPlayerRole(chris, NormalBadGuy), BadPlayerRole(liran, NormalBadGuy))
    result.goodGuys should contain allOf(
      GoodPlayerRole(carter, Merlin),
      GoodPlayerRole(nick, NormalGoodGuy),
      GoodPlayerRole(cory, NormalGoodGuy),
      GoodPlayerRole(matt, NormalGoodGuy),
      GoodPlayerRole(brian, NormalGoodGuy))
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

    val users: List[Nickname] =
      List(
        User(taylor),
        User(chris),
        User(liran),
        User(carter),
        User(nick),
        User(cory),
        User(matt),
        User(brian),
        User(austin)).map(_.nickname)

    val result = Utils.assignRoles[IO](users, GameConfig.default, u => IO.pure(u)).unsafeRunSync()

    (result.badGuys ++ result.goodGuys).size should be(users.size)
    result.badGuys should contain allOf(BadPlayerRole(taylor, Assassin), BadPlayerRole(chris, NormalBadGuy), BadPlayerRole(liran, NormalBadGuy))
    result.goodGuys should contain allOf(
      GoodPlayerRole(carter, Merlin),
      GoodPlayerRole(nick, NormalGoodGuy),
      GoodPlayerRole(cory, NormalGoodGuy),
      GoodPlayerRole(matt, NormalGoodGuy),
      GoodPlayerRole(brian, NormalGoodGuy),
      GoodPlayerRole(austin, NormalGoodGuy))
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

    val users: List[Nickname] =
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
        User(austin)).map(_.nickname)

    val result = Utils.assignRoles[IO](users, GameConfig.default, u => IO.pure(u)).unsafeRunSync()

    (result.badGuys ++ result.goodGuys).size should be(users.size)
    result.badGuys should contain allOf(
      BadPlayerRole(taylor, Assassin),
      BadPlayerRole(chris, NormalBadGuy),
      BadPlayerRole(liran, NormalBadGuy),
      BadPlayerRole(justin, NormalBadGuy))
    result.goodGuys should contain allOf(
      GoodPlayerRole(carter, Merlin),
      GoodPlayerRole(nick, NormalGoodGuy),
      GoodPlayerRole(cory, NormalGoodGuy),
      GoodPlayerRole(matt, NormalGoodGuy),
      GoodPlayerRole(brian, NormalGoodGuy),
      GoodPlayerRole(austin, NormalGoodGuy))
  }
}