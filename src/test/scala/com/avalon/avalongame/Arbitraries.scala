package com.avalon.avalongame

import com.avalon.avalongame.common.RoomId
import org.scalacheck.{Arbitrary, Gen}

object Arbitraries {
  implicit val roomIdArb: Arbitrary[RoomId] = Arbitrary(Gen.alphaStr.map(RoomId.create))
}
