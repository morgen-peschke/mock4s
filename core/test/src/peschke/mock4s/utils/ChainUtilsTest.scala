package peschke.mock4s.utils

import cats.Eq
import cats.data.Chain
import cats.syntax.all._
import munit.ScalaCheckSuite
import peschke.mock4s.MUnitCats
import peschke.mock4s.utils.ChainUtils.{Identify, Mock4sChainOps}
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop._

class ChainUtilsTest extends ScalaCheckSuite with MUnitCats {
  implicit def identifyTuple[A,B]: Identify[(A,B), A] = _._1

  test("updateFirstBy should update only the first matching element") {
    assertEq(
      Chain(1 -> "a", 2 -> "b", 3 -> "c", 1 -> "a").updateFirstBy(1)(_.copy(_2 = "_")),
      Chain(1 -> "_", 2 -> "b", 3 -> "c", 1 -> "a").some
    )
  }

  test("updateFirstBy should return none if no elements match") {
    assertEq(
      Chain(1 -> "a", 2 -> "b", 3 -> "c", 1 -> "a").updateFirstBy(7)(_.copy(_2 = "_")),
      none
    )
  }

  test("updateFirstByE should return a failure if updating fails") {
    assertEq(
      Chain(1 -> "a", 2 -> "b", 3 -> "c", 1 -> "a").updateFirstByE[Int, String](1)(_.show.asLeft),
      "(1,a)".asLeft.some
    )
  }

  test("insertAfterBy should return none if no elements match") {
    assertEq(
      Chain(1 -> "a", 2 -> "b", 3 -> "c", 1 -> "a").insertAfterBy(0 -> "_", 7),
      none
    )
  }

  test("insertAfterBy should return a new chain with the element in the expected location") {
    assertEq(
      Chain(1 -> "a", 2 -> "b", 3 -> "c", 1 -> "a").insertAfterBy(0 -> "_", 2),
      Chain(1 -> "a", 2 -> "b", 0 -> "_", 3 -> "c", 1 -> "a").some
    )
  }

  test("insertAfterBy should only insert the new element once") {
    assertEq(
      Chain(1 -> "a", 2 -> "b", 3 -> "c", 1 -> "a").insertAfterBy(0 -> "_", 1),
      Chain(1 -> "a", 0 -> "_", 2 -> "b", 3 -> "c", 1 -> "a").some
    )
  }

  test("insertBeforeBy should return none if no elements match") {
    assertEq(
      Chain(1 -> "a", 2 -> "b", 3 -> "c", 1 -> "a").insertBeforeBy(0 -> "_", 7),
      none
    )
  }

  test("insertBeforeBy should return a new chain with the element in the expected location") {
    assertEq(
      Chain(1 -> "a", 2 -> "b", 3 -> "c", 1 -> "a").insertBeforeBy(0 -> "_", 3),
      Chain(1 -> "a", 2 -> "b", 0 -> "_", 3 -> "c", 1 -> "a").some
    )
  }

  test("insertBeforeBy should only insert the new element once") {
    assertEq(
      Chain(1 -> "a", 2 -> "b", 3 -> "c", 1 -> "a").insertBeforeBy(0 -> "_", 1),
      Chain(0 -> "_", 1 -> "a", 2 -> "b", 3 -> "c", 1 -> "a").some
    )
  }

  property("startsWith should return true if the chain starts with the prefix") {
    forAll(chainsOf(Gen.alphaChar), chainsOf(Gen.numChar)) { (prefix, suffix) =>
      assert((prefix ++ suffix).startsWith(prefix))
    }
  }

  property("startsWith should return false if the chain does not start with the prefix") {
    forAll(nonEmptyChainsOf(Gen.alphaChar), nonEmptyChainsOf(Gen.numChar)) { (prefix, suffix) =>
      assert(!(suffix.toChain ++ prefix.toChain).startsWith(prefix.toChain))
    }
  }

  property("startsWith should respect custom equality") {
    val unlawfulTupleEquality: Eq[(Char, Int)] = Eq.by(_._1)

    forAll(
      chainsOf(Gen.zip(Gen.alphaChar, arbitrary[Int])),
      chainsOf(Gen.zip(Gen.numChar, arbitrary[Int]))
    ) { (rawPrefix, suffix) =>
      val input = rawPrefix ++ suffix
      val prefix = {
        val (chars, ints) = rawPrefix.unzip
        chars.zipAll(ints.reverse, '_', 0)
      }
      assert(input.startsWith(prefix)(unlawfulTupleEquality))
    }
  }

  property("endsWith should return true if the chain ends with the suffix") {
    forAll(chainsOf(Gen.alphaChar), chainsOf(Gen.numChar)) { (prefix, suffix) =>
      assert((prefix ++ suffix).endsWith(suffix))
    }
  }

  property("endsWith should return false if the chain does not end with the suffix") {
    forAll(nonEmptyChainsOf(Gen.alphaChar), nonEmptyChainsOf(Gen.numChar)) { (prefix, suffix) =>
      assert(!(suffix.toChain ++ prefix.toChain).endsWith(suffix.toChain))
    }
  }

  property("endsWith should respect custom equality") {
    val unlawfulTupleEquality: Eq[(Char, Int)] = Eq.by(_._1)

    forAll(
      chainsOf(Gen.zip(Gen.alphaChar, arbitrary[Int])),
      chainsOf(Gen.zip(Gen.numChar, arbitrary[Int]))
    ) { (prefix, rawSuffix) =>
      val input = prefix ++ rawSuffix
      val suffix = {
        val (chars, ints) = rawSuffix.unzip
        chars.zipAll(ints.reverse, '_', 0)
      }
      assert(input.endsWith(suffix)(unlawfulTupleEquality))
    }
  }

  property("containsEq should return true if the chain contains the sub-chain") {
    forAll(
      chainsOf(Gen.numChar),
      chainsOf(Gen.alphaChar),
      chainsOf(Gen.numChar)
    ) { (prefix, subChain, suffix) =>
      assert((prefix ++ subChain ++ suffix).containsEq(subChain))
    }
  }

  property("containsEq should return false if the chain does not contain with the sub-chain") {
    forAll(nonEmptyChainsOf(Gen.alphaChar), nonEmptyChainsOf(Gen.numChar)) { (input, subChain) =>
      assert(!input.toChain.containsEq(subChain.toChain))
    }
  }

  property("containsEq should respect custom equality") {
    val unlawfulTupleEquality: Eq[(Char, Int)] = Eq.by(_._1)

    forAll(
      chainsOf(Gen.zip(Gen.numChar, arbitrary[Int])),
      chainsOf(Gen.zip(Gen.alphaChar, arbitrary[Int])),
      chainsOf(Gen.zip(Gen.numChar, arbitrary[Int]))
    ) { (prefix, rawSubChain, suffix) =>
      val input = prefix ++ rawSubChain ++ suffix
      val subChain = {
        val (chars, ints) = rawSubChain.unzip
        chars.zipAll(ints.reverse, '_', 0)
      }
      assert(input.containsEq(subChain)(unlawfulTupleEquality))
    }
  }
}
