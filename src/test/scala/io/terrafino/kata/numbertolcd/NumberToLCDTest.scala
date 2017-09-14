package io.terrafino.kata.numbertolcd

import org.scalatest.{FunSuite, Matchers}

class NumberToLCDTest extends FunSuite with Matchers {

  private val Zero =
      " _ \n" +
      "| |\n" +
      "|_|\n"
  private val BigZero =
      "      \n" +
      "  __  \n" +
      " |  | \n" +
      " |  | \n" +
      " |  | \n" +
      " |__| \n"
  private val One =
      "   \n" +
      "  |\n" +
      "  |\n"
  private val Two =
      " _ \n" +
      " _|\n" +
      "|_ \n"
  private val Three =
      " _ \n" +
      " _|\n" +
      " _|\n"
  private val Four =
      "   \n" +
      "|_|\n" +
      "  |\n"
  private val Five =
      " _ \n" +
      "|_ \n" +
      " _|\n"
  private val Six =
      " _ \n" +
      "|_ \n" +
      "|_|\n"
  private val Seven =
      " _ \n" +
      "  |\n" +
      "  |\n"
  private val Eight =
      " _ \n" +
      "|_|\n" +
      "|_|\n"
  private val Nine =
      " _ \n" +
      "|_|\n" +
      " _|\n"
  private val Ten =
      "     _ \n" +
      "  | | |\n" +
      "  | |_|\n"
  private val OneTwoThree =
      "     _   _ \n" +
      "  |  _|  _|\n" +
      "  | |_   _|\n"

  val ntl = new NumberToLCD()

  test("Should convert 0") {
    ntl.convert(0) shouldBe Zero
  }

  test("Should convert 1") {
    ntl.convert(1) shouldBe One
  }

  test("Should convert 2") {
    ntl.convert(2) shouldBe Two
  }

  test("Should convert 3") {
    ntl.convert(3) shouldBe Three
  }

  test("Should convert 4") {
    ntl.convert(4) shouldBe Four
  }

  test("Should convert 5") {
    ntl.convert(5) shouldBe Five
  }

  test("Should convert 6") {
    ntl.convert(6) shouldBe Six
  }

  test("Should convert 7") {
    ntl.convert(7) shouldBe Seven
  }

  test("Should convert 8") {
    ntl.convert(8) shouldBe Eight
  }

  test("Should convert 9") {
    ntl.convert(9) shouldBe Nine
  }

  test("Should convert 10") {
    ntl.convert(10) shouldBe Ten
  }

  test("Should convert 123") {
    ntl.convert(123) shouldBe OneTwoThree
  }

  test("Should convert 0 (with factor 2)") {
    val ntl = new NumberToLCD(2)
    ntl.convert(0) shouldBe BigZero
  }

}
