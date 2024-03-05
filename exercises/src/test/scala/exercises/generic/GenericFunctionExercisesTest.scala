package exercises.generic

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import exercises.generic.GenericFunctionExercises._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Try

class GenericFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  test("Pair swap") {
    forAll{ (first: String, second: String) =>
      assert(Pair(first, second).swap == Pair(second, first))
    }
  }

  test("Pair map") {
    forAll { (first: String, second: String) =>
      assert(Pair(first, second).map(identity) == Pair(first, second))
    }
  }

  test("Pair decoded") {
    assert(decoded == Pair("Functional", "Programming"))
  }

  test("Pair zipWith") {
    assert(Pair(0, 2).zipWith(Pair(3, 4))((x, y) => x + y) == Pair(3, 6))
  }

  test("Pair productNames") {
    assert(products == Pair(Product("Coffee", 2.5), Product("Plane ticket", 329.99)))
  }

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  test("Predicate &&") {
    forAll{ (eval1: Int => Boolean, value: Int) =>
      val p1 = Predicate(eval1)

      def False[A]: Predicate[A] = Predicate(_ => false)
      def True[A]: Predicate[A] = Predicate(_ => true)

      assert(!(p1 && False)(value))
      assert((p1 && True)(value) == p1(value))
    }
  }

  test("Predicate ||") {
    forAll{ (eval1: Int => Boolean, value: Int) =>
      val p1 = Predicate(eval1)

      def False[A]: Predicate[A] = Predicate(_ => false)
      def True[A]: Predicate[A] = Predicate(_ => true)

      assert((p1 || False)(value) == p1(value))
      assert((p1 || True)(value) )
    }

  }

  test("Predicate flip") {
    assert(Predicate.True.flip() == false)
    assert(Predicate.False.flip() == true)
  }

  test("Predicate isValidUser") {
    assert(isValidUser(User("John", 20)))
    assert(!isValidUser(User("John", 17)))
    assert(!isValidUser(User("john", 20)))
    assert(!isValidUser(User("x"   , 23)))
  }

  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  test("JsonDecoder UserId") {
    assert(userIdDecoder.decode("1234") == UserId(1234))
    assert(Try(userIdDecoder.decode("John")).isFailure)
  }

  test("JsonDecoder UserId round-trip") {
    forAll { number: Int =>
      val json = number.toString
      assert(userIdDecoder.decode(json) == UserId(number))
    }
  }

  test("JsonDecoder LocalDate") {
    assert(localDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020,3,26))
    assert(Try(localDateDecoder.decode("2020-03-26")).isFailure)
    assert(Try(localDateDecoder.decode("hello")).isFailure)
  }

  test("JsonDecoder LocalDate round-trip") {
    forAll { localDate: LocalDate =>
      val json = s"\"${DateTimeFormatter.ISO_LOCAL_DATE.format(localDate)}\""
      assert(localDateDecoder.decode(json) == localDate)
    }
  }

  val genLocalDate: Gen[LocalDate] = Gen.choose(LocalDate.MIN.toEpochDay, LocalDate.MAX.toEpochDay)
    .map(LocalDate.ofEpochDay)

  implicit val arbitraryLocalDate: Arbitrary[LocalDate] =
    Arbitrary(genLocalDate)

  test("JsonDecoder weirdLocalDateDecoder") {
    assert(weirdLocalDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020,3,26))
    assert(weirdLocalDateDecoder.decode("18347")          == LocalDate.of(2020,3,26))
    assert(Try(weirdLocalDateDecoder.decode("hello")).isFailure)
  }

}
