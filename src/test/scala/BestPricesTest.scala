import BestPrices.{CabinPrice, Rate, BestGroupPrice, getBestGroupPrices}
import org.scalatest._
import org.scalatest.matchers.should._
import scala.collection.immutable._

class BestPricesTest extends FlatSpec with Matchers {

  val rates: Seq[Rate] = List(
    Rate("M1", "Military"),
    Rate("M2", "Military"),
    Rate("S1", "Senior"),
    Rate("S2", "Senior"))
  val cabinPrices: Seq[CabinPrice] = List(
    CabinPrice("CA", "M1", 200.00),
    CabinPrice("CA", "M2", 250.00),
    CabinPrice("CA", "S1", 225.00),
    CabinPrice("CA", "S2", 260.00),
    CabinPrice("CB", "M1", 230.00),
    CabinPrice("CB", "M2", 260.00),
    CabinPrice("CB", "S1", 245.00),
    CabinPrice("CB", "S2", 270.00))
  val expectedBestGroupPrices: Seq[BestGroupPrice] = List(
    BestGroupPrice("CA", "M1", 200.0, "Military"),
    BestGroupPrice("CA", "S1", 225.0, "Senior"),
    BestGroupPrice("CB", "M1", 230.0, "Military"),
    BestGroupPrice("CB", "S1", 245.0, "Senior"))

  "given getBestGroupPrices is passed rates and cabinPrices" should "produce expectedBestGroupPrices" in {
    val result = getBestGroupPrices(rates, cabinPrices)
    println("Expected Output - Best Cabin Prices:")
    result.foreach(println(_))
    result should be(expectedBestGroupPrices)
  }

  "given cabin with unnown rate code" should "produce NoSuchElementException" in {
    val unknownRateCodeCabinPrices = List(CabinPrice("CA", "M3", 200.00))
    assertThrows[NoSuchElementException] {
      getBestGroupPrices(rates, unknownRateCodeCabinPrices)
    }
  }

  "given empty cabinPrices" should "produce empty result" in {
    val emptyCabinPrices = List()
    getBestGroupPrices(rates, emptyCabinPrices) should be(List())
  }

  "given empty rates" should "produce empty result" in {
    val emptyRates = List()
    getBestGroupPrices(emptyRates, cabinPrices) should be(List())
  }
}
