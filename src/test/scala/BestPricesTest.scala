import BestPrices.{CabinPrice, Rate, BestGroupPrice, getBestGroupPrices}
import org.scalatest._
import org.scalatest.matchers.should._
import scala.collection.immutable._

class BestPricesTest extends FlatSpec with Matchers {

  "given defined inputs" should "produce expected outputs" in {
    val rates = List(Rate("M1", "Military"), Rate("M2", "Military"), Rate("S1", "Senior"), Rate("S2", "Senior"))
    val cabinPrices = List(CabinPrice("CA", "M1", 200.00), CabinPrice("CA", "M2", 250.00), CabinPrice("CA", "S1", 225.00), CabinPrice("CA", "S2", 260.00), CabinPrice("CB", "M1", 230.00), CabinPrice("CB", "M2", 260.00), CabinPrice("CB", "S1", 245.00), CabinPrice("CB", "S2", 270.00))
    getBestGroupPrices(rates, cabinPrices) should be (List(BestGroupPrice("CA","M1",200.0,"Military"), BestGroupPrice("CA","S1",225.0,"Senior"), BestGroupPrice("CB","M1",230.0,"Military"), BestGroupPrice("CB","S1",245.0,"Senior")))
  }
}
