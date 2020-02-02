import collection.mutable.Map
import scala.collection.mutable
import scala.collection.{mutable => m}

object BestPrices {

  def main(args: Array[String]): Unit = {
    val rates = List(Rate("M1", "Military"), Rate("M2", "Military"), Rate("S1", "Senior"), Rate("S2", "Senior"))
    val cabinPrices = List(CabinPrice("CA", "M1", 200.00), CabinPrice("CA", "M2", 250.00), CabinPrice("CA", "S1", 225.00), CabinPrice("CA", "S2", 260.00), CabinPrice("CB", "M1", 230.00), CabinPrice("CB", "M2", 260.00), CabinPrice("CB", "S1", 245.00), CabinPrice("CB", "S2", 270.00))
    getBestGroupPrices(rates, cabinPrices).foreach(println(_))
  }

  def getBestGroupPrices(
    rates: Seq[Rate],
    prices: Seq[CabinPrice]
  ): Seq[BestGroupPrice] = {
    val rateMap = rates.map(rate => (rate.rateCode, rate.rateGroup)).toMap
    prices
      .foldLeft(m.Map[(String, String), BestGroupPrice]())((bestPrices, price) => {
        val key = (price.cabinCode, rateMap(price.rateCode))
        val currentBestGroupPrice = bestPrices.get(key)
        if (!currentBestGroupPrice.exists(bestGroupPrice => bestGroupPrice.price < price.price)) {
          bestPrices += (key -> BestGroupPrice(price.cabinCode, price.rateCode, price.price, rateMap(price.rateCode)))
        }
        bestPrices
      }).values.toSeq.sortBy(_.price)
  }

  case class Rate(rateCode: String, rateGroup: String)

  case class CabinPrice(
    cabinCode: String,
    rateCode: String,
    price: BigDecimal
  )

  case class BestGroupPrice(
    cabinCode: String,
    rateCode: String,
    price: BigDecimal,
    rateGroup: String
  )

}