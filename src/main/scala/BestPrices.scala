import collection.mutable.Map
import scala.collection.mutable
import scala.collection.{mutable => m}

object BestPrices {

  def getBestGroupPrices(
    rates: Seq[Rate],
    prices: Seq[CabinPrice]
  ): Seq[BestGroupPrice] = {
    if (rates.isEmpty) List()
    else {
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
  }

  case class Rate(
    rateCode: String,
    rateGroup: String
  )

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