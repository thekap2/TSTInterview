import scala.language.postfixOps
import scala.collection.mutable.ListBuffer
import scala.collection.{mutable => m}

object CombinablePromoCodes {

  def main(args: Array[String]): Unit = {
    val allPromotions = List(Promotion("P1", Seq("P3")), // P1 is not combinable with P3
      Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
      Promotion("P3", Seq("P1")), // P3 is not combinable with P1
      Promotion("P4", Seq("P2")), // P4 is not combinable with P2
      Promotion("P5", Seq("P2")))
    allCombinablePromotions(allPromotions).foreach(println(_))
  }

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    def buildCombinations(promotions: Seq[Promotion]): Seq[Seq[String]] = {
      promotions.foldLeft(new ListBuffer[Seq[String]])((allPromotionCombos, promotion) => {
        val allPromotionsSubList = promotions.filterNot(otherPromotion => promotions.indexOf(promotion) > promotions.indexOf(
          otherPromotion) || otherPromotion == promotion || promotion.notCombinableWith.contains(
          otherPromotion.code))
        if (allPromotionsSubList.isEmpty) {
          allPromotionCombos += List(promotion.code)
        } else {
          allPromotionCombos ++= buildCombinations(allPromotionsSubList).map(codes => {
            var newCodes = promotion.code :: codes.toList
            newCodes
          }).toList
        }
        allPromotionCombos
      }).toList
    }

    var combinations = buildCombinations(allPromotions)
    for (a <- combinations; b <- combinations) {
      if (a != b && a.forall(b contains)) combinations = combinations.filterNot(_ == a)
    }
    combinations.map(combo => PromotionCombo(combo))
  }

  case class Promotion(code: String, notCombinableWith: Seq[String])

  case class PromotionCombo(promotionCodes: Seq[String])

}
