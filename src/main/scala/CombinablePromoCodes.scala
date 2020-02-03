import scala.language.postfixOps
import scala.collection.mutable.ListBuffer
import scala.collection.{mutable => m}

object CombinablePromoCodes {

  def combinablePromotions(
    promotionCode: String,
    allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] = {
    if (allPromotions.isEmpty) List()
    else {
      val promotion = allPromotions.find(_.code == promotionCode).get
      allCombinablePromotions(allPromotions.filterNot(otherPromotion => promotion.notCombinableWith.contains(
        otherPromotion.code)))
    }
  }

  def allCombinablePromotions(
    allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] = {
    def buildCombinations(
      promotions: Seq[Promotion]
    ): Seq[Seq[String]] = {
      promotions
        .foldLeft(new ListBuffer[Seq[String]])((allPromotionCombos, promotion) => {
          val allPromotionsSubList = promotions
            .filterNot(otherPromotion => promotions.indexOf(promotion) > promotions.indexOf(otherPromotion)
              || otherPromotion == promotion
              || promotion.notCombinableWith.contains(otherPromotion.code))
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

    if (allPromotions.isEmpty) List()
    else {
      var combinations = buildCombinations(allPromotions)
      for (a <- combinations; b <- combinations) {
        if (a != b && a.forall(b contains)) combinations = combinations.filterNot(_ == a)
      }
      combinations.map(combo => PromotionCombo(combo))
    }
  }

  case class Promotion(
    code: String,
    notCombinableWith: Seq[String]
  )

  case class PromotionCombo(
    promotionCodes: Seq[String]
  )

}
