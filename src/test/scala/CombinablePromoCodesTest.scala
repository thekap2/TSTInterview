import CombinablePromoCodes.{Promotion, PromotionCombo, allCombinablePromotions, combinablePromotions}
import org.scalatest._
import org.scalatest.matchers.should._

class CombinablePromoCodesTest extends FlatSpec with Matchers {

  val allPromotions = List(Promotion("P1", Seq("P3")), // P1 is not combinable with P3
    Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
    Promotion("P3", Seq("P1")), // P3 is not combinable with P1
    Promotion("P4", Seq("P2")), // P4 is not combinable with P2
    Promotion("P5", Seq("P2")))

  val expectedResultAllPromotions = List(
    PromotionCombo(Seq("P1", "P2")),
    PromotionCombo(Seq("P1", "P4", "P5")),
    PromotionCombo(Seq("P2", "P3")),
    PromotionCombo(Seq("P3", "P4", "P5")))

  val expectedResultP1 = List(
    PromotionCombo(Seq("P1", "P2")),
    PromotionCombo(Seq("P1", "P4", "P5")))

  val expectedResultP3 = List(
    PromotionCombo(Seq("P2", "P3")),
    PromotionCombo(Seq("P3", "P4", "P5")))

  "given allCombinablePromotions is passed allPromotions" should "produce expectedResultAllPromotions" in {
    var result = allCombinablePromotions(allPromotions)
    println()
    info("Expected Output for All Promotion Combinations:")
    result.foreach(combo => info(combo.toString))
    result should be(expectedResultAllPromotions)
  }

  "given combinablePromotions is passed P1 and allPromotions" should "produce expectedResultP1" in {
    var result = combinablePromotions("P1", allPromotions)
    info("Expected Output for Promotion Combinations for promotionCode=”P1”:")
    result.foreach(combo => info(combo.toString))
    result should be(expectedResultP1)
  }

  "given combinablePromotions is passed P3 and allPromotions" should "produce expectedResultP3" in {
    var result = combinablePromotions("P3", allPromotions)
    info("Expected Output for Promotion Combinations for promotionCode=”P3”:")
    result.foreach(combo => info(combo.toString))
    result should be(expectedResultP3)
  }

  "given allCombinablePromotions is passed empty list " should "produce empty result" in {
    allCombinablePromotions(List()) should be(List())
  }

  "given combinablePromotions is passed empty list " should "produce empty result" in {
    combinablePromotions("P1", List()) should be(List())
  }

  "given combinablePromotions is passed unknown promotionCode " should "produce NoSuchElementException" in {
    assertThrows[NoSuchElementException] {
      combinablePromotions("P6", allPromotions) should be(List())
    }
  }
}
