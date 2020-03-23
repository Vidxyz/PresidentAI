import game.GameUtilities
import org.scalatest.FunSpec
import player.{Player, PlayerIndicators}
import utils.Consants

class PlayerTest extends FunSpec {

  describe("PlayerTest") {

    // TODO - Refine these tests and clean up the nonsense
    it("should getNewHand") {
      val player = Player("test", GameUtilities.dealNewHand(54, Consants.totalNumberOfCards))
//      println(player.highCardModifier)
      println(PlayerIndicators.applyCustomSpecialCardModifier(1))
    }

    it("should playNextMove") {

    }

  }

  describe("Tests for methods in Player class") {
    describe("Tests for playNextMove()"){

    }

    describe("Tests for getNewHand()") {

    }
  }


  describe("Tests for methods in PlayerIndicator object") {
    describe("Tests for getListSetSizeForCard()") {

    }
  }

}
