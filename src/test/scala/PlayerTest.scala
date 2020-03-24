import game.FaceValue._
import game.Suits._
import game.{GameUtilities, Hand, Joker, Move, NormalCard, SpecialCard}
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
      val currentHand = Hand(List(
        NormalCard(SIX, Diamond),
        NormalCard(SIX, Heart),
        NormalCard(EIGHT, Club),
        NormalCard(TEN, Heart),
        NormalCard(ACE, Diamond),
        NormalCard(ACE, Heart),
        SpecialCard(TWO, Diamond),
        Joker,
      ))
      val player = Player("Test", currentHand)
      describe("When the movePlayed is none"){
        it("Should return the same hand") {
          assert(player.getNewHand(player.hand, None) == currentHand)
        }
      }

      describe("When the movePlayed does not involve a hand"){
        it("Should return the same hand") {
          assert(player.getNewHand(player.hand, Some(Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Spade)))))
            == currentHand)
        }
      }

      describe("When the movePlayed involves a card in the hand"){
        describe("When the move played is a normalCard") {
          it("Should return the hand minus the played cards") {
            assert(player.getNewHand(player.hand, Some(Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Heart)))))
            == Hand(currentHand.listOfCards.slice(0,4) ++ currentHand.listOfCards.slice(6, 8)))
          }
        }

        describe("When the move played is a specialCard") {
          it("Should return the hand minus the played cards") {
            assert(player.getNewHand(player.hand, Some(Move(List(SpecialCard(TWO, Diamond)))))
              == Hand(currentHand.listOfCards.slice(0,6) ++ currentHand.listOfCards.slice(7, 8)))
            assert(player.getNewHand(player.hand, Some(Move(List(Joker))))
              == Hand(currentHand.listOfCards.slice(0,7)))
          }
        }

      }

    }

    describe("Tests for getNewHand()") {

    }
  }


  describe("Tests for methods in PlayerIndicator object") {
    describe("Tests for getListSetSizeForCard()") {

    }
  }

}
