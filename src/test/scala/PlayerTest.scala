import game.FaceValue._
import game.Suits._
import game.{Hand, Joker, Move, NormalCard, SpecialCard}
import org.scalatest.FunSpec
import player.{Player, PlayerIndicators}

import org.scalactic.{Equality, TolerantNumerics}

class PlayerTest extends FunSpec{

  // Relaxing the error rate for testing purposes, if something still fails, then you know something is definitely wrong
  val epsilon = 1e-1f
  implicit val doubleEq: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(epsilon)

  describe("Tests for methods in Player class") {

    // No tests for playNextMove yet, individual methods are tested however
    // Possibly try and verify object method calls happened for unit testing

    describe("Tests for getNewHand()") {
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


  describe("Tests for methods in PlayerIndicator object") {

    describe("Tests for applyCustomerSpecialCardModifier()"){
      it("Should get the modifier value based on the spline interpolation data points") {
        assert(PlayerIndicators.applyCustomSpecialCardModifier(30) === 0d)
        assert(PlayerIndicators.applyCustomSpecialCardModifier(20) === 10d)
        assert(PlayerIndicators.applyCustomSpecialCardModifier(10) === 20d)
        assert(PlayerIndicators.applyCustomSpecialCardModifier(6) === 50d)
        assert(PlayerIndicators.applyCustomSpecialCardModifier(2) === 99d)
        assert(PlayerIndicators.applyCustomSpecialCardModifier(1) === 100d)
      }
    }

    describe("Tests for getListSetSizeForCard()") {

      describe("When validMove size is 1") {
        val validMove = Move(List(NormalCard(SEVEN, Spade)))

        describe("When it is only a single card") {
          it("Should return 1") {
            val hand = Hand(List(NormalCard(SEVEN, Diamond)))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 1)
          }
        }

        describe("When it is part of a double") {
          it("Should return 2") {
            val hand = Hand(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club)))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 2)
          }
        }

        describe("When it is part of a triple") {
          it("Should return 3") {
            val hand = Hand(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart)))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 3)
          }
        }

        describe("When it is part of a quad") {
          it("Should return 4") {
            val hand = Hand(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 4)
          }
        }
      }

      describe("When validMove size is 2") {
        val validMove = Move(List(NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))

        describe("When it is part of a double") {
          it("Should return 2") {
            val hand = Hand(List(NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 2)
          }
        }

        describe("When it is part of a triple") {
          it("Should return 3") {
            val hand = Hand(List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 3)
          }
        }

        describe("When it is part of a quad") {
          it("Should return 4") {
            val hand = Hand(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 4)
          }
        }
      }

      describe("When validMove size is 3") {
        val validMove = Move(List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))

        describe("When it is part of a triple") {
          it("Should return 3") {
            val hand = Hand(List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 3)
          }
        }

        describe("When it is part of a quad") {
          it("Should return 4") {
            val hand = Hand(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 4)
          }
        }
      }

      describe("When validMove size is 4") {
        val validMove = Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))

        describe("When it is part of a quad") {
          it("Should return 4") {
            val hand = Hand(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 4)
          }
        }
      }
    }
  }

}
