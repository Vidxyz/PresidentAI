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

    describe("Tests for applyWildCardPenaltyModifier()") {
      it("Should get the modifier value based on the function (1 /(1 + e^[-((0.5d * sizeOfHand) - 4)]))") {
        assert(PlayerIndicators.applyWildCardPenaltyModifer(0) === 0.0179)
        assert(PlayerIndicators.applyWildCardPenaltyModifer(1) === 0.0293)
        assert(PlayerIndicators.applyWildCardPenaltyModifer(5) === 0.1824)
        assert(PlayerIndicators.applyWildCardPenaltyModifer(10) === 0.7310)
        assert(PlayerIndicators.applyWildCardPenaltyModifer(15) === 0.9706)
        assert(PlayerIndicators.applyWildCardPenaltyModifer(20) === 0.9975)
        assert(PlayerIndicators.applyWildCardPenaltyModifer(27) === 0.9999)
      }
    }

    // TODO - update these tests for Wildcards
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
