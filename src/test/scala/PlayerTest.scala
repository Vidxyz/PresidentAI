import game.{Hand, Joker, Move, NormalCard, SpecialCard}
import org.scalatest.FunSpec
import player.{Player, PlayerIndicators}
import utils.Consants._

import org.scalactic.{Equality, TolerantNumerics}

class PlayerTest extends FunSpec{

  // Relaxing the error rate for testing purposes, if something still fails, then you know something is definitely wrong
  val epsilon = 1e-1f
  implicit val doubleEq: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(epsilon)

  describe("Tests for methods in Player class") {

    // No tests for playNextMove yet, individual methods are tested however
    // Possibly try and verify object method calls happened for unit testing
    describe("Tests for playNextMove") {
      // Write test for Automatically plays next move when not real player

      // Write test for Waits for player input when it is real player
    }

    describe("Tests for promptForNextMove") {
      // TODO - above and this - needs more thought
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

    describe("Tests for getListSetSizeForCard()") {

      describe("When validMove size is 1") {
        val validMove = Move(List(SEVEN_Spade))

        describe("When it is only a single card") {
          it("Should return 1") {
            val hand = Hand(List(SEVEN_Diamond))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 1)
          }
        }

        describe("When it is part of a double") {
          it("Should return 2") {
            val hand = Hand(List(SEVEN_Diamond, SEVEN_Club))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 2)
          }
        }

        describe("When it is part of a triple") {
          it("Should return 3") {
            val hand = Hand(List(SEVEN_Diamond, SEVEN_Club, SEVEN_Heart))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 3)
          }
        }

        describe("When it is part of a quad") {
          it("Should return 4") {
            val hand = Hand(List(SEVEN_Diamond, SEVEN_Club, SEVEN_Heart, SEVEN_Spade))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 4)
          }
        }
      }

      describe("When validMove size is 2") {
        val validMove = Move(List(SEVEN_Heart, SEVEN_Spade))

        describe("When it is part of a double") {
          it("Should return 2") {
            val hand = Hand(List(SEVEN_Heart, SEVEN_Spade))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 2)
          }
        }

        describe("When it is part of a triple") {
          it("Should return 3") {
            val hand = Hand(List(SEVEN_Club, SEVEN_Heart, SEVEN_Spade))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 3)
          }
        }

        describe("When it is part of a quad") {
          it("Should return 4") {
            val hand = Hand(List(SEVEN_Diamond, SEVEN_Club, SEVEN_Heart, SEVEN_Spade))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 4)
          }
        }
      }

      describe("When validMove size is 3") {
        val validMove = Move(List(SEVEN_Club, SEVEN_Heart, SEVEN_Spade))

        describe("When it is part of a triple") {
          it("Should return 3") {
            val hand = Hand(List(SEVEN_Club, SEVEN_Heart, SEVEN_Spade))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 3)
          }
        }

        describe("When it is part of a quad") {
          it("Should return 4") {
            val hand = Hand(List(SEVEN_Diamond, SEVEN_Club, SEVEN_Heart, SEVEN_Spade))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 4)
          }
        }
      }

      describe("When validMove size is 4") {
        val validMove = Move(List(SEVEN_Diamond, SEVEN_Club, SEVEN_Heart, SEVEN_Spade))

        describe("When it is part of a quad") {
          it("Should return 4") {
            val hand = Hand(List(SEVEN_Diamond, SEVEN_Club, SEVEN_Heart, SEVEN_Spade))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == 4)
          }
        }
      }

      describe("When validMove has wildcards in it") {

        describe("When it is comprised purely of wildcards") {
          it("Should return move parity when size is 2") {
            val validMove = Move(List(THREE_Diamond(14), THREE_Spade(14)))
            val hand = Hand(List(SEVEN_Diamond, SEVEN_Club, SEVEN_Heart, SEVEN_Spade))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == validMove.parity)
          }

          it("Should return move parity when size is 3") {
            val validMove = Move(List(THREE_Diamond(14), THREE_Club(14), THREE_Spade(14)))
            val hand = Hand(List(SEVEN_Diamond, SEVEN_Club, SEVEN_Heart, SEVEN_Spade))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == validMove.parity)
          }

          it("Should return move parity when size is 4") {
            val validMove = Move(List(THREE_Diamond(14), THREE_Club(14), THREE_Heart(14), THREE_Spade(14)))
            val hand = Hand(List(SEVEN_Diamond, SEVEN_Club, SEVEN_Heart, SEVEN_Spade))
            val playerIndicators = PlayerIndicators(hand)
            assert(playerIndicators.getListSetSizeForCard(validMove) == validMove.parity)
          }
        }

        describe("When move is comprised of wildcards plus normalCards") {
          val validMove = Move(List(THREE_Diamond(12), THREE_Club(12), QUEEN_Heart))

          describe("When it is the only card in Hand") {
            it("Should return 1") {
              val hand = Hand(List(JACK_Diamond, QUEEN_Heart, KING_Heart))
              val playerIndicators = PlayerIndicators(hand)
              assert(playerIndicators.getListSetSizeForCard(validMove) == 1)
            }
          }

          describe("When it is part of a double") {
            it("Should return 1") {
              val hand = Hand(List(JACK_Diamond, QUEEN_Heart, QUEEN_Spade, KING_Heart))
              val playerIndicators = PlayerIndicators(hand)
              assert(playerIndicators.getListSetSizeForCard(validMove) == 2)
            }
          }

          describe("When it is part of a triple") {
            it("Should return 1") {
              val hand = Hand(List(JACK_Diamond, QUEEN_Diamond, QUEEN_Club, QUEEN_Heart, KING_Heart))
              val playerIndicators = PlayerIndicators(hand)
              assert(playerIndicators.getListSetSizeForCard(validMove) == 3)
            }
          }

          describe("When it is part of a quad") {
            it("Should return 1") {
              val hand = Hand(List(JACK_Diamond, QUEEN_Diamond, QUEEN_Club, QUEEN_Heart, QUEEN_Spade, KING_Heart))
              val playerIndicators = PlayerIndicators(hand)
              assert(playerIndicators.getListSetSizeForCard(validMove) == 4)
            }
          }

        }

      }
    }
  }

}
