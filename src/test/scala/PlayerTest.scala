import java.io.ByteArrayInputStream

import game.GameUtilities.IllegalMoveSuppliedException
import game.{BlackJoker, GameUtilities, Hand, IllegalAssumedValueException, Move, NormalCard, RedJoker, SpecialCard}
import org.scalatest.FunSpec
import player.{Player, PlayerIndicators}
import utils.Constants._
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
      val hand = Hand(List(
        THREE_Club, FOUR_Heart, SIX_Heart, SIX_Spade, SEVEN_Heart, SEVEN_Spade, TEN_Diamond, JACK_Diamond,
        JACK_Club, ACE_Spade, TWO_Club, TWO_Heart, TWO_Spade, BlackJoker
      ))
      val player = Player("Test", hand)

      describe("When user inputs 'pass'") {
        it("Should return none") {
          val in = new ByteArrayInputStream(("pAsS").getBytes)
          Console.withIn(in){
            assert(player.promptForNextMove(hand, Move(List.empty)).isEmpty)
          }
        }
      }

      describe("When a move that is valid") {
        it("Should return the move") {
          val in = new ByteArrayInputStream(("<2,HEArt> <2,club> <2,spADE>").getBytes)
          Console.withIn(in){
            assert(player.promptForNextMove(hand, Move(List.empty)).get == Move(List(TWO_Club, TWO_Heart, TWO_Spade)))
          }
        }
      }

      describe("When the user inputs a move that is invalid given the gameState") {
        it("Prompts again until it gets a correct move") {
          val in = new ByteArrayInputStream(("<6,spade> <6,heart>\n<7,heart> <7,spade>").getBytes)
          val gameState = Move(List(SEVEN_Diamond, SEVEN_Club))
          Console.withIn(in) {
            assert(player.promptForNextMove(hand, gameState).get == Move(List(SEVEN_Heart, SEVEN_Spade)))
          }
        }
      }

      describe("When the user inputs a move that is comprised of cards not in hand") {
        it("Prompts again until it gets a correct move") {
          val in = new ByteArrayInputStream(("<10,diamond> <10,spade>\n<3(10),Heart> <10,diamond>\n<2,diamond> <2,club>\n<7,heart> <7,spade>").getBytes)
          val gameState = Move(List(SEVEN_Diamond, SEVEN_Club))
          Console.withIn(in) {
            assert(player.promptForNextMove(hand, gameState).get == Move(List(SEVEN_Heart, SEVEN_Spade)))
          }
        }
      }

      describe("When the user inputs bad input") {
        it("Prompts again until it gets a correct move") {
          val in = new ByteArrayInputStream(("<>#$sd\n12390df_Wd\n<14,adfo> <123,HFO>\n\n<7,heart> <7,spade>").getBytes)
          val gameState = Move(List(SEVEN_Diamond, SEVEN_Club))
          Console.withIn(in) {
            assert(player.promptForNextMove(hand, gameState).get == Move(List(SEVEN_Heart, SEVEN_Spade)))
          }
        }

        it("Prompts again until user passes") {
          val in = new ByteArrayInputStream(("<>#$sd\n12390df_Wd\n<14,adfo> <123,HFO>\n\npass").getBytes)
          val gameState = Move(List(SEVEN_Diamond, SEVEN_Club))
          Console.withIn(in) {
            assert(player.promptForNextMove(hand, gameState).isEmpty)
          }
        }
      }
    }

    describe("Tests for parseUserLine") {
      val hand = Hand(List(
        THREE_Club, FOUR_Heart, SEVEN_Club, TEN_Diamond, JACK_Diamond,
        JACK_Club, ACE_Spade, TWO_Club, TWO_Heart, TWO_Spade, BlackJoker
      ))
      val player = Player("Test", hand)

      describe("When user inputs bad expression") {
        it("Should throw IllegalMoveSuppliedException"){
          assertThrows[IllegalMoveSuppliedException](player.parseUserLine("Pass"))
          assertThrows[IllegalMoveSuppliedException](player.parseUserLine("<3,gla>"))
          assertThrows[IllegalMoveSuppliedException](player.parseUserLine("<(_)>"))
          assertThrows[IllegalMoveSuppliedException](player.parseUserLine("<3(5,club>"))
          assertThrows[IllegalMoveSuppliedException](player.parseUserLine("(5,Heart)"))
          assertThrows[IllegalMoveSuppliedException](player.parseUserLine(""))
          assertThrows[IllegalMoveSuppliedException](player.parseUserLine("_-*/2314"))
          assertThrows[IllegalMoveSuppliedException](player.parseUserLine("[4,club]]"))
          assertThrows[IllegalMoveSuppliedException](player.parseUserLine("[x,23987]"))
          assertThrows[IllegalMoveSuppliedException](player.parseUserLine("<3(-1),HEARt>"))
        }

        it("Should throw IllegalAssumedValueException") {
          assertThrows[IllegalAssumedValueException](player.parseUserLine("<3(3),Club>"))
          assertThrows[IllegalAssumedValueException](player.parseUserLine("<3(0),diamond>"))
          assertThrows[IllegalAssumedValueException](player.parseUserLine("<3(15),SpAdE>"))
          assertThrows[IllegalAssumedValueException](player.parseUserLine("<3(100),HearT>"))
        }
      }

      describe("When user inputs a single card") {
        it("Should return the right move") {
          val expectedMove = Move(List(SEVEN_Heart))
          val move = player.parseUserLine("<7,heart>")
          assert(expectedMove == move.get)
        }
      }

      describe("When user inputs a double ") {
        it("Should return the right move") {
          val expectedMove = Move(List(SEVEN_Heart, SEVEN_Spade))
          val move = player.parseUserLine("<7,heart> <7,spade>")
          assert(expectedMove == move.get)
        }
      }

      describe("When user inputs a triple ") {
        it("Should return the right move") {
          val expectedMove = Move(List(JACK_Club, JACK_Heart, JACK_Spade))
          val move = player.parseUserLine("<J,club> <J,heart> <j,SPADE>")
          assert(expectedMove == move.get)
        }
      }

      describe("When user inputs a quad ") {
        it("Should return the right move") {
          val expectedMove = Move(List(JACK_Diamond, JACK_Club, JACK_Heart, JACK_Spade))
          val move = player.parseUserLine("<J,diamond> <J,club> <J,heart> <j,SPADE>")
          assert(expectedMove == move.get)
        }
      }

      describe("When it involves a WildCard in it") {
        it("Should return the right move") {
          val expectedMove = Move(List(THREE_Heart(11), THREE_Spade(11), JACK_Heart, JACK_Spade))
          val move = player.parseUserLine("<3(11),spade> <3(11),HEARt> <j,SPADE> <J,heart>")
          assert(expectedMove == move.get)
        }
      }

      describe("When it is fully comprised of WildCards") {
        it("Should return the right move") {
          val expectedMove = Move(List(THREE_Diamond(11), THREE_Heart(11)))
          val move = player.parseUserLine("<3(11),heart> <3(11),diAMONd>")
          assert(expectedMove == move.get)
        }
      }

      describe("When it is 2 (SpecialCard)") {
        it("Should return the right move") {
          val expectedMove = Move(List(TWO_Club, TWO_Heart))
          val move = player.parseUserLine("<2,heart> <2,CLUB>")
          assert(expectedMove == move.get)
        }
      }

      describe("When it is a Joker") {
        it("Should return the BlackJoker as the right move") {
          val expectedMove = Move(List(BlackJoker))
          val move = player.parseUserLine("<black_JokER>")
          assert(expectedMove == move.get)
        }

        it("Should return the RedJoker as the right move") {
          val expectedMove = Move(List(RedJoker))
          val move = player.parseUserLine("<ReD_JokER>")
          assert(expectedMove == move.get)
        }
      }
    }

    describe("Tests for getWorstCards") {
      val hand = Hand(List(THREE_Club, THREE_Spade, FOUR_Heart, SIX_Diamond, TEN_Heart, KING_Club, ACE_Heart, TWO_Spade, BlackJoker))
      val player = Player("Test", hand)

      describe("When hand is empty") {
        it("Should return empty list") {
          val h = Hand(List.empty)
          val p = Player("t", h)
          assert(p.getWorstCards(0) == List.empty)
          assert(p.getWorstCards(1) == List.empty)
          assert(p.getWorstCards(2) == List.empty)
        }
      }

      it("Should return empty list when parameter is 0") {
        assert(player.getWorstCards(0) == List.empty)
      }

      it("Should return expected list of normalcards size 1 when parameter is 1") {
        assert(player.getWorstCards(1) == List(FOUR_Heart))
      }

      it("Should return expected list of normalcards size 2 when parameter is 2") {
        assert(player.getWorstCards(2) == List(FOUR_Heart, SIX_Diamond))
      }

      describe("When there are no normal cards to return") {
        it("Should return worst non-normal cards as they would be in a sorted hand meant to be given away") {
          val newHand = Hand(List(THREE_Club, THREE_Spade, TWO_Club, TWO_Spade, RedJoker, BlackJoker))
          val newPlayer = Player("Test", newHand)
          assert(newPlayer.getWorstCards(3) == List(TWO_Club, THREE_Club, THREE_Spade))
          assert(newPlayer.getWorstCards(6) == List(TWO_Club, THREE_Club, THREE_Spade, TWO_Spade, RedJoker, BlackJoker))
        }
      }
    }

    describe("Tests for getBestCards") {
      val hand = Hand(List(THREE_Club, THREE_Spade, FOUR_Heart, SIX_Diamond, TEN_Heart, KING_Club, ACE_Heart, TWO_Diamond, TWO_Spade, BlackJoker))
      val player = Player("Test", hand)

      describe("When hand is empty") {
        it("Should return empty list") {
          val h = Hand(List.empty)
          val p = Player("t", h)
          assert(p.getBestCards(0) == List.empty)
          assert(p.getBestCards(1) == List.empty)
          assert(p.getBestCards(2) == List.empty)
        }
      }

      it("Should return empty list when parameter is 0") {
        assert(player.getBestCards(0) == List.empty)
      }

      it("Should return expected list of normalcards size 1 when parameter is 1") {
        assert(player.getBestCards(1) == List(BlackJoker))
      }

      it("Should return expected list of normalcards size 2 when parameter is 2") {
        assert(player.getBestCards(2) == List(TWO_Spade, BlackJoker))
      }

      it("Should return expected list of normalcards size 4 when parameter is 4") {
        assert(player.getBestCards(4) == List(THREE_Club, THREE_Spade, TWO_Spade, BlackJoker))
      }

      it("Should return expected list of normalcards size 5 when parameter is 5") {
        assert(player.getBestCards(5) == List( TWO_Diamond, THREE_Club, THREE_Spade, TWO_Spade, BlackJoker))
      }

      describe("When there are no special cards to return") {
        it("Should return best normal cards as they would be in a sorted hand meant to be given away") {
          val newHand = Hand(List(SEVEN_Heart, EIGHT_Diamond, EIGHT_Heart, KING_Club, KING_Heart, KING_Spade, ACE_Club))
          val newPlayer = Player("Test", newHand)
          assert(newPlayer.getBestCards(3) == List(KING_Heart, KING_Spade, ACE_Club))
          assert(newPlayer.getBestCards(7) == List(SEVEN_Heart, EIGHT_Diamond, EIGHT_Heart, KING_Club, KING_Heart, KING_Spade, ACE_Club))
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
