import FaceValue._
import Suits._
import org.scalatest.FunSpec

class GameUtilitiesTest extends FunSpec {

  describe("tests for getListsOfSimilarCards()"){

    describe("When hand is empty") {
      it("Should return empty list") {
        assert(GameUtilities.getListsOfSimilarCards(Hand(List.empty)) == List.empty)
      }
    }

    describe("When hand size is 1") {
      it("Should return List(List(card))") {
        assert(GameUtilities.getListsOfSimilarCards(Hand(List(NormalCard(FOUR, Diamond)))) == List(List(NormalCard(FOUR, Diamond))))
      }
    }

    describe("When hand size is 2") {

      describe("And both the cards have the same faceValue") {
       it("Should return List[List(Card1, Card2)]") {
         val hand = Hand(List(NormalCard(FOUR, Diamond), NormalCard(FOUR, Spade)))
         assert(GameUtilities.getListsOfSimilarCards(hand) ==
           List(List(NormalCard(FOUR, Diamond), NormalCard(FOUR, Spade))))
       }
      }

      describe("And both the cards have different faceValues") {
       it("Should return List[List(Card1), List(Card2)] ") {
         val hand = Hand(List(NormalCard(FOUR, Diamond), NormalCard(SIX, Spade)))
         assert(GameUtilities.getListsOfSimilarCards(hand) ==
           List(List(NormalCard(FOUR, Diamond)), List(NormalCard(SIX, Spade))))
       }
      }

    }

    describe("When hand size is 6") {

      describe("And the hand is comprised of a random set of cards") {
        it("Should return the expected response"){
          val hand = Hand(List(NormalCard(FOUR, Spade), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade),
            NormalCard(JACK, Club), NormalCard(JACK, Heart), NormalCard(ACE, Club)))
          val expectedResult = List(
            List(NormalCard(FOUR, Spade)),
            List(NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)),
            List(NormalCard(JACK, Club), NormalCard(JACK, Heart)),
            List(NormalCard(ACE, Club))
          )
          assert(GameUtilities.getListsOfSimilarCards(hand) == expectedResult)
        }
      }

      describe("And the hand is comprised cards including 2s and Jokers") {
        it("Should return the expected response"){
          val hand = Hand(List(NormalCard(FOUR, Spade), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade),
            NormalCard(JACK, Club), NormalCard(TWO, Heart), Joker))
          val expectedResult = List(
            List(NormalCard(FOUR, Spade)),
            List(NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)),
            List(NormalCard(JACK, Club)),
            List(NormalCard(TWO, Heart)),
            List(Joker)
          )
          assert(GameUtilities.getListsOfSimilarCards(hand) == expectedResult)
        }
      }
    }

    describe("When the hand is comprised of all possible cards") {
      it("Should return 13 sets of four cards of each suit and 1 set of two Jokers"){
        val expectedResult = List(
          List(NormalCard(THREE, Diamond), NormalCard(THREE, Club), NormalCard(THREE, Heart), NormalCard(THREE, Spade)),
          List(NormalCard(FOUR, Diamond), NormalCard(FOUR, Club), NormalCard(FOUR, Heart), NormalCard(FOUR, Spade)),
          List(NormalCard(FIVE, Diamond), NormalCard(FIVE, Club), NormalCard(FIVE, Heart), NormalCard(FIVE, Spade)),
          List(NormalCard(SIX, Diamond), NormalCard(SIX, Club), NormalCard(SIX, Heart), NormalCard(SIX, Spade)),
          List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)),
          List(NormalCard(EIGHT, Diamond), NormalCard(EIGHT, Club), NormalCard(EIGHT, Heart), NormalCard(EIGHT, Spade)),
          List(NormalCard(NINE, Diamond), NormalCard(NINE, Club), NormalCard(NINE, Heart), NormalCard(NINE, Spade)),
          List(NormalCard(TEN, Diamond), NormalCard(TEN, Club), NormalCard(TEN, Heart), NormalCard(TEN, Spade)),
          List(NormalCard(JACK, Diamond), NormalCard(JACK, Club), NormalCard(JACK, Heart), NormalCard(JACK, Spade)),
          List(NormalCard(QUEEN, Diamond), NormalCard(QUEEN, Club), NormalCard(QUEEN, Heart), NormalCard(QUEEN, Spade)),
          List(NormalCard(KING, Diamond), NormalCard(KING, Club), NormalCard(KING, Heart), NormalCard(KING, Spade)),
          List(NormalCard(ACE, Diamond), NormalCard(ACE, Club), NormalCard(ACE, Heart), NormalCard(ACE, Spade)),
          List(NormalCard(TWO, Diamond), NormalCard(TWO, Club), NormalCard(TWO, Heart), NormalCard(TWO, Spade)),
          List(Joker, Joker),
        )
        assert(GameUtilities.getListsOfSimilarCards(Consants.sortedHandWithAllCards) == expectedResult)
      }
    }

  }

  describe("tests for getNextMove()"){

    describe("When validMoves is empty"){
      it("Should return an Empty Move") {
        assert(GameUtilities.getNextMove(Moves(List.empty), Move(List.empty)) == None)
      }
    }

    // Test for the following
    // Lowest possible valid move amongst all singles
    // Lowest possible valid move amongst all doubles
    // Lowest possible valid move amongst all triples
    // Lowest possible valid move amongst all quads
    // Picks twos only if no other cards to pick
    // Picks joker only if nothing else to pick
    // Picks +1 higher double than a -1 lower single
    // Picks
    describe("When gameState is Empty") {

      describe("When there is a slightly higher double than a lower single") {
        it("Should pick the slightly higher double") {
          val double6s = Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club)))
          val validMoves: Moves = Moves(List(
            Move(List(NormalCard(FIVE, Spade))),
            double6s
          ))
          val gameState = Move(List.empty)
          assert(GameUtilities.getNextMove(validMoves, gameState).contains(double6s))
        }
      }

      describe("When there is a slightly higher triple than a lower double") {
        it("Should pick the slightly higher double") {
          val double5s = Move(List(NormalCard(FIVE, Club), NormalCard(FIVE, Spade)))
          val triple6s = Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club), NormalCard(SIX, Heart)))
          val validMoves: Moves = Moves(List(double5s, triple6s))
          val gameState = Move(List.empty)
          assert(GameUtilities.getNextMove(validMoves, gameState).contains(triple6s))
        }
      }

      describe("When there is a slightly higher quad than a lower triple") {
        it("Should pick the slightly higher quad") {
          val triple5s = Move(List(NormalCard(FIVE, Diamond), NormalCard(FIVE, Club), NormalCard(FIVE, Heart)))
          val quad8s = Move(List(NormalCard(EIGHT, Diamond), NormalCard(EIGHT, Club), NormalCard(EIGHT, Heart), NormalCard(EIGHT, Spade)))
          val validMoves: Moves = Moves(List(triple5s, quad8s))
          val gameState = Move(List.empty)
          assert(GameUtilities.getNextMove(validMoves, gameState).contains(quad8s))
        }
      }

    }
  }

  describe("tests for getNextGameState") {
    // test single, double, triple
    // test suit burns
    // test two's
    // test jokers
    // None moves
    describe("When gameState is a single card") {

      val single7 = Move(List(NormalCard(SEVEN, Club)))

      it("Should be a suit burn") {
        val better7 = Move(List(NormalCard(SEVEN, Spade)))
        assert(GameUtilities.getNextGameState(single7, Some(better7)) == Move(List.empty))
      }

      it("Should be replaced by a higher single card") {
        val higherJack = Move(List(NormalCard(JACK, Diamond)))
        assert(GameUtilities.getNextGameState(single7, Some(higherJack)) == higherJack)
      }

      it("Should be a burn when a single 2 is played") {
        assert(GameUtilities.getNextGameState(single7, Some(Move(List(NormalCard(TWO, Club))))) == Move(List.empty))
      }

      it("Should be a burn when a single Joker is played") {
        assert(GameUtilities.getNextGameState(single7, Some(Move(List(Joker)))) == Move(List.empty))
      }
    }

    describe("When gameState is a double (Double 6s)") {

      val double6s = Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club)))

      it("Should be a suit burn") {
        val better6s = Move(List(NormalCard(SIX, Heart), NormalCard(SIX, Spade)))
        assert(GameUtilities.getNextGameState(double6s, Some(better6s)) == Move(List.empty))
      }

      it("Should be replaced by a higher double (Double 7s") {
        val double7s = Move(List(NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))
        assert(GameUtilities.getNextGameState(double6s, Some(double7s)) == double7s)
      }

      it("Should be a burn when a single 2 is played") {
        assert(GameUtilities.getNextGameState(double6s, Some(Move(List(NormalCard(TWO, Heart))))) == Move(List.empty))
      }

      it("Should be a burn when a single Joker is played") {
        assert(GameUtilities.getNextGameState(double6s, Some(Move(List(Joker)))) == Move(List.empty))
      }
    }

    describe("When gameState is a triple (Triple 7s)") {

      // No suit burns yet because no wildcard 3s
      val triple7s = Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart)))

      it("Should be replaced by a higher triple (Triple 9s") {
        val triple9s = Move(List(NormalCard(NINE, Diamond), NormalCard(NINE, Heart), NormalCard(NINE, Spade)))
        assert(GameUtilities.getNextGameState(triple7s, Some(triple9s)) == triple9s)
      }

      it("Should be a burn when a two 2s is played") {
        val double2s = Move(List(NormalCard(TWO, Heart), NormalCard(TWO, Spade)))
        assert(GameUtilities.getNextGameState(triple7s, Some(double2s)) == Move(List.empty))
      }

      it("Should be a burn when a single Joker is played") {
        val joker = Move(List(Joker))
        assert(GameUtilities.getNextGameState(triple7s, Some(joker)) == Move(List.empty))
      }
    }

    describe("When nextValidMove is None") {
      it("should return the gameState itself") {
        val gameState = Move(List(NormalCard(KING, Diamond), NormalCard(KING, Spade)))
        assert(GameUtilities.getNextGameState(gameState, None) == gameState)
      }
    }
  }

}
