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
  }

}
