import FaceValue._
import Suits._
import org.scalatest.FunSpec

class GameUtilitiesTest extends FunSpec {

  describe("tests for getListsOfSimilarCards"){

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

  }


//    test("getListsOfSimilarCards with a hand of size 1") {
//
//    }
//
//    test("getListsOfSimilarCards with a hand of size 2") {
//
//    }
//
//    test("getListsOfSimilarCards with a hand of size 5") {
//
//    }



}
