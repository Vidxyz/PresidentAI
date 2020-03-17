import FaceValue._
import Suits._
import org.scalatest.FunSpec

import scala.util.Random

class GameUtilitiesTest extends FunSpec {

  describe("tests for dealHands") {
    // Test that one person gets all the cards
    // Test 2 people getting non overlapping cards
    // Test 3 people same
    // Test 4 people uneven number of total cards
    // Test that 54 players get 1 card each
    // Test that 55+ players have at least 1 hand that is completely empty

    describe("When there are 2 players") {
      it("Each should be equal in size"){
        val dealtHands = GameUtilities.dealHands(2)
        assert(dealtHands.map(h => h.listOfCards.size).forall(s => s == dealtHands.head.listOfCards.size))
      }
      it("Should contain unique cards in each hand except for Jokers") {
        val dealtHands = GameUtilities.dealHands(2)
        val hand1 = dealtHands.head
        val hand2 = dealtHands.tail.head
        assert(hand1.listOfCards.filter(card => !(card == Joker)).forall(card => !(hand2.listOfCards.contains(card))))
        assert(hand2.listOfCards.filter(card => !(card == Joker)).forall(card => !(hand1.listOfCards.contains(card))))
      }
    }

    describe("When there are 3 players") {
      it("Each should be equal in size"){
        val dealtHands = GameUtilities.dealHands(3)
        assert(dealtHands.map(h => h.listOfCards.size).forall(s => s == dealtHands.head.listOfCards.size))
      }
      it("Should contain unique cards in each hand except for Jokers") {
        val dealtHands = GameUtilities.dealHands(3)
        val hand1 = dealtHands.head
        val hand2 = dealtHands(1)
        val hand3 = dealtHands(2)
        assert(hand1.listOfCards
          .filter(card => !(card == Joker))
          .forall(card => !(hand2.listOfCards.contains(card)) && !(hand3.listOfCards.contains(card))))
        assert(hand2.listOfCards
          .filter(card => !(card == Joker))
          .forall(card => !(hand1.listOfCards.contains(card)) && !(hand3.listOfCards.contains(card))))
        assert(hand3.listOfCards
          .filter(card => !(card == Joker))
          .forall(card => !(hand1.listOfCards.contains(card)) && !(hand2.listOfCards.contains(card))))
      }
    }

    describe("When there are 4 players") {
      it("Two hands should be of equal size, other two hands of equal size (13,13,14,14)") {
        val dealtHands = GameUtilities.dealHands(4)
        assert(dealtHands.map(h => h.listOfCards.size).count(s => s == 13) == 2)
        assert(dealtHands.map(h => h.listOfCards.size).count(s => s == 14) == 2)
      }
    }

    describe("When there are 54 players") {
      it("Each hand should have exactly one card in it") {
        val dealtHands = GameUtilities.dealHands(54)
        assert(dealtHands.map(hand => hand.listOfCards.size).forall(s => s == 1))
      }
    }

    describe("When there are 55 players") {
      it("At least one hand should be empty") {
        val dealtHands = GameUtilities.dealHands(55)
        assert(dealtHands.map(hand => hand.listOfCards.size).contains(0))
      }
    }

  }

  describe("tests for sortCards") {
    // Test sorting 0 cards
    // Test sorting 1 card
    // Test sorting 2 cards
    // Test sorting 10 cards
    // Test sorting 54 cards
    describe("When list of cards is empty") {
      it("should return empty list") {
        assert(GameUtilities.sortCards(List.empty) == List.empty)
      }
    }

    describe("When list of card consists only of one card"){
      it("should return the same list") {
        val sampleList = List(NormalCard(EIGHT, Heart))
        assert(GameUtilities.sortCards(sampleList) == sampleList)
      }
    }

    describe("When list of card consists only of exactly 2 cards"){
      it("should return a list with the higher card at the tail.head") {
        val sampleList = List(NormalCard(EIGHT, Heart), NormalCard(FOUR, Diamond))
        assert(GameUtilities.sortCards(sampleList) == sampleList.reverse)
        assert(GameUtilities.sortCards(sampleList.reverse) == sampleList.reverse)
      }
    }

    describe("When list of card consists only of an arbitrary set of 10 cards"){
      it("should return a sorted list ") {
        val sortedList = List(
          NormalCard(FOUR, Club),
          NormalCard(SIX, Club),
          NormalCard(SEVEN, Heart),
          NormalCard(SEVEN, Spade),
          NormalCard(NINE, Diamond),
          NormalCard(JACK, Diamond),
          NormalCard(QUEEN, Club),
          NormalCard(QUEEN, Spade),
          NormalCard(TWO, Diamond),
          Joker,
        )
        assert(GameUtilities.sortCards(Random.shuffle(sortedList)) == sortedList)
      }
    }

    describe("When list of card consists only of full deck (54 cards)"){
      it("should return a sorted list ") {
        assert(GameUtilities.sortCards(Random.shuffle(Consants.sortedHandWithAllCards.listOfCards))
          == Consants.sortedHandWithAllCards.listOfCards)
      }
    }


  }

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

  describe("tests for getAllMoves") {

    describe("When input intermediate list is empty"){
      it("should return Moves(List.empty)") {
        assert(GameUtilities.getAllMoves(List.empty) == Moves(List.empty))
      }
    }

    describe("When input intermediate list is of size 1"){

      describe("When the list comprises of a single card") {
        it("should return a single move") {
          assert(GameUtilities.getAllMoves(List(List(NormalCard(SEVEN, Club))))
            == Moves(List(Move(List(NormalCard(SEVEN, Club))))))
        }
      }

      describe("When the list comprises of two similar card") {
        it("should return a two moves") {
          assert(GameUtilities.getAllMoves(List(
            List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Spade))
            ))
            ==
            Moves(List(
            Move(List(NormalCard(SEVEN, Club))),
            Move(List(NormalCard(SEVEN, Spade))),
            Move(List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Spade)))
            )))
        }
      }

      describe("When the list comprises of three similar card") {
        it("should return a two moves") {
          assert(GameUtilities.getAllMoves(List(
            List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade))
          ))
            ==
            Moves(List(
              Move(List(NormalCard(SEVEN, Club))),
              Move(List(NormalCard(SEVEN, Heart))),
              Move(List(NormalCard(SEVEN, Spade))),
              Move(List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart))),
              Move(List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Spade))),
              Move(List(NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade))),
              Move(List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade))),
            )))
        }
      }
    }

    describe("When input intermediate list is of size 5"){

      val commonListOfLists = List(
        List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart)),
        List(NormalCard(NINE, Diamond)),
        List(NormalCard(QUEEN, Club)),
        List(NormalCard(ACE, Diamond), NormalCard(ACE, Spade)))

      val commonExpectedMovesList = List(
        Move(List(NormalCard(SEVEN, Club))),
        Move(List(NormalCard(SEVEN, Heart))),
        Move(List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart))),
        Move(List(NormalCard(NINE, Diamond))),
        Move(List(NormalCard(QUEEN, Club))),
        Move(List(NormalCard(ACE, Diamond))),
        Move(List(NormalCard(ACE, Spade))),
        Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Spade))))

      describe("When the list comprises of a single two in it") {
        it("should return the expected response") {
          val intermediateLists = commonListOfLists :+ List(NormalCard(TWO, Club))
          val expectedMoves = Moves(commonExpectedMovesList :+ Move(List(NormalCard(TWO, Club))))
          assert(GameUtilities.getAllMoves(intermediateLists) == expectedMoves)
        }
      }

      describe("When the list comprises of multiple twos") {
        it("should return the expected response") {
          val intermediateLists = commonListOfLists :+ List(NormalCard(TWO, Club), NormalCard(TWO, Heart))

          val expectedMoves = Moves(commonExpectedMovesList ++ List(
                                            Move(List(NormalCard(TWO, Club))),
                                            Move(List(NormalCard(TWO, Heart))),
                                            Move(List(NormalCard(TWO, Club), NormalCard(TWO, Heart)))))
          assert(GameUtilities.getAllMoves(intermediateLists) == expectedMoves)
        }
      }

      describe("When the list comprises of a single Joker in it") {
        it("should return the expected response") {
          val intermediateLists = commonListOfLists :+ List(Joker)
          val expectedMoves = Moves(commonExpectedMovesList :+ Move(List(Joker)))
          assert(GameUtilities.getAllMoves(intermediateLists) == expectedMoves)
        }
      }

      describe("When the list comprises of a multiple Jokers in it") {
        it("should return the expected response") {
          val intermediateLists = commonListOfLists :+ List(Joker) :+ List(Joker)
          val expectedMoves = Moves(commonExpectedMovesList :+ Move(List(Joker)) :+ Move(List(Joker)))
          assert(GameUtilities.getAllMoves(intermediateLists) == expectedMoves)
        }
      }

    }

  }

  describe("tests for getValidMoves") {

  }

  describe("tests for isValidMove") {

  }

  describe("tests for checkIfBetter") {

  }

  describe("tests for getHeuristicValue") {

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
