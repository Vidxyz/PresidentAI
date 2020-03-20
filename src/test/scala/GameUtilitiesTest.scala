import FaceValue._
import Suits._
import org.scalatest.FunSpec

import scala.util.Random

class GameUtilitiesTest extends FunSpec {

  describe("tests for dealHands()") {
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

  describe("tests for sortCards()") {
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

    describe("When list of card consists only of an arbitrary set of 11 cards"){
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
          SpecialCard(TWO, Diamond),
          Joker,
          Joker
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
            NormalCard(JACK, Club), SpecialCard(TWO, Heart), Joker))
          val expectedResult = List(
            List(NormalCard(FOUR, Spade)),
            List(NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)),
            List(NormalCard(JACK, Club)),
            List(SpecialCard(TWO, Heart)),
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
          List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club), SpecialCard(TWO, Heart), SpecialCard(TWO, Spade)),
          List(Joker, Joker),
        )
        assert(GameUtilities.getListsOfSimilarCards(Consants.sortedHandWithAllCards) == expectedResult)
      }
    }

  }

  describe("tests for getAllMoves()") {

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
        it("should return 4 moves") {
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
        it("should return 7 moves") {
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

      describe("When the list comprises of four similar card") {
        it("should return 15 moves") {
          assert(GameUtilities.getAllMoves(List(
            List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade))
          ))
            ==
            Moves(List(
              Move(List(NormalCard(SEVEN, Diamond))),
              Move(List(NormalCard(SEVEN, Club))),
              Move(List(NormalCard(SEVEN, Heart))),
              Move(List(NormalCard(SEVEN, Spade))),
              Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club))),
              Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Heart))),
              Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Spade))),
              Move(List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart))),
              Move(List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Spade))),
              Move(List(NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade))),
              Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart))),
              Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Spade))),
              Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade))),
              Move(List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade))),
              Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade))),
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
          val intermediateLists = commonListOfLists :+ List(SpecialCard(TWO, Club))
          val expectedMoves = Moves(commonExpectedMovesList :+ Move(List(SpecialCard(TWO, Club))))
          assert(GameUtilities.getAllMoves(intermediateLists) == expectedMoves)
        }
      }

      describe("When the list comprises of multiple twos") {
        it("should return the expected response") {
          val intermediateLists = commonListOfLists :+ List(SpecialCard(TWO, Club), SpecialCard(TWO, Heart))

          val expectedMoves = Moves(commonExpectedMovesList ++ List(
                                            Move(List(SpecialCard(TWO, Club))),
                                            Move(List(SpecialCard(TWO, Heart))),
                                            Move(List(SpecialCard(TWO, Club), SpecialCard(TWO, Heart)))))
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

  describe("tests for getValidMoves()") {
    /*
    Test the following
    1. Empty gameState
    2. Single card on top - Get all singles + single 2s + Joker
    3. Double card on top -  Get all doubles + single 2s + Joker
    4. Triple card on top - get all triples + double 2s + Joker
    5. Quad on top - get all quads + triple 2s + Joker
     */
    val allMoves = Moves(List(
      Move(List(NormalCard(FIVE, Diamond))),
      Move(List(NormalCard(NINE, Spade))),
      Move(List(NormalCard(JACK, Heart))),
      Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club))),
      Move(List(NormalCard(EIGHT, Club), NormalCard(EIGHT, Heart))),
      Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart))),
      Move(List(NormalCard(QUEEN, Diamond), NormalCard(QUEEN, Club), NormalCard(QUEEN, Heart))),
      Move(List(NormalCard(TEN, Diamond), NormalCard(TEN, Club), NormalCard(TEN, Heart), NormalCard(TEN,Spade))),
      Move(List(SpecialCard(TWO, Club))),
      Move(List(SpecialCard(TWO, Club), SpecialCard(TWO, Heart))),
      Move(List(SpecialCard(TWO, Club), SpecialCard(TWO, Heart), SpecialCard(TWO, Spade))),
      Move(List(Joker))
    ))

    describe("When gameState is empty") {
      it("Should return allMoves as valid moves") {
        assert(GameUtilities.getValidMoves(allMoves, Move(List.empty)) == allMoves)
      }
    }

    describe("When gameState is a single card") {
      it("Should return only singles, single 2s and Jokers") {
        assert(GameUtilities.getValidMoves(allMoves, Move(List(NormalCard(FOUR, Heart))))
          == Moves(allMoves.moves.slice(0, 3) :+ allMoves.moves(8) :+ allMoves.moves.last))
      }
    }

    describe("When gameState is a double card") {
      it("Should return only doubles, single 2s and Jokers") {
        assert(GameUtilities.getValidMoves(allMoves, Move(List(NormalCard(FOUR, Heart), NormalCard(FOUR, Spade))))
          == Moves(allMoves.moves.slice(3, 5) :+ allMoves.moves(8) :+ allMoves.moves.last))
      }
    }

    describe("When gameState is a triple card") {
      it("Should return only triples, double 2s and Jokers") {
        assert(GameUtilities.getValidMoves(allMoves,
          Move(List(NormalCard(FOUR, Club), NormalCard(FOUR, Heart), NormalCard(FOUR, Spade))))
          == Moves(allMoves.moves.slice(5, 7) :+ allMoves.moves(9) :+ allMoves.moves.last))
      }
    }

    describe("When gameState is quadruple card") {
      it("Should return only quadruples, triple 2s and Jokers") {
        assert(GameUtilities.getValidMoves(allMoves,
          Move(List(NormalCard(FOUR, Diamond), NormalCard(FOUR, Club), NormalCard(FOUR, Heart), NormalCard(FOUR, Spade))))
          == Moves(List.empty :+ allMoves.moves(7) :+ allMoves.moves(10) :+ allMoves.moves.last))
      }
    }
  }

  describe("tests for isValidMove()") {
    // 4 base cases
    // One case where parity(move) != parity(gameState)
    // other special two cases
    describe("When move is empty") {
      it("Should return false") {
        assert(!GameUtilities.isValidMove(Move(List.empty), Move(List.empty)))
      }
    }

    describe("When gameState is empty") {
      it("Should return true for a single") {
        assert(GameUtilities.isValidMove(Move(List(NormalCard(SEVEN, Spade))), Move(List.empty)))
      }
      it("Should return true for a double") {
        assert(GameUtilities.isValidMove(Move(List(NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade))),
          Move(List.empty)))
      }
      it("Should return true for a triple") {
        assert(GameUtilities.isValidMove(Move(List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade))),
          Move(List.empty)))
      }
      it("Should return true for a quad") {
        assert(GameUtilities.isValidMove(
          Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade))),
          Move(List.empty)))
      }
      it("Should return true for a single2") {
        assert(GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Spade))),
          Move(List.empty)))
      }
      it("Should return true for a double2") {
        assert(GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Club), SpecialCard(TWO, Spade))),
          Move(List.empty)))
      }
      it("Should return true for a triple2") {
        assert(GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Club), SpecialCard(TWO, Heart), SpecialCard(TWO, Spade))),
          Move(List.empty)))
      }
      it("Should return true for a quadruple2") {
        assert(GameUtilities.isValidMove(
          Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club), SpecialCard(TWO, Heart), SpecialCard(TWO, Spade))),
          Move(List.empty)))
      }
      it("Should return true for a Joker") {
        assert(GameUtilities.isValidMove(Move(List(Joker)), Move(List.empty)))
      }
    }

    describe("When gameState is Joker") {
      it("Should return false") {
        assert(!GameUtilities.isValidMove(Move(List(NormalCard(SEVEN, Spade))), Move(List(Joker))))
      }
    }

    describe("When move is Joker") {
      it("Should return true for a single") {
        assert(GameUtilities.isValidMove(Move(List(Joker)),
          Move(List(NormalCard(ACE, Diamond)))))
      }
      it("Should return true for a double") {
        assert(GameUtilities.isValidMove(Move(List(Joker)),
          Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Heart)))))
      }
      it("Should return true for a triple") {
        assert(GameUtilities.isValidMove(Move(List(Joker)),
          Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Heart), NormalCard(ACE, Club)))))
      }
      it("Should return true for a quadruple") {
        assert(GameUtilities.isValidMove(Move(List(Joker)),
          Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Heart), NormalCard(ACE, Club), NormalCard(ACE, Spade)))))
      }
      it("Should return true for a single2") {
        assert(GameUtilities.isValidMove(Move(List(Joker)),
          Move(List(SpecialCard(TWO, Diamond)))))
      }
      it("Should return true for a double2s") {
        assert(GameUtilities.isValidMove(Move(List(Joker)),
          Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Heart)))))
      }
      it("Should return true for a triple2s") {
        assert(GameUtilities.isValidMove(Move(List(Joker)),
          Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Heart), SpecialCard(TWO, Club)))))
      }
      it("Should return true for a quad2s") {
        assert(GameUtilities.isValidMove(Move(List(Joker)),
          Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Heart), SpecialCard(TWO, Club), SpecialCard(TWO, Spade)))))
      }
    }

    describe("When move involves a two in it") {
      // Single two against single card
      // Single two against double card
      // Two 2s against triple card
      // Three 2s against quadruple cards
      // Single higher two against single 2
      // Better pair2s should burn lower pair2s
      describe("When move involves a single 2") {
        it("should return true when gameState is a high single") {
          assert(GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Club))), Move(List(NormalCard(ACE, Spade)))))
        }
        it("should return true when gameState is a high double") {
          assert(GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Club))),
            Move(List(NormalCard(ACE, Heart), NormalCard(ACE, Spade)))))
        }
        it("should return true when gameState is a two of lesser suit (Suit burn)") {
          assert(GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Club))), Move(List(SpecialCard(TWO, Diamond)))))
        }
        it("should return false when gameState is a two of lesser suit (No Suit burn)") {
          assert(!GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Diamond))), Move(List(SpecialCard(TWO, Club)))))
        }
        it("should return false when gameState is a double2") {
          assert(!GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Diamond))),
            Move(List(SpecialCard(TWO, Club), SpecialCard(TWO, Spade)))))
        }
        it("should return false when gameState is a triple") {
          assert(!GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Spade))),
            Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart)))))
        }
        it("should return false when gameState is a quadruple") {
          assert(!GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Spade))),
            Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))))
        }
      }

      describe("When move involves two 2s") {
        it("should return true when gameState is a high triple") {
          assert(GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club))),
            Move(List(NormalCard(ACE, Club), NormalCard(ACE, Heart), NormalCard(ACE, Spade)))))
        }
        it("should return true when gameState is two 2s of lesser suit (Suit burn)") {
          assert(GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Club), SpecialCard(TWO, Spade))),
            Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club)))))
        }
        it("should return false when gameState is two 2s of lesser suit (No Suit burn)") {
          assert(!GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Club), SpecialCard(TWO, Heart))),
            Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Spade)))))
        }
        it("should return false when gameState is a triple2") {
          assert(!GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Diamond))),
            Move(List(SpecialCard(TWO, Club), SpecialCard(TWO, Heart), SpecialCard(TWO, Spade)))))
        }
        it("should return false when gameState is a single2") {
          assert(!GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Heart))),
            Move(List(SpecialCard(TWO, Club)))))
        }
        it("should return false when gameState is a single") {
          assert(!GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Heart), SpecialCard(TWO, Spade))),
            Move(List(NormalCard(SEVEN, Diamond)))))
        }
        it("should return false when gameState is a double") {
          assert(!GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Heart), SpecialCard(TWO, Spade))),
            Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club)))))
        }
        it("should return false when gameState is a quadruple") {
          assert(!GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Heart), SpecialCard(TWO, Spade))),
            Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))))
        }
      }

      describe("When move involves three 2s") {
        it("should return true when gameState is a high quadruple") {
          assert(GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club))),
            Move(List(NormalCard(ACE, Club), NormalCard(ACE, Heart), NormalCard(ACE, Spade)))))
        }
        // No suit burns
        it("should return false when gameState is a single2") {
          assert(!GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Heart), SpecialCard(TWO, Spade))),
            Move(List(SpecialCard(TWO, Club)))))
        }
        it("should return false when gameState is a double2") {
          assert(!GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Heart), SpecialCard(TWO, Spade))),
            Move(List(SpecialCard(TWO, Club), SpecialCard(TWO, Club)))))
        }
        it("should return false when gameState is a quad2") {
          assert(!GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Heart), SpecialCard(TWO, Spade))),
            Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club), SpecialCard(TWO, Heart), SpecialCard(TWO, Spade)))))
        }
        it("should return false when gameState is a single") {
          assert(!GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Club), SpecialCard(TWO, Heart), SpecialCard(TWO, Spade))),
            Move(List(NormalCard(SEVEN, Diamond)))))
        }
        it("should return false when gameState is a double") {
          assert(!GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Club), SpecialCard(TWO, Heart), SpecialCard(TWO, Spade))),
            Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club)))))
        }
        it("should return false when gameState is a triple") {
          assert(!GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Club), SpecialCard(TWO, Heart), SpecialCard(TWO, Spade))),
            Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart)))))
        }
      }

    }

    describe("When move involves regular cards with a defined gameState of certain parity") {

      describe("When gameState is a single") {
        it("Should return true if move is a higher single") {
          assert(GameUtilities.isValidMove(Move(List(NormalCard(SEVEN, Club))), Move(List(NormalCard(SIX, Spade)))))
        }
        it("Should return true if move is a higher single of the same value (suit burn)") {
          assert(GameUtilities.isValidMove(Move(List(NormalCard(SEVEN, Club))), Move(List(NormalCard(SEVEN, Diamond)))))
        }
        it("Should return false if move is a lower single") {
          assert(!GameUtilities.isValidMove(Move(List(NormalCard(SIX, Club))), Move(List(NormalCard(SEVEN, Spade)))))
        }
        it("Should return false if move is not a single") {
          assert(!GameUtilities.isValidMove(Move(List(NormalCard(SIX, Club), NormalCard(SIX, Spade))),
            Move(List(NormalCard(SEVEN, Spade)))))
        }
      }

      describe("When gameState is a double") {
        it("Should return true if move is a higher double") {
          assert(GameUtilities.isValidMove(Move(List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart))),
            Move(List(NormalCard(SIX, Heart), NormalCard(SIX, Spade)))))
        }
        it("Should return true if move is a double single of the same value (suit burn)") {
          assert(GameUtilities.isValidMove(Move(List(NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade))),
            Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club)))))
        }
        it("Should return false if move is a lower double") {
          assert(!GameUtilities.isValidMove(Move(List(NormalCard(SIX, Club), NormalCard(SIX, Heart))),
            Move(List(NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))))
        }
        it("Should return false if move is not a double") {
          assert(!GameUtilities.isValidMove(Move(List(NormalCard(SEVEN, Club))),
            Move(List(NormalCard(SIX, Heart), NormalCard(SIX, Spade)))))
          assert(!GameUtilities.isValidMove(Move(List(NormalCard(SIX, Club), NormalCard(SIX, Heart), NormalCard(SIX, Spade))),
            Move(List(NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))))
        }
      }

      describe("When gameState is a triple") {
        it("Should return true if move is a higher triple") {
          assert(GameUtilities.isValidMove(Move(List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade))),
            Move(List(NormalCard(SIX, Club), NormalCard(SIX, Heart), NormalCard(SIX, Spade)))))
        }
        // No suit burns for triples... yet
        it("Should return false if move is a lower triple") {
          assert(!GameUtilities.isValidMove(Move(List(NormalCard(SIX, Club), NormalCard(SIX, Heart), NormalCard(SIX, Spade))),
            Move(List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))))
        }
        it("Should return false if move is not a triple") {
          assert(!GameUtilities.isValidMove(Move(List(NormalCard(SEVEN, Club))),
            Move(List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))))
          assert(!GameUtilities.isValidMove(Move(List(NormalCard(SIX, Club), NormalCard(SIX, Heart))),
            Move(List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))))
          assert(!GameUtilities.isValidMove(Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club), NormalCard(SIX, Heart), NormalCard(SIX, Spade))),
            Move(List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))))
        }
      }

      describe("When gameState is a quadruple") {
        it("Should return true if move is a higher quadruple") {
          assert(GameUtilities.isValidMove(Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade))),
            Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club), NormalCard(SIX, Heart), NormalCard(SIX, Spade)))))
        }
        // No suit burns for triples... yet
        it("Should return false if move is a lower quadruple") {
          assert(!GameUtilities.isValidMove(Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club), NormalCard(SIX, Heart), NormalCard(SIX, Spade))),
            Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))))
        }
        it("Should return false if move is not a quadruple") {
          assert(!GameUtilities.isValidMove(Move(List(NormalCard(SEVEN, Club))),
            Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))))
          assert(!GameUtilities.isValidMove(Move(List(NormalCard(SIX, Club), NormalCard(SIX, Heart))),
            Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))))
          assert(!GameUtilities.isValidMove(Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club), NormalCard(SIX, Heart))),
            Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))))
        }
      }

    }
  }

  describe("tests for checkIfBetter()") {
    // Check if true for card that is better
    // Check if true for suit burn
    // Check if false for  card that is worse
    // Check for  same  card
    // Check  for one empty card
    // Check  for both empty cards
    // check lowest 2 better than highest normal card
    // Check 2 suit burn
    // check joker higher than everything
    // 2 jokers case
    describe("When moves in question comprise of single cards") {

      describe("When move1 is 8-Heart and move2 is 7-spade") {
        it("Should return true") {
          assert(GameUtilities.checkIfBetter(Move(List(NormalCard(EIGHT, Heart))),
            Move(List(NormalCard(SEVEN, Spade)))))
        }
      }

      describe("When move1 is 8-Heart and move2 is 8-Club") {
        it("Should return true") {
          assert(GameUtilities.checkIfBetter(Move(List(NormalCard(EIGHT, Heart))),
            Move(List(NormalCard(EIGHT, Club)))))
        }
      }

      describe("When move1 is 7-Heart and move2 is 8-spade") {
        it("Should return false") {
          assert(!GameUtilities.checkIfBetter(Move(List(NormalCard(SEVEN, Heart))),
            Move(List(NormalCard(EIGHT, Spade)))))
        }
      }

      describe("When move1 is 8-Heart and move2 is ALSO 8-heart") {
        it("Should return false") {
          assert(!GameUtilities.checkIfBetter(Move(List(NormalCard(EIGHT, Heart))),
            Move(List(NormalCard(EIGHT, Heart)))))
        }
      }

      describe("When move1 is 2-diamond and move2 is Ace-Spade") {
        it("Should return true") {
          assert(GameUtilities.checkIfBetter(Move(List(SpecialCard(TWO, Diamond))), Move(List(NormalCard(ACE, Spade)))))
        }
      }

      describe("When move1 is 2-spade and move2 is 2-Heart") {
        it("Should return true") {
          assert(GameUtilities.checkIfBetter(Move(List(SpecialCard(TWO, Spade))), Move(List(SpecialCard(TWO, Heart)))))
        }
      }

      describe("When move1 is Joker and move2 is 2-Spade") {
        it("Should return true") {
          assert(GameUtilities.checkIfBetter(Move(List(Joker)), Move(List(SpecialCard(TWO, Spade)))))
        }
      }

      describe("When move1 is Joker and move2 is ALSO Joker") {
        it("Should return false") {
          assert(!GameUtilities.checkIfBetter(Move(List(Joker)), Move(List(Joker))))
        }
      }
    }

    describe("When moves in question comprise of doubles"){
      // Better double
      // Worse double
      // Single 2  > Double
      // Joker > any double
      describe("When move1 is double8s and move2 is double6s") {
        it("should return true"){
          assert(GameUtilities.checkIfBetter(
            Move(List(NormalCard(EIGHT, Diamond), NormalCard(EIGHT,Club))),
            Move(List(NormalCard(SIX, Diamond), NormalCard(SIX,Club)))))
        }
      }

      describe("When move1 is doubl68s and move2 is double8s") {
        it("should return false"){
          assert(!GameUtilities.checkIfBetter(
            Move(List(NormalCard(SIX, Diamond), NormalCard(SIX,Club))),
            Move(List(NormalCard(EIGHT, Diamond), NormalCard(EIGHT,Club)))))
        }
      }

      describe("When move1 is single2 and move2 is doubleAces") {
        it("should return true"){
          assert(GameUtilities.checkIfBetter(
            Move(List(SpecialCard(TWO, Diamond))),
            Move(List(NormalCard(ACE, Diamond), NormalCard(ACE,Club)))))
        }
      }

      describe("When move1 is Joker and move2 is doubleAces") {
        it("should return true"){
          assert(GameUtilities.checkIfBetter(
            Move(List(Joker)),
            Move(List(NormalCard(ACE, Diamond), NormalCard(ACE,Club)))))
        }
      }
    }

    describe("When moves in question comprise of triples"){
      // higher triple
      // two 2s higher than triple
      // suit burn on twos
      // joker > any triple
      describe("When move1 is tripleAces and move2 is triple9s") {
        it("should return true"){
          assert(GameUtilities.checkIfBetter(
            Move(List(NormalCard(ACE, Diamond), NormalCard(ACE,Club), NormalCard(ACE,Heart))),
            Move(List(NormalCard(NINE, Club), NormalCard(NINE, Heart), NormalCard(NINE, Spade)))))
        }
      }

      describe("When move1 is two2s and move2 is tripleAces") {
        it("should return true"){
          assert(GameUtilities.checkIfBetter(
            Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO,Club))),
            Move(List(NormalCard(ACE, Club), NormalCard(ACE, Heart), NormalCard(ACE, Spade)))))
        }
      }

      describe("When move1 is two2s and move2 is two2s of lower suit") {
        it("should return true"){
          assert(GameUtilities.checkIfBetter(
            Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO,Spade))),
            Move(List(SpecialCard(TWO, Club), SpecialCard(TWO,Heart)))))
        }
      }

      describe("When move1 is Joker and move2 is tripleAces") {
        it("should return true"){
          assert(GameUtilities.checkIfBetter(
            Move(List(Joker)),
            Move(List(NormalCard(ACE, Diamond), NormalCard(ACE,Club), NormalCard(ACE,Heart)))))
        }
      }
    }

    describe("When moves in question comprise of quadruples"){
      // higher quad
      // three 2s higher than quad
      // joker > any quad
      describe("When move1 is quadAces and move2 is quad9s") {
        it("should return true"){
          assert(GameUtilities.checkIfBetter(
            Move(List(NormalCard(ACE, Diamond), NormalCard(ACE,Club), NormalCard(ACE,Heart), NormalCard(ACE,Spade))),
            Move(List(NormalCard(NINE, Diamond), NormalCard(NINE, Club), NormalCard(NINE, Heart), NormalCard(NINE, Spade)))))
        }
      }

      describe("When move1 is two2s and move2 is tripleAces") {
        it("should return true"){
          assert(GameUtilities.checkIfBetter(
            Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO,Club), SpecialCard(TWO, Heart))),
            Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Club), NormalCard(ACE, Heart), NormalCard(ACE, Spade)))))
        }
      }

      describe("When move1 is Joker and move2 is tripleAces") {
        it("should return true"){
          assert(GameUtilities.checkIfBetter(
            Move(List(Joker)),
            Move(List(NormalCard(ACE, Diamond), NormalCard(ACE,Club), NormalCard(ACE,Heart), NormalCard(ACE,Spade)))))
        }
      }
    }

  }

  describe("tests for getHeuristicValue()") {

    // TEST FOR EXPERIMENT
    // TODO - delete this once done. Currently, a 2Diamond is favored over a 4Spade, this is WRONG and needs changing
    it("It is a test") {
      val validMove1 = Move(List(SpecialCard(TWO, Diamond)))
      val validMove2 = Move(List(NormalCard(FOUR, Spade)))
//      val validMove2 = Move(List(NormalCard(ACE, Club), NormalCard(ACE, Spade)))
      val validMove3 = Move(List(NormalCard(QUEEN, Club), NormalCard(QUEEN, Heart), NormalCard(QUEEN, Spade)))
      val gameState = Move(List.empty)
//      val gameState = Move(List(NormalCard(THREE, Club), NormalCard(THREE, Spade)))/
      println(GameUtilities.getHeuristicValue(validMove1, gameState, .3875956999999997))
      println(GameUtilities.getHeuristicValue(validMove2, gameState))
      println(GameUtilities.getHeuristicValue(validMove3, gameState))
    }
    // TODO - delete above when done

    describe("When move involves a Joker") {
      it("should return 0") {
        assert(GameUtilities.getHeuristicValue(Move(List(Joker)), Move(List.empty)) == 0)
      }
    }

    describe("When move involves a 2") {
      describe("When it is a single 2") {
        it("should return 0") {
          assert(GameUtilities.getHeuristicValue(
            Move(List(SpecialCard(TWO, Diamond))),
            Move(List.empty)) == 0)
        }
      }
      describe("When it is double 2s") {
        it("should return 0") {
          assert(GameUtilities.getHeuristicValue(
            Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Heart))),
            Move(List.empty)) == 0)
        }
      }
      describe("When it is triple 2s") {
        it("should return 0") {
          assert(GameUtilities.getHeuristicValue(
            Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Heart), SpecialCard(TWO, Spade))),
            Move(List.empty)) == 0)
        }
      }
    }

    describe("When gameState is Empty") {
      val emptyGameState = Move(List.empty)

      describe("When validMove is a single 4")  {
        it("should return value") {
          assert(GameUtilities.getHeuristicValue(Move(List(NormalCard(FOUR,Heart))), emptyGameState) == 0.25)
        }
      }

      describe("When validMove is double4s")  {
        it("should return value") {
          assert(GameUtilities.getHeuristicValue(
            Move(List(NormalCard(FOUR,Heart), NormalCard(FOUR,Spade))),
            emptyGameState) == 0.375)
        }
      }

      describe("When validMove is triple4s")  {
        it("should return value") {
          assert(GameUtilities.getHeuristicValue(
            Move(List(NormalCard(FOUR,Club), NormalCard(FOUR,Heart), NormalCard(FOUR,Spade))),
            emptyGameState) == 0.5)
        }
      }

      describe("When validMove is quad4s")  {
        it("should return value") {
          assert(GameUtilities.getHeuristicValue(
            Move(List(NormalCard(FOUR,Diamond), NormalCard(FOUR,Club),
              NormalCard(FOUR,Heart), NormalCard(FOUR,Spade))),
            emptyGameState) == 0.625)
        }
      }
    }

    describe("When gameState is a single") {
      it("should return the right value"){
        val gameState =  Move(List(NormalCard(FIVE, Spade)))
        val validMove = Move(List(NormalCard(NINE, Diamond)))
        assert(GameUtilities.getHeuristicValue(validMove, gameState) == 0.25)
      }
    }

    describe("When gameState is a double") {
      it("should return the right value"){
        val gameState =  Move(List(NormalCard(FIVE, Club), NormalCard(FIVE, Spade)))
        val validMove = Move(List(NormalCard(NINE, Diamond), NormalCard(NINE, Spade)))
        assert(GameUtilities.getHeuristicValue(validMove, gameState) == 0.375)
      }
    }

    describe("When gameState is a triple") {
      it("should return the right value"){
        val gameState =  Move(List(NormalCard(FIVE, Club), NormalCard(FIVE, Heart), NormalCard(FIVE, Spade)))
        val validMove = Move(List(NormalCard(NINE, Diamond), NormalCard(NINE, Club),  NormalCard(NINE, Spade)))
        assert(GameUtilities.getHeuristicValue(validMove, gameState) == 0.5)
      }
    }

    describe("When gameState is a quadruple") {
      it("should return the right value"){
        val gameState =  Move(List(NormalCard(FIVE, Diamond), NormalCard(FIVE, Club),
                          NormalCard(FIVE, Heart), NormalCard(FIVE, Spade)))
        val validMove = Move(List(NormalCard(NINE, Diamond), NormalCard(NINE, Club),
                          NormalCard(NINE, Heart),  NormalCard(NINE, Spade)))
        assert(GameUtilities.getHeuristicValue(validMove, gameState) == 0.625)
      }
    }
  }

  describe("tests for getNextMove()"){

    implicit val playerIndicators: PlayerIndicators = PlayerIndicators(Hand(List.empty))

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
        it("Should pick the slightly higher triple") {
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

  describe("tests for getNextGameState()") {
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

      it("Should be replaced by a 2 when a single 2 is played") {
        val playedMove = Move(List(SpecialCard(TWO, Club)))
        assert(GameUtilities.getNextGameState(single7, Some(playedMove)) == playedMove)
      }

      it("Should be a burn when a higher two is played on top of a lower 2") {
        val playedMove = Move(List(SpecialCard(TWO, Club)))
        assert(GameUtilities.getNextGameState(Move(List(SpecialCard(TWO, Diamond))), Some(playedMove)) == Move(List.empty))
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

      it("Should be a replaced by a 2 when a single 2 is played") {
        val playedMove = Move(List(SpecialCard(TWO, Heart)))
        assert(GameUtilities.getNextGameState(double6s, Some(playedMove)) == playedMove)
      }

      it("Should be a burn when a higher 2-2s is played on top of a lower 2-2s") {
        val gameState = Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club)))
        val playedMove = Move(List(SpecialCard(TWO, Heart), SpecialCard(TWO, Spade)))
        assert(GameUtilities.getNextGameState(gameState, Some(playedMove)) == Move(List.empty))
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

      it("Should be replaced by 2-2s when 2-2s are played") {
        val double2s = Move(List(SpecialCard(TWO, Heart), SpecialCard(TWO, Spade)))
        assert(GameUtilities.getNextGameState(triple7s, Some(double2s)) == double2s)
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
