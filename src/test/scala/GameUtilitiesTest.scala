import game.{BlackJoker, Bum, Card, GameUtilities, Hand, IllegalAssumedValueException, Move, Moves, Neutral, NormalCard, President, RedJoker, SpecialCard, ViceBum, VicePres, WildCard}
import org.scalatest.FunSpec
import utils.Constants

import scala.util.Random
import game.FaceValue._
import game.GameUtilities.{IllegalCardSuppliedException, IllegalMoveSuppliedException}
import game.Suits._
import player.Player
import utils.Constants._

class GameUtilitiesTest extends FunSpec {

  implicit def enhanceWithContainsDuplicates[T](s:List[T]) = new {
    def containsDuplicatesExcludingJokers ={
      (s.size - s.distinct.size > 1)
    }
  }

  describe("tests for generatePlayersAndDealHands()") {

    describe("When there are no players") {
      it("Should return empty List[Player]") {
        assert(GameUtilities.generatePlayersAndDealHands(List.empty) == List.empty)
      }
    }

    describe("When there is exactly one player") {
      it("Should return a List[Player] of size 1 with hand comprising of all unique 54 cards") {
        val result = GameUtilities.generatePlayersAndDealHands(List("Player1"))
        assert(result.size == 1)
        assert(result.head.hand.size == Constants.totalNumberOfCards)
        assert(result.head.name == "Player1")
        assert(!result.head.hand.listOfCards.containsDuplicatesExcludingJokers)
      }
    }

    describe("When there are 4 players") {
      it("Should return List[Player] of size 4, each having unique cards and two having 13 cards and two having 14 cards") {
        val listOfNames = List("p1", "p2", "p3", "p4")
        val result = GameUtilities.generatePlayersAndDealHands(listOfNames)
        // Verify size of resulting list
        assert(result.size == 4)
        // Check if listOfNames is created properly
        assert(result.map(p => p.name) == listOfNames)
        // Check two hands are size 13, two other of size 14
        assert(result.map(p => p.hand).map(h => h.listOfCards.size).count(s => s == 13) == 2)
        assert(result.map(p => p.hand).map(h => h.listOfCards.size).count(s => s == 14) == 2)
        // Check all hands are unqiue, excluding jokers
        assert(result.map(p => p.hand).map(h => !h.listOfCards.containsDuplicatesExcludingJokers).forall(x => x))
        // Check all hands cards add up to total
        assert(result.map(p => p.hand).flatMap(h => h.listOfCards).size == Constants.totalNumberOfCards)
      }
    }
  }

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
        assert(hand1.listOfCards.filter(card => !(card == BlackJoker)).forall(card => !(hand2.listOfCards.contains(card))))
        assert(hand2.listOfCards.filter(card => !(card == BlackJoker)).forall(card => !(hand1.listOfCards.contains(card))))
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
          .filter(card => !(card == BlackJoker))
          .forall(card => !(hand2.listOfCards.contains(card)) && !(hand3.listOfCards.contains(card))))
        assert(hand2.listOfCards
          .filter(card => !(card == BlackJoker))
          .forall(card => !(hand1.listOfCards.contains(card)) && !(hand3.listOfCards.contains(card))))
        assert(hand3.listOfCards
          .filter(card => !(card == BlackJoker))
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
          BlackJoker,
          BlackJoker
        )
        assert(GameUtilities.sortCards(Random.shuffle(sortedList)) == sortedList)
      }
    }

    describe("When list of card consists only of full deck (54 cards)"){
      it("should return a sorted list ") {
        assert(GameUtilities.sortCards(Random.shuffle(Constants.sortedHandWithAllCards.listOfCards))
          == Constants.sortedHandWithAllCards.listOfCards)
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
            NormalCard(JACK, Club), SpecialCard(TWO, Heart), BlackJoker))
          val expectedResult = List(
            List(NormalCard(FOUR, Spade)),
            List(NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)),
            List(NormalCard(JACK, Club)),
            List(SpecialCard(TWO, Heart)),
            List(BlackJoker)
          )
          assert(GameUtilities.getListsOfSimilarCards(hand) == expectedResult)
        }
      }
    }

    describe("When the hand is comprised of all possible cards") {
      it("Should return 13 sets of four cards of each suit and 1 set of two Jokers"){
        val expectedResult = List(
          List(WildCard(THREE, Diamond), WildCard(THREE, Club), WildCard(THREE, Heart), WildCard(THREE, Spade)),
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
          List(RedJoker),
          List(BlackJoker)
        )
        assert(GameUtilities.getListsOfSimilarCards(Constants.sortedHandWithAllCards) == expectedResult)
      }
    }

  }

  describe("tests for getAllMoves()") {

    describe("When input intermediate list is empty"){
      it("should return game.Moves(List.empty)") {
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

      describe("When the list comprises of a single game.Joker in it") {
        it("should return the expected response") {
          val intermediateLists = commonListOfLists :+ List(BlackJoker)
          val expectedMoves = Moves(commonExpectedMovesList :+ Move(List(BlackJoker)))
          assert(GameUtilities.getAllMoves(intermediateLists) == expectedMoves)
        }
      }

      describe("When the list comprises of a multiple Jokers in it") {
        it("should return the expected response") {
          val intermediateLists = commonListOfLists :+ List(BlackJoker) :+ List(BlackJoker)
          val expectedMoves = Moves(commonExpectedMovesList :+ Move(List(BlackJoker)) :+ Move(List(BlackJoker)))
          assert(GameUtilities.getAllMoves(intermediateLists) == expectedMoves)
        }
      }

    }

  }

  describe("tests for getValidMoves()") {
    /*
    Test the following
    1. Empty gameState
    2. Single card on top - Get all singles + single 2s + game.Joker
    3. Double card on top -  Get all doubles + single 2s + game.Joker
    4. Triple card on top - get all triples + double 2s + game.Joker
    5. Quad on top - get all quads + triple 2s + game.Joker
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
      Move(List(BlackJoker))
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
      it("Should return true for a game.Joker") {
        assert(GameUtilities.isValidMove(Move(List(BlackJoker)), Move(List.empty)))
      }
    }

    describe("When gameState is game.Joker") {
      it("Should return false") {
        assert(!GameUtilities.isValidMove(Move(List(NormalCard(SEVEN, Spade))), Move(List(BlackJoker))))
      }
    }

    describe("When move is game.Joker") {
      it("Should return true for a single") {
        assert(GameUtilities.isValidMove(Move(List(BlackJoker)),
          Move(List(NormalCard(ACE, Diamond)))))
      }
      it("Should return true for a double") {
        assert(GameUtilities.isValidMove(Move(List(BlackJoker)),
          Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Heart)))))
      }
      it("Should return true for a triple") {
        assert(GameUtilities.isValidMove(Move(List(BlackJoker)),
          Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Heart), NormalCard(ACE, Club)))))
      }
      it("Should return true for a quadruple") {
        assert(GameUtilities.isValidMove(Move(List(BlackJoker)),
          Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Heart), NormalCard(ACE, Club), NormalCard(ACE, Spade)))))
      }
      it("Should return true for a single2") {
        assert(GameUtilities.isValidMove(Move(List(BlackJoker)),
          Move(List(SpecialCard(TWO, Diamond)))))
      }
      it("Should return true for a double2s") {
        assert(GameUtilities.isValidMove(Move(List(BlackJoker)),
          Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Heart)))))
      }
      it("Should return true for a triple2s") {
        assert(GameUtilities.isValidMove(Move(List(BlackJoker)),
          Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Heart), SpecialCard(TWO, Club)))))
      }
      it("Should return true for a quad2s") {
        assert(GameUtilities.isValidMove(Move(List(BlackJoker)),
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
        it("should return true when gameState is a two of lesser suit (game.Suit burn)") {
          assert(GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Club))), Move(List(SpecialCard(TWO, Diamond)))))
        }
        it("should return false when gameState is a two of lesser suit (No game.Suit burn)") {
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
        it("should return true when gameState is two 2s of lesser suit (game.Suit burn)") {
          assert(GameUtilities.isValidMove(Move(List(SpecialCard(TWO, Club), SpecialCard(TWO, Spade))),
            Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club)))))
        }
        it("should return false when gameState is two 2s of lesser suit (No game.Suit burn)") {
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

      describe("When move1 is game.Joker and move2 is 2-Spade") {
        it("Should return true") {
          assert(GameUtilities.checkIfBetter(Move(List(BlackJoker)), Move(List(SpecialCard(TWO, Spade)))))
        }
      }

      describe("When move1 is game.Joker and move2 is ALSO game.Joker") {
        it("Should return false") {
          assert(!GameUtilities.checkIfBetter(Move(List(BlackJoker)), Move(List(BlackJoker))))
        }
      }
    }

    describe("When moves in question comprise of doubles"){
      // Better double
      // Worse double
      // Single 2  > Double
      // game.Joker > any double
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

      describe("When move1 is game.Joker and move2 is doubleAces") {
        it("should return true"){
          assert(GameUtilities.checkIfBetter(
            Move(List(BlackJoker)),
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

      describe("When move1 is game.Joker and move2 is tripleAces") {
        it("should return true"){
          assert(GameUtilities.checkIfBetter(
            Move(List(BlackJoker)),
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

      describe("When move1 is game.Joker and move2 is tripleAces") {
        it("should return true"){
          assert(GameUtilities.checkIfBetter(
            Move(List(BlackJoker)),
            Move(List(NormalCard(ACE, Diamond), NormalCard(ACE,Club), NormalCard(ACE,Heart), NormalCard(ACE,Spade)))))
        }
      }
    }

    describe("When moves in question involve the same faceValue with Wildcard/Normalcard") {
      val move1 = Move(List(THREE_Spade(7), SEVEN_Diamond, SEVEN_Heart))
      val move2 = Move(List(THREE_Heart(7), SEVEN_Club, SEVEN_Spade))

      it("Should return false when the gameState has the NormalCard") {
        assert(!GameUtilities.checkIfBetter(move1, move2))
      }

      it("Should return true when the gameState has the WildCard") {
        assert(GameUtilities.checkIfBetter(move2, move1))
      }
    }
  }

  describe("tests for cardOrderValue()") {
    it("should return the right value when supplied card is a NormalCard") {
      assert(GameUtilities.cardOrderValue(NormalCard(SIX, Heart)) == 14)
    }
    it("should return the right value when supplied card is a SpecialCard") {
      assert(GameUtilities.cardOrderValue(SpecialCard(TWO, Spade)) == 51)
    }
    it("should return the right value when supplied card is a Red Joker") {
      assert(GameUtilities.cardOrderValue(RedJoker) == 52)
    }
    it("should return the right value when supplied card is a Black Joker") {
      assert(GameUtilities.cardOrderValue(BlackJoker) == 53)
    }
    it("Should return the right value when supplied card is a WildCard assuming another card") {
      assert(GameUtilities.cardOrderValue(THREE_Spade(8)) == GameUtilities.cardOrderValue(EIGHT_Spade))
    }

  }

  describe("tests for isOnlySpecialMovesAvailable()") {
    describe("When only moves involving 2s are available") {
      it("Should return true") {
        val validMoves = Moves(List(
          Move(List(SpecialCard(TWO, Diamond))),
          Move(List(SpecialCard(TWO, Club))),
          Move(List(SpecialCard(TWO, Heart))),
          Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club))),
          Move(List(SpecialCard(TWO, Club), SpecialCard(TWO, Heart))),
          Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Heart))),
          Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club), SpecialCard(TWO, Heart)))
        ))
        assert(GameUtilities.isOnlySpecialMovesAvailable(validMoves))
      }
    }

    describe("When only moves involving a Joker are available") {
      it("Should return true") {
        val validMoves = Moves(List(
          Move(List(RedJoker)),
          Move(List(BlackJoker)),
          Move(List(RedJoker, BlackJoker)),
        ))
        assert(GameUtilities.isOnlySpecialMovesAvailable(validMoves))
      }
    }

    describe("When only moves involving NormalCards are available") {
      it("Should return false") {
        val validMoves = Moves(List(
          Move(List(NormalCard(FOUR, Diamond))),
          Move(List(NormalCard(SEVEN, Club))),
          Move(List(NormalCard(NINE, Heart))),
          Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club))),
          Move(List(NormalCard(EIGHT, Club), NormalCard(EIGHT, Heart))),
          Move(List(NormalCard(JACK, Diamond), NormalCard(JACK, Heart))),
          Move(List(NormalCard(KING, Diamond), NormalCard(KING, Club), NormalCard(KING, Heart)))
        ))
        assert(!GameUtilities.isOnlySpecialMovesAvailable(validMoves))
      }
    }

    describe("When moves involving all types of cards are available") {
      it("Should return false") {
        val validMoves = Moves(List(
          Move(List(WildCard(THREE, Diamond))),
          Move(List(NormalCard(SEVEN, Club))),
          Move(List(NormalCard(NINE, Heart))),
          Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club))),
          Move(List(NormalCard(EIGHT, Club), NormalCard(EIGHT, Heart))),
          Move(List(NormalCard(JACK, Diamond), NormalCard(JACK, Heart))),
          Move(List(NormalCard(KING, Diamond), NormalCard(KING, Club), NormalCard(KING, Heart))),
          Move(List(SpecialCard(TWO, Diamond))),
          Move(List(SpecialCard(TWO, Club))),
          Move(List(SpecialCard(TWO, Heart))),
          Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club))),
          Move(List(SpecialCard(TWO, Club), SpecialCard(TWO, Heart))),
          Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Heart))),
          Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club), SpecialCard(TWO, Heart))),
          Move(List(RedJoker)),
          Move(List(BlackJoker)),
          Move(List(RedJoker, BlackJoker)),
        ))
        assert(!GameUtilities.isOnlySpecialMovesAvailable(validMoves))
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

      it("Should be a burn when a single game.Joker is played") {
        assert(GameUtilities.getNextGameState(single7, Some(Move(List(BlackJoker)))) == Move(List.empty))
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

      it("Should be a burn when a single game.Joker is played") {
        assert(GameUtilities.getNextGameState(double6s, Some(Move(List(BlackJoker)))) == Move(List.empty))
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

      it("Should be a burn when a single game.Joker is played") {
        val joker = Move(List(BlackJoker))
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

  describe("tests for filterNonSpecialCardMoves()") {

    val listOfSpecialMoves = List(
      Move(List(SpecialCard(TWO, Diamond))),
      Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club))),
      Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club), SpecialCard(TWO, Heart))),
      Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club), SpecialCard(TWO, Heart), SpecialCard(TWO, Spade))))
    val listOfNormalMoves = List(
      Move(List(NormalCard(SEVEN, Diamond))),
      Move(List(NormalCard(NINE, Diamond), NormalCard(NINE, Club))),
      Move(List(NormalCard(JACK, Diamond), NormalCard(JACK, Club), NormalCard(JACK, Heart))),
      Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Club), NormalCard(ACE, Heart), NormalCard(ACE, Spade))))
    val listOfWilcardMoves = List(
      Move(List(THREE_Spade(7), SEVEN_Club)),
      Move(List(THREE_Diamond(9), THREE_Spade(9), NINE_Spade))
    )
    val validSpecialMoves = Moves(listOfSpecialMoves)
    val validNormalMoves = Moves(listOfNormalMoves)
    val validWildCardMoves = Moves(listOfWilcardMoves)
    val validNormalWildcardMoves = Moves(listOfWilcardMoves ++ listOfNormalMoves)

    describe("When validMoves comprise only of NormalCard moves") {
      it("Should return the validMoves itself") {
        assert(GameUtilities.filterNonSpecialCardMoves(validNormalMoves) == validNormalMoves)
      }
    }

    describe("When validMoves comprise only of WildCard moves") {
      it("Should return the validMoves itself") {
        assert(GameUtilities.filterNonSpecialCardMoves(validWildCardMoves) == validWildCardMoves)
      }
    }

    describe("When validMoves comprise only of SpecialCard moves") {
      it("Should return empty list") {
        assert(GameUtilities.filterNonSpecialCardMoves(validSpecialMoves).moves.isEmpty)
      }
    }

    describe("When validMoves comprise of both specialCard moves and NormalCard moves"){
      it("Should return list containing only NormalCard moves") {
        assert(GameUtilities.filterNonSpecialCardMoves(Moves(listOfSpecialMoves ++ listOfNormalMoves)) == validNormalMoves)
      }
    }

    describe("When validMoves comprise of both NormalCard and WildCard moves") {
      it("Should return list containing everything") {
        assert(GameUtilities.filterNonSpecialCardMoves(Moves(listOfWilcardMoves ++ listOfNormalMoves)) == validNormalWildcardMoves)
      }
    }

    describe("When validMoves comprise of both SpecialCard and WildCard moves") {
      it("Should return list containing only wilcardMoves") {
        assert(GameUtilities.filterNonSpecialCardMoves(Moves(listOfWilcardMoves ++ listOfSpecialMoves)) == validWildCardMoves)
      }
    }
  }

  describe("tests for getWildCardListFromIntermediateList()") {

    val intermediateList = List(
      List(FOUR_Heart),
      List(SIX_Diamond, SIX_Club),
      List(EIGHT_Club, EIGHT_Heart, EIGHT_Spade),
      List(TEN_Diamond, TEN_Club, TEN_Heart, TEN_Spade),
      List(TWO_Spade),
      List(BlackJoker)
    )

    describe("When intermediate list is empty") {
      it("Should return empty list") {
        val intermediateList = List.empty
        assert(GameUtilities.getWildCardListFromIntermediateList(intermediateList).isEmpty)
      }
    }

    describe("When intermediate list is a List(List.empty)") {
      it("Should return empty list") {
        val intermediateList = List(List.empty)
        assert(GameUtilities.getWildCardListFromIntermediateList(intermediateList).isEmpty)
      }
    }

    describe("When intermediateList does NOT have a list of cards with 3s in it") {
      it("Should return empty list") {
        assert(GameUtilities.getWildCardListFromIntermediateList(intermediateList).isEmpty)
      }
    }

    describe("When intermediateList has a list of card(s) with 3s in it") {

      describe("When the list of 3s is of size 1") {
        it("Should return a list of size 1") {
          val intermediate: List[List[Card]] = intermediateList :+ List(THREE_Diamond)
          assert(GameUtilities.getWildCardListFromIntermediateList(intermediate).size == 1)
        }
      }

      describe("When the list of 3s is of size 2") {
        it("Should return a list of size 2") {
          val intermediate: List[List[Card]] = intermediateList :+
            List(THREE_Diamond, THREE_Club)
          assert(GameUtilities.getWildCardListFromIntermediateList(intermediate).size == 2)
        }
      }

      describe("When the list of 3s is of size 3") {
        it("Should return a list of size 3") {
          val intermediate: List[List[Card]] = intermediateList :+
            List(THREE_Diamond, THREE_Club, THREE_Heart)
          assert(GameUtilities.getWildCardListFromIntermediateList(intermediate).size == 3)
        }
      }

      describe("When the list of 3s is of size 4") {
        it("Should return a list of size 4") {
          val intermediate: List[List[Card]] = intermediateList :+
            List(THREE_Diamond, THREE_Club, THREE_Heart, THREE_Spade)
          assert(GameUtilities.getWildCardListFromIntermediateList(intermediate).size == 4)
        }
      }
    }
  }

  describe("Tests for getNewHand()") {
    val currentHand = Hand(List(
      THREE_Club, THREE_Heart,
      SIX_Diamond, SIX_Heart,
      EIGHT_Club, TEN_Heart,
      ACE_Diamond, ACE_Heart,
      TWO_Diamond, BlackJoker))
    val player = Player("Test", currentHand)

    describe("When the movePlayed is none"){
      it("Should return the same hand") {
        assert(GameUtilities.getNewHand(player.hand, None) == currentHand)
      }
    }

    describe("When the movePlayed does not involve a card in the hand"){
      it("Should return the same hand when the move is composed of Normal Cards") {
        assert(GameUtilities.getNewHand(player.hand, Some(Move(List(SEVEN_Diamond, SEVEN_Spade))))
          == currentHand)
      }

      it("Should return the same hand when the move is composed of Special Cards") {
        assert(GameUtilities.getNewHand(player.hand, Some(Move(List(TWO_Club, TWO_Heart))))
          == currentHand)
      }

      it("Should return the same hand when the move is composed of WildCards") {
        assert(GameUtilities.getNewHand(player.hand, Some(Move(List(THREE_Diamond(8), THREE_Spade(8)))))
          == currentHand)
      }

    }

    describe("When the movePlayed involves a card in the hand"){

      describe("When the move played is a normalCard") {
        it("Should return the hand minus the played cards") {
          assert(GameUtilities.getNewHand(player.hand, Some(Move(List(ACE_Diamond, ACE_Heart))))
            == Hand(currentHand.listOfCards.slice(0,6) ++ currentHand.listOfCards.slice(8, 10)))
        }
      }

      describe("When the move played is a specialCard") {
        it("Should return the hand minus the played cards") {
          assert(GameUtilities.getNewHand(player.hand, Some(Move(List(TWO_Diamond))))
            == Hand(currentHand.listOfCards.slice(0,8) ++ currentHand.listOfCards.slice(9, 10)))
          assert(GameUtilities.getNewHand(player.hand, Some(Move(List(BlackJoker))))
            == Hand(currentHand.listOfCards.slice(0,9)))
        }

        it("Should only filter out BlackJoker joker when the hand has two in it and the move is a BlackJoker") {
          val hand = Hand(List(TWO_Club, RedJoker, BlackJoker))
          assert(GameUtilities.getNewHand(hand, Some(Move(List(BlackJoker)))) == Hand(hand.listOfCards.take(2)))
        }

        it("Should only filter out RedJoker joker when the hand has two in it and the move is a RedJoker") {
          val hand = Hand(List(TWO_Club, BlackJoker, RedJoker))
          assert(GameUtilities.getNewHand(hand, Some(Move(List(RedJoker)))) == Hand(hand.listOfCards.take(2)))
        }


        it("Should filter out both jokers from the hand if the move played is 2 jokers") {
          val hand = Hand(List(TWO_Club, BlackJoker, RedJoker))
          assert(GameUtilities.getNewHand(hand, Some(Move(List(RedJoker, BlackJoker)))) == Hand(hand.listOfCards.take(1)))
        }

      }

      describe("When the move played is a WildCard") {
        it("Should return the hand minus the played cards") {
          assert(GameUtilities.getNewHand(player.hand,
            Some(Move(List(THREE_Club(14), THREE_Heart(14), ACE_Diamond, ACE_Heart))))
          == Hand(currentHand.listOfCards.slice(2, 6) ++ currentHand.listOfCards.slice(8,10)))
        }
      }

    }
  }

  describe("tests for getCardAssumedByWildCard()") {
    it("Should return right value for a 3 diamond assuming a ten") {
      assert(GameUtilities.getCardAssumedByWildCard(THREE_Diamond(10)) == TEN_Diamond)
    }

    it("Should return right value for a 3 club assuming an eight") {
      assert(GameUtilities.getCardAssumedByWildCard(THREE_Club(8)) == EIGHT_Club)
    }

    it("Should return right value for a 3 heart assuming a jack") {
      assert(GameUtilities.getCardAssumedByWildCard(THREE_Heart(11)) == JACK_Heart)
    }

    it("Should return right value for a 3 spade assuming an ace") {
      assert(GameUtilities.getCardAssumedByWildCard(THREE_Spade(14)) == ACE_Spade)
    }
  }


  describe(" tests for getNumberOfWildCardsInMove()"){
    it("Should return 0") {
      val move = Move(List(ACE_Diamond, ACE_Club, ACE_Heart, ACE_Spade))
      assert(GameUtilities.getNumberOfWildCardsInMove(move) == 0)
    }

    it("Should return 1") {
      val move = Move(List(THREE_Diamond(14), ACE_Club, ACE_Heart, ACE_Spade))
      assert(GameUtilities.getNumberOfWildCardsInMove(move) == 1)
    }

    it("Should return 2") {
      val move = Move(List(THREE_Diamond(14), THREE_Club(14), ACE_Heart, ACE_Spade))
      assert(GameUtilities.getNumberOfWildCardsInMove(move) == 2)
    }

    it("Should return 3") {
      val move = Move(List(THREE_Diamond(14), THREE_Club(14), THREE_Heart(14), ACE_Spade))
      assert(GameUtilities.getNumberOfWildCardsInMove(move) == 3)
    }

    it("Should return 4") {
      val move = Move(List(THREE_Diamond(14), THREE_Club(14), THREE_Heart(14), THREE_Spade(14)))
      assert(GameUtilities.getNumberOfWildCardsInMove(move) == 4)
    }
  }

  describe("tests for getWildCardListFromIntermediateList") {
    val intermediateList = List(
      List(FOUR_Diamond, FOUR_Spade),
      List(NINE_Spade),
      List(JACK_Diamond, JACK_Heart, JACK_Spade),
      List(ACE_Diamond, ACE_Club, ACE_Heart, ACE_Spade)
    )
    describe("When no wildcards are present in intermediate list") {
      it("returns an empty list") {
        assert(GameUtilities.getWildCardListFromIntermediateList(intermediateList) == List.empty)
      }
    }

    describe("When wildcards are present in intermediate list") {
      it("Should return a list of size 1 when number of wildcards is 1") {
        val oneWildCardList = List(THREE_Diamond)
        assert(GameUtilities.getWildCardListFromIntermediateList(intermediateList :+ oneWildCardList) == oneWildCardList)
      }

      it("Should return a list of size 2 when number of wildcards is 2") {
        val oneWildCardList = List(THREE_Diamond, THREE_Club)
        assert(GameUtilities.getWildCardListFromIntermediateList(intermediateList :+ oneWildCardList) == oneWildCardList)
      }

      it("Should return a list of size 3 when number of wildcards is 3") {
        val oneWildCardList = List(THREE_Diamond, THREE_Club, THREE_Heart)
        assert(GameUtilities.getWildCardListFromIntermediateList(intermediateList :+ oneWildCardList) == oneWildCardList)
      }

      it("Should return a list of size 4 when number of wildcards is 4") {
        val oneWildCardList = List(THREE_Diamond,  THREE_Club, THREE_Heart, THREE_Spade)
        assert(GameUtilities.getWildCardListFromIntermediateList(intermediateList :+ oneWildCardList) == oneWildCardList)
      }
    }

  }

  describe("tests for addThreesToMoves()") {
    val allMoves = Moves(List(
      Move(List(SIX_Club, SIX_Diamond)),
      Move(List(SEVEN_Heart)),
      Move(List(KING_Heart, KING_Diamond, KING_Club)),
      Move(List(TWO_Spade)), Move(List(BlackJoker))))

    describe("When suppliedMoves has a wildcard in it") {
      it("Throws an exception") {
        val moves = Moves(List(
          Move(List(SIX_Club)),
          Move(List(KING_Club, KING_Diamond)),
          Move(List(THREE_Spade(9), NINE_Spade, NINE_Heart))
        ))
        assertThrows[IllegalMoveSuppliedException](GameUtilities.addThreesToMoves(moves, List(THREE_Spade)))
      }
    }

    describe("When listOfThrees is empty") {
      it("Should return allMoves without any changes") {
        assert(GameUtilities.addThreesToMoves(allMoves, List.empty) == allMoves)
      }
    }

    describe("When listOfThrees is nonEmpty") {

      describe("When listOfThrees is of size 1") {
        it("Should return result as expected") {
          val listOfThrees = List(THREE_Diamond)
          val expectedResult = Moves(allMoves.moves.slice(0, 3) ++ List(
            Move(List(THREE_Diamond(6), SIX_Club, SIX_Diamond)),
            Move(List(THREE_Diamond(7), SEVEN_Heart)),
            Move(List(THREE_Diamond(13), KING_Heart, KING_Diamond, KING_Club)),
            Move(List(THREE_Diamond(14))),
            Move(List(TWO_Spade)), Move(List(BlackJoker))))
          println(expectedResult)
          assert(GameUtilities.addThreesToMoves(allMoves, listOfThrees) == expectedResult)
        }
      }

      describe("When listOfThrees is of size 2") {
        it("Should return result as expected") {
          val listOfThrees = List(THREE_Heart, THREE_Spade)
          val expectedResult = Moves(allMoves.moves.slice(0, 3) ++ List(
          Move(List(THREE_Heart(6), SIX_Club, SIX_Diamond)),
          Move(List(THREE_Heart(7), SEVEN_Heart)),
          Move(List(THREE_Heart(13), KING_Heart, KING_Diamond, KING_Club)),

          Move(List(THREE_Spade(6), SIX_Club, SIX_Diamond)),
          Move(List(THREE_Spade(7), SEVEN_Heart)),
          Move(List(THREE_Spade(13), KING_Heart, KING_Diamond, KING_Club)),

          Move(List(THREE_Heart(6), THREE_Spade(6), SIX_Club, SIX_Diamond)),
          Move(List(THREE_Heart(7), THREE_Spade(7), SEVEN_Heart)),
          Move(List(THREE_Heart(13), THREE_Spade(13),  KING_Heart, KING_Diamond, KING_Club)),

          Move(List(THREE_Heart(14))),
          Move(List(THREE_Spade(14))),
          Move(List(THREE_Heart(14), THREE_Spade(14))),

          Move(List(TWO_Spade)), Move(List(BlackJoker))))
          assert(GameUtilities.addThreesToMoves(allMoves, listOfThrees) == expectedResult)
        }
      }

    }

  }

  describe("tests for assignWildCardsOptimally()") {

    val validMoves = Moves(List(
      Move(List(THREE_Spade(6), SIX_Diamond, SIX_Club)),
      Move(List(NINE_Club, NINE_Heart, NINE_Spade)),
      Move(List(THREE_Club(11), THREE_Heart(11), JACK_Diamond)),
    ))

    describe("When there are no validMoves comprised purely of WildCards") {
      it("Should return the same list of validMoves") {
        assert(GameUtilities.assignWildCardsOptimally(validMoves, Move(List.empty)) == validMoves)
      }
    }

    describe("When there are validMoves comprising purely of WildCards") {
      it("Should return the expected result") {
        val gameState = Move(List(FIVE_Diamond, FIVE_Club, FIVE_Heart))
        val allValidMoves = Moves(validMoves.moves ++ List(
          Move(List(THREE_Club(14), THREE_Heart(14), THREE_Spade(14)))
        ))
        val expectedResult = Moves(validMoves.moves ++ List(
          Move(List(THREE_Club(5), THREE_Heart(5), THREE_Spade(5)))
        ))
        assert(GameUtilities.assignWildCardsOptimally(allValidMoves, gameState) == expectedResult)
      }
    }

  }

  describe("tests for getMoveWithOptimalWildCardValue()") {

    describe("When move is not fully comprised of WildCards") {
      it("Should throw an exception when move supplied has a normalCard in it") {
        val validMove = Move(List(THREE_Club(14), THREE_Heart(14), THREE_Spade(14), ACE_Spade))
        assertThrows[IllegalMoveSuppliedException](GameUtilities.getMoveWithOptimalWildCardValue(validMove, Move(List.empty)))
      }

      it("Should throw an exception when move supplied has a specialCard in it") {
        val validMove = Move(List(TWO_Spade))
        val validMove2 = Move(List(BlackJoker))
        assertThrows[IllegalMoveSuppliedException](GameUtilities.getMoveWithOptimalWildCardValue(validMove, Move(List.empty)))
        assertThrows[IllegalMoveSuppliedException](GameUtilities.getMoveWithOptimalWildCardValue(validMove2, Move(List.empty)))
      }
    }

    describe("When move is comprised fully of wildcards") {
      describe("When gameState is EMPTY") {
        it("Should return the move itself") {
          val validMove = Move(List(THREE_Heart(14), THREE_Spade(14)))
          assert(GameUtilities.getMoveWithOptimalWildCardValue(validMove, Move(List.empty)) == validMove)
        }
      }

      describe("When gameState is NON EMPTY") {

        describe("When gameState is a single") {
          it("Should return move itself when it cannot burn") {
            val validMove = Move(List(THREE_Spade(14)))
            val gameState = Move(List(TEN_Spade))
            assert(GameUtilities.getMoveWithOptimalWildCardValue(validMove, gameState) == validMove)
          }

          it("Should return move with faceValue == gameState.moveFaceValue if it CAN burn") {
            val validMove = Move(List(THREE_Spade(14)))
            val gameState = Move(List(TEN_Heart))
            assert(GameUtilities.getMoveWithOptimalWildCardValue(validMove, gameState) == Move(List(THREE_Spade(10))))
          }
        }

        describe("When gameState is a double") {
          it("Should return move itself when it cannot burn") {
            val validMove = Move(List(THREE_Heart(14), THREE_Spade(14)))
            val gameState = Move(List(TEN_Heart, TEN_Spade))
            assert(GameUtilities.getMoveWithOptimalWildCardValue(validMove, gameState) == validMove)
          }

          it("Should return move with faceValue == gameState.moveFaceValue if it CAN burn") {
            val validMove = Move(List(THREE_Heart(14), THREE_Spade(14)))
            val gameState = Move(List(TEN_Diamond, TEN_Club))
            assert(GameUtilities.getMoveWithOptimalWildCardValue(validMove, gameState)
              == Move(List(THREE_Heart(10), THREE_Spade(10))))
          }
        }

        describe("When gameState is a triple") {
          it("Should return move itself when it cannot burn") {
            val validMove = Move(List(THREE_Diamond(14), THREE_Club(14), THREE_Heart(14)))
            val gameState = Move(List(TEN_Club, TEN_Heart, TEN_Spade))
            assert(GameUtilities.getMoveWithOptimalWildCardValue(validMove, gameState) == validMove)
          }

          it("Should return move with faceValue == gameState.moveFaceValue if it CAN burn") {
            val validMove = Move(List(THREE_Club(14), THREE_Heart(14), THREE_Spade(14)))
            val gameState = Move(List(TEN_Diamond, TEN_Club, TEN_Heart))
            assert(GameUtilities.getMoveWithOptimalWildCardValue(validMove, gameState)
              == Move(List(THREE_Club(10), THREE_Heart(10), THREE_Spade(10))))
          }
        }

      }

    }

  }

  describe("tests for getCardFromCardStrings") {

    describe("When card strings are Joker") {
      it("Should return BlackJoker") {
        assert(GameUtilities.getCardFromCardStrings("black_joker", "joker") == BlackJoker)
      }
      it("Should return RedJoker") {
        assert(GameUtilities.getCardFromCardStrings("red_joker", "joker") == RedJoker)
      }
    }

    describe("When card strings are for a TWO") {
      it("Should return TWO_Diamond") {
        assert(GameUtilities.getCardFromCardStrings("2", "diAmOnD") == TWO_Diamond)
      }
      it("Should return TWO_Club") {
        assert(GameUtilities.getCardFromCardStrings("2", "CLUb") == TWO_Club)
      }
      it("Should return TWO_Heart") {
        assert(GameUtilities.getCardFromCardStrings("2", "HEarT") == TWO_Heart)
      }
      it("Should return TWO_Spade") {
        assert(GameUtilities.getCardFromCardStrings("2", "spade") == TWO_Spade)
      }
    }

    describe("When card strings are for a THREE") {
      it("Should return THREE_Diamond assuming value 4") {
        assert(GameUtilities.getCardFromCardStrings("3(4)", "DiAMOND") == THREE_Diamond(4))
      }

      it("Should return THREE_Club assuming value 9") {
        assert(GameUtilities.getCardFromCardStrings("3(9)", "ClUB") == THREE_Club(9))
      }

      it("Should return THREE_HEART assuming value Queen") {
        assert(GameUtilities.getCardFromCardStrings("3(12)", "HEART") == THREE_Heart(12))
      }

      it("Should return THREE_Spade assuming value ACE") {
        assert(GameUtilities.getCardFromCardStrings("3(14)", "Spade") == THREE_Spade(14))
      }
    }

    describe("When card strings are for a normalcard") {
      it("Should return SEVEN_Spade") {
        assert(GameUtilities.getCardFromCardStrings("7", "Spade") == SEVEN_Spade)
      }

      it("Should return TEN_Diamond") {
        assert(GameUtilities.getCardFromCardStrings("10", "diAMOND") == TEN_Diamond)
      }

      it("Should return KING_Heart") {
        assert(GameUtilities.getCardFromCardStrings("K", "HeART") == KING_Heart)
      }
    }

    describe("When bad faceValue is supplied") {
      it("Should throw exception"){
        assertThrows[IllegalMoveSuppliedException](GameUtilities.getCardFromCardStrings("JOKA", "ko"))
        assertThrows[IllegalMoveSuppliedException](GameUtilities.getCardFromCardStrings("11", "Diamond"))
        assertThrows[IllegalMoveSuppliedException](GameUtilities.getCardFromCardStrings("3", "Spade"))
        assertThrows[IllegalMoveSuppliedException](GameUtilities.getCardFromCardStrings("3()", "Spade"))
        assertThrows[IllegalMoveSuppliedException](GameUtilities.getCardFromCardStrings("0", "Club"))
      }
    }

    describe("When bad suit is supplied") {
      it("Should throw exception") {
        assertThrows[IllegalMoveSuppliedException](GameUtilities.getCardFromCardStrings("3(6)", "Banana"))
        assertThrows[IllegalMoveSuppliedException](GameUtilities.getCardFromCardStrings("7", "Potato"))
        assertThrows[IllegalMoveSuppliedException](GameUtilities.getCardFromCardStrings("2", "Shades"))
      }
    }

    describe("When bad assumedValue for 3 is supplied") {
      it("Should throw exception") {
        assertThrows[IllegalAssumedValueException](GameUtilities.getCardFromCardStrings("3(3)", "diamond"))
        assertThrows[IllegalAssumedValueException](GameUtilities.getCardFromCardStrings("3(15)", "club"))
        assertThrows[IllegalAssumedValueException](GameUtilities.getCardFromCardStrings("3(0)", "heart"))
        assertThrows[IllegalMoveSuppliedException](GameUtilities.getCardFromCardStrings("3(-1)", "spade"))
      }
    }

  }

  describe("Tests for isLegalMove") {

    // Single, double, triple, quad + w/wout 3s + single/double/triple 2 + joker
    describe("It should return true when") {

      it("The move is a single") {
        assert(GameUtilities.isLegalMove(Move(List(FOUR_Spade))))
      }

      it("The move is a double") {
        assert(GameUtilities.isLegalMove(Move(List(FOUR_Diamond, FOUR_Spade))))
      }

      it("The move is a quad involving a WildCard") {
        assert(GameUtilities.isLegalMove(Move(List(THREE_Spade(4), FOUR_Diamond, FOUR_Heart, FOUR_Spade))))
      }

      it("The move is comprised of a single SpecialCard") {
        assert(GameUtilities.isLegalMove(Move(List(TWO_Diamond))))
      }

      it("The move is comprised of a multiple SpecialCards") {
        assert(GameUtilities.isLegalMove(Move(List(TWO_Diamond, TWO_Club, TWO_Heart))))
      }

      it("The move is comprised of a single BlackJoker") {
        assert(GameUtilities.isLegalMove(Move(List(BlackJoker))))
      }

      it("The move is comprised of a single RedJoker") {
        assert(GameUtilities.isLegalMove(Move(List(RedJoker))))
      }

    }

    describe("It should return false when") {

      it("Is a move involving multiple Jokers") {
        assert(!GameUtilities.isLegalMove(Move(List(RedJoker, BlackJoker))))
      }

      it("Is a move involving NormalCards and Joker"){
        assert(!GameUtilities.isLegalMove(Move(List(BlackJoker, FOUR_Spade))))
      }

      it("Is a move involving SpecialCards and Joker"){
        assert(!GameUtilities.isLegalMove(Move(List(BlackJoker, TWO_Club))))
      }

      it("Is a move involving WildCards and Joker"){
        assert(!GameUtilities.isLegalMove(Move(List(BlackJoker, THREE_Diamond(12)))))
      }

      it("Is a move involving SpecialCards and WildCards"){
        assert(!GameUtilities.isLegalMove(Move(List(TWO_Heart, THREE_Spade(5)))))
      }

      it("Is a move involving SpecialCards and NormalCards"){
        assert(!GameUtilities.isLegalMove(Move(List(TWO_Heart, SIX_Diamond))))
      }

      it("Is a move involving SpecialCards and NormalCards and WildCards"){
        assert(!GameUtilities.isLegalMove(Move(List(TWO_Heart, THREE_Spade(5), FIVE_Heart))))
      }

      it("Is a move involving multiple NormalCards of different values") {
        assert(!GameUtilities.isLegalMove(Move(List(SIX_Diamond, SEVEN_Spade, EIGHT_Club))))
      }

      it("Is a move involving multiple NormalCards of different values and a WildCard") {
        assert(!GameUtilities.isLegalMove(Move(List(THREE_Spade(6), SIX_Diamond, SIX_Club, EIGHT_Club))))
      }
    }
  }

  describe("Tests for fixWildCardAssumedValueInMove") {

    describe("If the move has no wildCards in it"){
      it("Should return the move itself") {
        val move = Move(List(SEVEN_Heart, SEVEN_Spade))
        assert(GameUtilities.fixWildCardAssumedValueInMove(move, Move(List.empty)) == move)
      }
    }

    describe("If the move has a NormalCard in it") {
      it("Should assign assumedValue for all WildCards to the NormalCard") {
        val rawMove = Move(List(THREE_Diamond, THREE_Club, THREE_Heart, THREE_Spade, KING_Spade))
        val expectedMove = Move(List(THREE_Diamond(12), THREE_Club(12), THREE_Heart(12), THREE_Spade(12), KING_Spade))
        assert(GameUtilities.fixWildCardAssumedValueInMove(rawMove, Move(List.empty)) == expectedMove)
      }
    }

    describe("If the move is comprised entirely of WildCards") {

      it("Should assign them the value for double ACES if it cannot burn the gameState") {
        val rawMove = Move(List(THREE_Heart, THREE_Spade))
        val expectedMove = Move(List(THREE_Heart(14), THREE_Spade(14)))
        val gameState = Move(List(SIX_Diamond, SIX_Spade))
        assert(GameUtilities.fixWildCardAssumedValueInMove(rawMove, gameState) == expectedMove)
      }

      it("Should reassign the value to TRIPLE 7s if it can burn the gameState") {
        val rawMove = Move(List(THREE_Diamond, THREE_Heart, THREE_Spade))
        val expectedMove = Move(List(THREE_Diamond(7), THREE_Heart(7), THREE_Spade(7)))
        val gameState = Move(List(THREE_Club(7), SEVEN_Diamond, SEVEN_Club))
        assert(GameUtilities.fixWildCardAssumedValueInMove(rawMove, gameState) == expectedMove)
      }
    }

  }

  describe("Tests for dropAndReplaceCardsInHand") {

    describe("When hand is empty") {
      val hand = Hand(List.empty)
      describe("When cards to drop is empty") {
        val cardsToDrop = List.empty
        val cardsToReplace = List(TWO_Diamond, BlackJoker)
        val expectedHand = Hand(cardsToReplace)
        assert(GameUtilities.dropAndReplaceCardsInHand(hand, cardsToDrop, cardsToReplace) == expectedHand)
      }

      describe("When cards to replace is empty") {
        val cardsToDrop = List(SIX_Spade, SEVEN_Club)
        val cardsToReplace = List.empty
        val expectedHand = Hand(cardsToReplace)
        assert(GameUtilities.dropAndReplaceCardsInHand(hand, cardsToDrop, cardsToReplace) == expectedHand)
      }

      describe("When both cards to drop and cards to replace is non-empty") {
        it("Should only add cards and not drop anything") {
          val cardsToDrop = List(SIX_Spade, SEVEN_Club)
          val cardsToReplace = List(TWO_Diamond, BlackJoker)
          val expectedHand = Hand(cardsToReplace)
          assert(GameUtilities.dropAndReplaceCardsInHand(hand, cardsToDrop, cardsToReplace) == expectedHand)
        }

        it("Should work fine when 2 jokers are to be added") {
          val cardsToDrop = List(SIX_Spade, SEVEN_Club)
          val cardsToReplace = List(RedJoker, BlackJoker)
          val expectedHand = Hand(cardsToReplace)
          assert(GameUtilities.dropAndReplaceCardsInHand(hand, cardsToDrop, cardsToReplace) == expectedHand)
        }
      }
    }

    describe("When hand is non-empty") {
      val hand = Hand(List(THREE_Spade, SIX_Spade,  EIGHT_Club, TEN_Diamond, KING_Spade, KING_Heart, ACE_Spade, TWO_Club, BlackJoker))

      describe("When cards to drop is empty") {
        it("Should drop no cards and replace as needed") {
          val cardsToDrop = List.empty
          val cardsToReplace = List(BlackJoker, THREE_Club, BlackJoker, EIGHT_Diamond)
          val expectedHand = Hand(GameUtilities.sortCards(hand.listOfCards ++ cardsToReplace))
          assert(GameUtilities.dropAndReplaceCardsInHand(hand, cardsToDrop, cardsToReplace) == expectedHand)
        }
      }

      describe("When cards to replace with is empty") {
        it("Should only drop cards and not replace them with anything") {
          val cardsToReplace = List.empty
          val cardsToDrop = List(SIX_Club, SIX_Spade, THREE_Spade, TWO_Heart, TWO_Club, BlackJoker)
          val expectedHand = Hand(List(EIGHT_Club, TEN_Diamond, KING_Heart, KING_Spade, ACE_Spade))
          assert(GameUtilities.dropAndReplaceCardsInHand(hand, cardsToDrop, cardsToReplace) == expectedHand)
        }
      }

      describe("When there are cards to drop and replace") {
        it("Should only drop those cards that appear in the hand and add cardToReplace to hand even if there are duplicates") {
          val cardsToDrop = List(THREE_Spade, SIX_Spade, TWO_Club, BlackJoker, NINE_Spade)
          val cardsToReplace = List(SEVEN_Club, SEVEN_Club, SEVEN_Heart, SEVEN_Spade)
          val expectedHand = Hand(List(SEVEN_Club, SEVEN_Club, SEVEN_Heart, SEVEN_Spade, EIGHT_Club, TEN_Diamond, KING_Heart, KING_Spade, ACE_Spade))
          assert(GameUtilities.dropAndReplaceCardsInHand(hand, cardsToDrop, cardsToReplace) == expectedHand)
        }

        it("Should drop both jokers accurately and replace") {
          val newHand = Hand(List(THREE_Spade, SIX_Spade,  EIGHT_Club, TEN_Diamond, KING_Spade, KING_Heart, ACE_Spade, TWO_Club, RedJoker, BlackJoker))
          val cardsToDrop = List(THREE_Spade, SIX_Spade, TWO_Club, RedJoker, NINE_Spade, BlackJoker)
          val cardsToReplace = List(SEVEN_Club, SEVEN_Club, SEVEN_Heart, SEVEN_Spade)
          val expectedHand = Hand(List(SEVEN_Club, SEVEN_Club, SEVEN_Heart, SEVEN_Spade, EIGHT_Club, TEN_Diamond, KING_Heart, KING_Spade, ACE_Spade))
          assert(GameUtilities.dropAndReplaceCardsInHand(newHand, cardsToDrop, cardsToReplace) == expectedHand)
        }

        it("Should drop only one joker accurately and replace") {
          val newHand = Hand(List(THREE_Spade, SIX_Spade,  EIGHT_Club, TEN_Diamond, KING_Spade, KING_Heart, ACE_Spade, TWO_Club, RedJoker, BlackJoker))
          val cardsToDrop = List(THREE_Spade, SIX_Spade, TWO_Club, BlackJoker, NINE_Spade)
          val cardsToReplace = List(SEVEN_Club, SEVEN_Club, SEVEN_Heart, SEVEN_Spade)
          val expectedHand = Hand(List(SEVEN_Club, SEVEN_Club, SEVEN_Heart, SEVEN_Spade, EIGHT_Club, TEN_Diamond, KING_Heart, KING_Spade, ACE_Spade, RedJoker))
          assert(GameUtilities.dropAndReplaceCardsInHand(newHand, cardsToDrop, cardsToReplace) == expectedHand)
        }

        it("Should drop no jokers and replace as required") {
          val newHand = Hand(List(THREE_Spade, SIX_Spade,  EIGHT_Club, TEN_Diamond, KING_Spade, KING_Heart, ACE_Spade, TWO_Club, RedJoker, BlackJoker))
          val cardsToDrop = List(THREE_Spade, SIX_Spade, TWO_Club, NINE_Spade)
          val cardsToReplace = List(SEVEN_Club, SEVEN_Club, SEVEN_Heart, SEVEN_Spade)
          val expectedHand = Hand(List(SEVEN_Club, SEVEN_Club, SEVEN_Heart, SEVEN_Spade, EIGHT_Club, TEN_Diamond, KING_Heart, KING_Spade, ACE_Spade, RedJoker, BlackJoker))
          assert(GameUtilities.dropAndReplaceCardsInHand(newHand, cardsToDrop, cardsToReplace) == expectedHand)
        }
      }
    }
  }

  describe("Tests for sortCardsToGiveAway") {

    it("Should throw an exception when supplied with a Normal Card") {
      val listOfCards = List(THREE_Spade, KING_Spade, TWO_Diamond, BlackJoker)
      assertThrows[IllegalCardSuppliedException](GameUtilities.sortCardsInPreferenceOrderOfGivingAwayBestCards(listOfCards))
    }

    it("Should return empty list when supplied list is empty") {
      assert(GameUtilities.sortCardsInPreferenceOrderOfGivingAwayBestCards(List.empty) == List.empty)
    }

    describe("When only non-normal cards are supplied") {
      it("Should return the same list of size 1") {
        val listOfCards = List(TWO_Diamond)
        assert(GameUtilities.sortCardsInPreferenceOrderOfGivingAwayBestCards(listOfCards) == listOfCards)
      }

      it("Should sort the list of size 2 appropriately") {
        val listOfCards = List(TWO_Diamond, THREE_Spade, BlackJoker)
        assert(GameUtilities.sortCardsInPreferenceOrderOfGivingAwayBestCards(listOfCards) == List(BlackJoker, THREE_Spade, TWO_Diamond))
      }

      it("Should sort the list of size 10 appropriately") {
        val expectedListOfCards = List(TWO_Diamond, TWO_Club, THREE_Diamond, THREE_Club, THREE_Heart,
                                      THREE_Spade, TWO_Heart, TWO_Spade, RedJoker, BlackJoker).reverse
        assert(GameUtilities.sortCardsInPreferenceOrderOfGivingAwayBestCards(Random.shuffle(expectedListOfCards)) == expectedListOfCards)
      }
    }
  }

  describe("Tests for exchangeHands") {

    describe("When there is 3 players") {
      val h1 = Hand(List(THREE_Spade, SEVEN_Spade, JACK_Club, TWO_Heart, BlackJoker))
      val h2 = Hand(List(THREE_Heart, SEVEN_Club, JACK_Spade, TWO_Spade, BlackJoker))
      val h3 = Hand(List(THREE_Club, SEVEN_Heart, JACK_Heart, TWO_Club, RedJoker))
      val p1 = Player("p1", h1)
      val p2 = Player("p2", h2)
      val p3 = Player("p3", h3)
      val players = List(p1, p2, p3).toBuffer

      describe("When everyone is neutral") {
        it("Should involve no exchanges") {
          val playerCompletionOrder = List("p1", "p2", "p3")
          val userSelectedCardToGetRidOf = List.empty
          val playerCompletionStatuses = List(Neutral, Neutral, Neutral)
          val (newPlayers, receivedCards)  = GameUtilities.exchangeHands(players, playerCompletionOrder, playerCompletionStatuses, userSelectedCardToGetRidOf)
          assert(newPlayers == players)
        }
      }

      describe("When positions are filled ") {
        val playerCompletionStatuses = List(President, Neutral, Bum)

        describe("When there is a real player"){
          val newPlayers = players.map(p => if(p.name == "p1") p.copy(isRealPlayer = true) else p)

          describe("When real player is President") {
            it("Should drop cards based on supplied parameter and not default to auto-drop") {
              val playerCompletionOrder = List("p1", "p2", "p3")
              val userSelectedCardToGetRidOf = List(JACK_Club)
              val (resultingPlayers, receivedCards) = GameUtilities.exchangeHands(newPlayers, playerCompletionOrder, playerCompletionStatuses, userSelectedCardToGetRidOf)
              assert(resultingPlayers.head.hand == Hand(List(THREE_Spade, SEVEN_Spade, TWO_Heart, RedJoker, BlackJoker)))
              assert(resultingPlayers(1).hand == h2)
              assert(resultingPlayers.last.hand == Hand(List(THREE_Club, SEVEN_Heart, JACK_Club, JACK_Heart, TWO_Club)))
              assert(receivedCards == List(RedJoker))
            }
          }

          describe("When real player is Neutral") {
            it("Should exchange cards for others but not the real player") {
              val playerCompletionOrder = List("p2", "p1", "p3")
              val userSelectedCardToGetRidOf = List(JACK_Club)
              val (resultingPlayers, receivedCards) = GameUtilities.exchangeHands(newPlayers, playerCompletionOrder, playerCompletionStatuses, userSelectedCardToGetRidOf)
              assert(resultingPlayers.head.hand == h1)
              assert(resultingPlayers(1).hand == Hand(List(THREE_Heart, JACK_Spade, TWO_Spade, RedJoker, BlackJoker)))
              assert(resultingPlayers.last.hand == Hand(List(THREE_Club, SEVEN_Club, SEVEN_Heart, JACK_Heart, TWO_Club)))
              assert(receivedCards == List.empty)
            }
          }

          describe("When real player is Bum") {
            it("Should auto-exchange cards even for the real player") {
              val playerCompletionOrder = List("p3", "p2", "p1")
              val userSelectedCardToGetRidOf = List(JACK_Club)
              val (resultingPlayers, receivedCards) = GameUtilities.exchangeHands(newPlayers, playerCompletionOrder, playerCompletionStatuses, userSelectedCardToGetRidOf)
              assert(resultingPlayers.head.hand == Hand(List(THREE_Spade, SEVEN_Heart, SEVEN_Spade, JACK_Club, TWO_Heart)))
              assert(resultingPlayers(1).hand == h2)
              assert(resultingPlayers.last.hand == Hand(List(THREE_Club, JACK_Heart, TWO_Club, RedJoker, BlackJoker)))
              assert(receivedCards == List(SEVEN_Heart))
            }
          }
        }

        describe("When all players are AI") {
          it("Should auto-exchange cards for everyone") {
            val playerCompletionOrder = List("p1", "p2", "p3")
            val userSelectedCardToGetRidOf = List(JACK_Club)
            val (resultingPlayers, receivedCards) = GameUtilities.exchangeHands(players, playerCompletionOrder, playerCompletionStatuses, userSelectedCardToGetRidOf)
            assert(resultingPlayers.head.hand == Hand(List(THREE_Spade, JACK_Club, TWO_Heart, RedJoker, BlackJoker)))
            assert(resultingPlayers(1).hand == h2)
            assert(resultingPlayers.last.hand == Hand(List(THREE_Club, SEVEN_Heart, SEVEN_Spade, JACK_Heart, TWO_Club)))
            assert(receivedCards == List.empty)
          }
        }
      }

    }

    describe("When there is 6 players") {
      val h1 = Hand(List(THREE_Spade, SEVEN_Spade, JACK_Club, TWO_Heart, BlackJoker))
      val h2 = Hand(List(THREE_Heart, SEVEN_Club, JACK_Spade, TWO_Spade, RedJoker))
      val h3 = Hand(List(THREE_Club, SEVEN_Heart, JACK_Heart, TWO_Club, BlackJoker))
      val h4 = Hand(List(THREE_Diamond, SEVEN_Diamond, JACK_Diamond, TWO_Diamond, RedJoker))
      val h5 = Hand(List(FIVE_Club, SIX_Diamond, EIGHT_Heart, ACE_Club, BlackJoker))
      val h6 = Hand(List(FIVE_Diamond, SIX_Spade, EIGHT_Diamond, ACE_Diamond, RedJoker))

      val p1 = Player("p1", h1)
      val p2 = Player("p2", h2)
      val p3 = Player("p3", h3)
      val p4 = Player("p4", h4)
      val p5 = Player("p5", h5)
      val p6 = Player("p6", h6)
      val players = List(p1, p2, p3, p4, p5, p6).toBuffer
      val playerCompletionOrder = List("p1", "p2", "p3", "p4", "p5", "p6")

      describe("When everyone is neutral") {
        it("Should involve no exchanges") {
          val userSelectedCardToGetRidOf = List.empty
          val playerCompletionStatuses = List(Neutral, Neutral, Neutral, Neutral, Neutral, Neutral)
          val (newPlayers, receivedCards) = GameUtilities.exchangeHands(players, playerCompletionOrder, playerCompletionStatuses, userSelectedCardToGetRidOf)
          assert(newPlayers == players)
          assert(receivedCards == List.empty)
        }
      }

      describe("When all positions are filled") {
        val playerCompletionStatuses = List(President, VicePres, Neutral, Neutral, ViceBum, Bum)

        describe("When there is a real player"){
          val newPlayers = players.map(p => if(p.name == "p1") p.copy(isRealPlayer = true) else p)

          describe("When real player is President") {
            it("Should drop cards based on supplied parameter and not default to auto-drop") {
              val playerCompletionOrder = List("p1", "p2", "p3", "p4", "p5", "p6")
              val userSelectedCardToGetRidOf = List(JACK_Club, TWO_Heart)
              val (resultingPlayers, receivedCards) = GameUtilities.exchangeHands(newPlayers, playerCompletionOrder, playerCompletionStatuses, userSelectedCardToGetRidOf)
              assert(resultingPlayers.head.hand == Hand(List(THREE_Spade, SEVEN_Spade, ACE_Diamond, RedJoker, BlackJoker)))
              assert(resultingPlayers(1).hand ==  Hand(List(THREE_Heart, JACK_Spade, TWO_Spade, RedJoker, BlackJoker)))
              assert(resultingPlayers(2).hand ==  h3)
              assert(resultingPlayers(3).hand ==  h4)
              assert(resultingPlayers(4).hand ==  Hand(List(FIVE_Club, SIX_Diamond, SEVEN_Club, EIGHT_Heart, ACE_Club)))
              assert(resultingPlayers.last.hand == Hand(List(FIVE_Diamond, SIX_Spade, EIGHT_Diamond, JACK_Club, TWO_Heart)))
              assert(receivedCards == List(ACE_Diamond, RedJoker))
            }
          }

          describe("When real player is VicePresident") {
            it("Should drop cards based on supplied parameter and not default to auto-drop") {
              val playerCompletionOrder = List("p2", "p1", "p3", "p4", "p5", "p6")
              val userSelectedCardToGetRidOf = List(JACK_Club)
              val (resultingPlayers, receivedCards) = GameUtilities.exchangeHands(newPlayers, playerCompletionOrder, playerCompletionStatuses, userSelectedCardToGetRidOf)
              assert(resultingPlayers.head.hand == Hand(List(THREE_Spade, SEVEN_Spade, TWO_Heart, BlackJoker, BlackJoker)))
              assert(resultingPlayers(1).hand ==  Hand(List(THREE_Heart, ACE_Diamond, TWO_Spade, RedJoker, RedJoker)))
              assert(resultingPlayers(2).hand ==  h3)
              assert(resultingPlayers(3).hand ==  h4)
              assert(resultingPlayers(4).hand ==  Hand(List(FIVE_Club, SIX_Diamond, EIGHT_Heart, JACK_Club, ACE_Club)))
              assert(resultingPlayers.last.hand == Hand(List(FIVE_Diamond, SIX_Spade, SEVEN_Club, EIGHT_Diamond, JACK_Spade)))
              assert(receivedCards == List(BlackJoker))
            }
          }

          describe("When real player is Neutral") {
            it("Should auto-exchange cards for everyone") {
              val playerCompletionOrder = List("p2", "p3", "p1", "p4", "p5", "p6")
              val userSelectedCardToGetRidOf = List(JACK_Club)
              val (resultingPlayers, receivedCards) = GameUtilities.exchangeHands(newPlayers, playerCompletionOrder, playerCompletionStatuses, userSelectedCardToGetRidOf)
              assert(resultingPlayers.head.hand == h1)
              assert(resultingPlayers(1).hand == Hand(List(THREE_Heart, ACE_Diamond, TWO_Spade, RedJoker, RedJoker)))
              assert(resultingPlayers(2).hand == Hand(List(THREE_Club, JACK_Heart, TWO_Club, BlackJoker, BlackJoker)))
              assert(resultingPlayers(3).hand == h4)
              assert(resultingPlayers(4).hand == Hand(List(FIVE_Club, SIX_Diamond, SEVEN_Heart, EIGHT_Heart, ACE_Club)))
              assert(resultingPlayers.last.hand == Hand(List(FIVE_Diamond, SIX_Spade, SEVEN_Club, EIGHT_Diamond, JACK_Spade)))
              assert(receivedCards == List.empty)
            }
          }

          describe("When real player is ViceBum") {
            it("Should auto-exchange cards for everyone") {
              val playerCompletionOrder = List("p2", "p3", "p4", "p5", "p1", "p6")
              val userSelectedCardToGetRidOf = List(JACK_Club)
              val (resultingPlayers, receivedCards) = GameUtilities.exchangeHands(newPlayers, playerCompletionOrder, playerCompletionStatuses, userSelectedCardToGetRidOf)
              assert(resultingPlayers.head.hand == Hand(List(THREE_Spade, SEVEN_Heart, SEVEN_Spade, JACK_Club, TWO_Heart)))
              assert(resultingPlayers(1).hand == Hand(List(THREE_Heart, ACE_Diamond, TWO_Spade, RedJoker, RedJoker)))
              assert(resultingPlayers(2).hand == Hand(List(THREE_Club, JACK_Heart, TWO_Club, BlackJoker, BlackJoker)))
              assert(resultingPlayers(3).hand == h4)
              assert(resultingPlayers(4).hand == h5)
              assert(resultingPlayers.last.hand == Hand(List(FIVE_Diamond, SIX_Spade, SEVEN_Club, EIGHT_Diamond, JACK_Spade)))
              assert(receivedCards == List(SEVEN_Heart))
            }
          }

          describe("When real player is Bum") {
            it("Should auto-exchange cards for everyone") {
              val playerCompletionOrder = List("p2", "p3", "p4", "p5", "p6", "p1")
              val userSelectedCardToGetRidOf = List(JACK_Club)
              val (resultingPlayers, receivedCards) = GameUtilities.exchangeHands(newPlayers, playerCompletionOrder, playerCompletionStatuses, userSelectedCardToGetRidOf)
              assert(resultingPlayers.head.hand == Hand(List(THREE_Spade, SEVEN_Club, SEVEN_Spade, JACK_Club, JACK_Spade)))
              assert(resultingPlayers(1).hand == Hand(List(THREE_Heart, TWO_Heart, TWO_Spade, RedJoker, BlackJoker)))
              assert(resultingPlayers(2).hand == Hand(List(THREE_Club, JACK_Heart, TWO_Club, RedJoker, BlackJoker)))
              assert(resultingPlayers(3).hand == h4)
              assert(resultingPlayers(4).hand == h5)
              assert(resultingPlayers.last.hand == Hand(List(FIVE_Diamond, SIX_Spade, SEVEN_Heart, EIGHT_Diamond, ACE_Diamond)))
              assert(receivedCards == List(SEVEN_Club, JACK_Spade))
            }
          }
        }

        describe("When all players are AI") {
          it("Should auto-exchange cards for everyone") {
            val playerCompletionOrder = List("p1", "p2", "p3", "p4", "p5", "p6")
            val userSelectedCardToGetRidOf = List(JACK_Club)
            val (resultingPlayers, receivedCards) = GameUtilities.exchangeHands(players, playerCompletionOrder, playerCompletionStatuses, userSelectedCardToGetRidOf)
            assert(resultingPlayers.head.hand == Hand(List(THREE_Spade, ACE_Diamond, TWO_Heart, RedJoker, BlackJoker)))
            assert(resultingPlayers(1).hand == Hand(List(THREE_Heart, JACK_Spade, TWO_Spade, RedJoker, BlackJoker)))
            assert(resultingPlayers(2).hand == h3)
            assert(resultingPlayers(3).hand == h4)
            assert(resultingPlayers(4).hand == Hand(List(FIVE_Club, SIX_Diamond, SEVEN_Club, EIGHT_Heart, ACE_Club)))
            assert(resultingPlayers.last.hand == Hand(List(FIVE_Diamond, SIX_Spade, SEVEN_Spade, EIGHT_Diamond, JACK_Club)))
            assert(receivedCards == List.empty)
          }
        }
      }
    }

  }

  describe("tests for getNormalAndNonNormalListsOfCardsFromHand") {
    describe("When hand is empty") {
      it("Should return two empty lists of cards") {
        val hand = Hand(List.empty)
        val (l1, l2) = GameUtilities.getNormalAndNonNormalListsOfCardsFromHand(hand)
        assert(l1.isEmpty)
        assert(l2.isEmpty)
      }
    }

    describe("When hand is non empty") {
      describe("When hand has only normal cards") {
        it("Should return an empty list of non normal cards") {
          val hand = Hand(List(SIX_Heart, NINE_Club, JACK_Spade, KING_Diamond, ACE_Spade))
          val (l1, l2) = GameUtilities.getNormalAndNonNormalListsOfCardsFromHand(hand)
          assert(l1 == hand.listOfCards)
          assert(l2.isEmpty)
        }
      }

      describe("When hand has only non-normal cards") {
        it("Should return an empty list of normal cards") {
          val hand = Hand(List(THREE_Club, THREE_Spade, TWO_Diamond, TWO_Club, RedJoker, BlackJoker))
          val (l1, l2) = GameUtilities.getNormalAndNonNormalListsOfCardsFromHand(hand)
          assert(l1.isEmpty)
          assert(l2 == List(TWO_Diamond, TWO_Club, THREE_Club, THREE_Spade, RedJoker, BlackJoker))
        }
      }

      describe("When hand has both normal and non-normal cards") {
        it("Should return a non-empty list of normal and non-normal cards") {
          val hand = Hand(List(SIX_Heart, NINE_Club, JACK_Spade, KING_Diamond, ACE_Spade, THREE_Club, THREE_Spade, TWO_Diamond, TWO_Club, RedJoker, BlackJoker))
          val (l1, l2) = GameUtilities.getNormalAndNonNormalListsOfCardsFromHand(hand)
          assert(l1 == List(SIX_Heart, NINE_Club, JACK_Spade, KING_Diamond, ACE_Spade))
          assert(l2 == List(TWO_Diamond, TWO_Club, THREE_Club, THREE_Spade, RedJoker, BlackJoker))
        }
      }
    }
  }

}
