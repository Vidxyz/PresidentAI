import game.{Card, GameUtilities, Hand, IllegalAssumedValueException, Joker, Move, Moves, NormalCard, SpecialCard, WildCard}
import org.scalatest.FunSpec
import utils.Consants

import scala.util.Random
import game.FaceValue._
import game.GameUtilities.IllegalMoveSuppliedException
import game.Suits._
import player.Player
import utils.Consants._

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
        assert(result.head.hand.size == Consants.totalNumberOfCards)
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
        assert(result.map(p => p.hand).flatMap(h => h.listOfCards).size == Consants.totalNumberOfCards)
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
          List(Joker, Joker),
        )
        assert(GameUtilities.getListsOfSimilarCards(Consants.sortedHandWithAllCards) == expectedResult)
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
      it("Should return true for a game.Joker") {
        assert(GameUtilities.isValidMove(Move(List(Joker)), Move(List.empty)))
      }
    }

    describe("When gameState is game.Joker") {
      it("Should return false") {
        assert(!GameUtilities.isValidMove(Move(List(NormalCard(SEVEN, Spade))), Move(List(Joker))))
      }
    }

    describe("When move is game.Joker") {
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
          assert(GameUtilities.checkIfBetter(Move(List(Joker)), Move(List(SpecialCard(TWO, Spade)))))
        }
      }

      describe("When move1 is game.Joker and move2 is ALSO game.Joker") {
        it("Should return false") {
          assert(!GameUtilities.checkIfBetter(Move(List(Joker)), Move(List(Joker))))
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

      describe("When move1 is game.Joker and move2 is tripleAces") {
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

      describe("When move1 is game.Joker and move2 is tripleAces") {
        it("should return true"){
          assert(GameUtilities.checkIfBetter(
            Move(List(Joker)),
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
    it("should return the right value when supplied card is a Joker") {
      assert(GameUtilities.cardOrderValue(Joker) == 52)
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
          Move(List(Joker)),
          Move(List(Joker)),
          Move(List(Joker, Joker)),
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
          Move(List(Joker)),
          Move(List(Joker)),
          Move(List(Joker, Joker)),
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

      it("Should be a burn when a single game.Joker is played") {
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

      it("Should be a burn when a single game.Joker is played") {
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
      List(Joker)
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
      TWO_Diamond, Joker))
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
          assert(GameUtilities.getNewHand(player.hand, Some(Move(List(Joker))))
            == Hand(currentHand.listOfCards.slice(0,9)))
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
      Move(List(TWO_Spade)), Move(List(Joker))))

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
            Move(List(TWO_Spade)), Move(List(Joker))))
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

          Move(List(TWO_Spade)), Move(List(Joker))))
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
        val validMove2 = Move(List(Joker))
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
      it("Should return Joker") {
        assert(GameUtilities.getCardFromCardStrings("joker", "joker") == Joker)
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

}
