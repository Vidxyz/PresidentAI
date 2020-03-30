import game.FaceValue._
import game.GameEngine.IllegalHeuristicFunctionException
import game.{Card, GameEngine, GameUtilities, Hand, Joker, Move, Moves, NormalCard, SpecialCard, WildCard}
import game.Suits._
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.FunSpec
import player.PlayerIndicators
import utils.Consants
import utils.Consants._

class GameEngineTest extends FunSpec {

  val epsilon = 1e-4f
  implicit val doubleEq: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(epsilon)

  describe("tests for getNextMove()"){

    describe("When validMoves is empty"){
      it("Should return None") {
        val playerIndicators: PlayerIndicators = PlayerIndicators(Hand(List.empty))
        assert(GameEngine.getNextMove(Moves(List.empty), Move(List.empty))
        (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).isEmpty)
      }
    }

    describe("When validMoves.size is 1") {
      describe("and it is a normalCard") {
        it("Should return the validMove") {
          val gameState = Move(List(JACK_Heart))
          val validMoves = Moves(List(Move(List(QUEEN_Diamond))))
          assert(GameEngine.getNextMove(validMoves, gameState)(GameEngine.applyNormalCardMoveHeuristic, PlayerIndicators(Hand(List(QUEEN_Diamond))))
            .contains(Move(List(QUEEN_Diamond))))
        }
      }

      describe("And it is a Wildcard") {
        it("Should return the validMove") {
          val gameState = Move(List(JACK_Heart))
          val validMoves = Moves(List(Move(List(THREE_Club(14)))))
          assert(GameEngine.getNextMove(validMoves, gameState)(GameEngine.applyNormalCardMoveHeuristic, PlayerIndicators(Hand(List(THREE_Club))))
            .contains(Move(List(THREE_Club(14)))))
        }
      }
    }

    // Test for the following
    // Test for normalMoveHeuristic and specialMoveHeuristic
    // Should return None when no valid moves
    // Should return the one Move in validMoves if validMoves.size = 1 and move is NormalCard

    // When gameState is Empty :-
    // Lowest possible valid move amongst all singles
    // Lowest possible valid move amongst all doubles
    // Lowest possible valid move amongst all triples
    // Lowest possible valid move amongst all quads
    // Picks a higher double over a lower single
    // Picks higher triple over lower double, single
    // Picks higher quad over lower single, double, triple
    // Plays 2s when possible, but not always, depends on specialCardModifier
    // Plays jokers when possible, but not always, depends on specialCardModifier

    describe("When gameState is Empty") {

      val playerIndicators: PlayerIndicators = PlayerIndicators(Hand(List.empty))

      val allSingles = Moves(List(Move(List(SIX_Spade)), Move(List(SEVEN_Heart)), Move(List(EIGHT_Diamond))))
      val allDoubles= Moves(List(
        Move(List(SIX_Heart, SIX_Spade)),
        Move(List(SEVEN_Club, SEVEN_Heart)),
        Move(List(EIGHT_Diamond, EIGHT_Club))))
      val allTriples = Moves(List(
        Move(List(SIX_Club, SIX_Heart, SIX_Spade)),
        Move(List(SEVEN_Diamond, SEVEN_Club, SEVEN_Heart)),
        Move(List(EIGHT_Diamond, EIGHT_Club, EIGHT_Heart))))
      val allQuads = Moves(List(
        Move(List(SIX_Diamond, SIX_Club, SIX_Heart, SIX_Spade)),
        Move(List(SEVEN_Diamond, SEVEN_Club, SEVEN_Heart, SEVEN_Spade)),
        Move(List(EIGHT_Diamond, EIGHT_Club, EIGHT_Heart, EIGHT_Spade))))

      describe("When the validMoves are all singles") {
        it("Should pick the lowest single") {
          assert(GameEngine.getNextMove(allSingles, Move(List.empty))(GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(allSingles.moves.head))
        }
      }

      describe("When the validMoves are all doubles") {
        it("Should pick the lowest double") {
          assert(GameEngine.getNextMove(allDoubles, Move(List.empty))(GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(allDoubles.moves.head))
        }
      }

      describe("When the validMoves are all triples") {
        it("Should pick the lowest triple") {
          assert(GameEngine.getNextMove(allTriples, Move(List.empty))(GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(allTriples.moves.head))
        }
      }

      describe("When the validMoves are all quads") {
        it("Should pick the lowest quad") {
          assert(GameEngine.getNextMove(allQuads, Move(List.empty))(GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(allQuads.moves.head))
        }
      }

      describe("When there is a slightly higher double than a lower single") {
        it("Should pick the slightly higher double") {
          val double6s = Move(List(SIX_Diamond, SIX_Club))
          val validMoves: Moves = Moves(List(
            Move(List(FIVE_Spade)),
            double6s
          ))
          val gameState = Move(List.empty)
          assert(GameEngine.getNextMove(validMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(double6s))
        }
      }

      describe("When there is a slightly higher triple than a lower double") {
        it("Should pick the slightly higher triple") {
          val double5s = Move(List(FIVE_Club, FIVE_Spade))
          val triple6s = Move(List(SIX_Diamond, SIX_Club, SIX_Heart))
          val validMoves: Moves = Moves(List(double5s, triple6s))
          val gameState = Move(List.empty)
          assert(GameEngine.getNextMove(validMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(triple6s))
        }
      }

      describe("When there is a slightly higher triple than a lower single") {
        it("Should pick the slightly higher triple") {
          val single5 = Move(List(FIVE_Club))
          val triple6s = Move(List(SIX_Diamond, SIX_Club, SIX_Heart))
          val validMoves: Moves = Moves(List(single5, triple6s))
          val gameState = Move(List.empty)
          assert(GameEngine.getNextMove(validMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(triple6s))
        }
      }

      describe("When there is a slightly higher quad than a lower triple") {
        it("Should pick the slightly higher quad") {
          val triple5s = Move(List(FIVE_Diamond, FIVE_Club, FIVE_Heart))
          val quad7s = Move(List(SEVEN_Diamond, SEVEN_Club, SEVEN_Heart, SEVEN_Spade))
          val validMoves: Moves = Moves(List(triple5s, quad7s))
          val gameState = Move(List.empty)
          assert(GameEngine.getNextMove(validMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(quad7s))
        }
      }

      describe("When there is a slightly higher quad than a lower double") {
        it("Should pick the slightly higher quad") {
          val double5s = Move(List(FIVE_Diamond, FIVE_Club))
          val quad7s = Move(List(SEVEN_Diamond, SEVEN_Club, SEVEN_Heart, SEVEN_Spade))
          val validMoves: Moves = Moves(List(double5s, quad7s))
          val gameState = Move(List.empty)
          assert(GameEngine.getNextMove(validMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(quad7s))
        }
      }

      describe("When there is a slightly higher quad than a lower single") {
        it("Should pick the slightly higher quad") {
          val single5 = Move(List(FIVE_Diamond))
          val quad7s = Move(List(SEVEN_Diamond, SEVEN_Club, SEVEN_Heart, SEVEN_Spade))
          val validMoves: Moves = Moves(List(single5, quad7s))
          val gameState = Move(List.empty)
          assert(GameEngine.getNextMove(validMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(quad7s))
        }
      }

      describe("When special moves are available") {
        val gameState = Move(List.empty)
        val randomHand = Hand(GameUtilities.dealNewHand(4, Consants.totalNumberOfCards).listOfCards.slice(0, 6))
        val playerInd = PlayerIndicators(randomHand)
        val joker = Move(List(Joker))
        val single2 = Move(List(TWO_Diamond))
        val double2 = Move(List(TWO_Diamond, TWO_Club))
        val triple2 = Move(List(TWO_Diamond, TWO_Club, TWO_Heart))

        describe("When special move is comprised of a single or multiple 2s") {
          it("Should return single2 or None when only single 2s are valid moves") {
            val validMoves = Moves(List(single2))
            val result = GameEngine.getNextMove(validMoves, gameState)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
            assert(result.contains(single2) || result.isEmpty)
          }

          it("Should return double2 or None when only double2s are valid moves") {
            val validMoves = Moves(List(double2))
            val result = GameEngine.getNextMove(validMoves, gameState)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
            assert(result.contains(double2) || result.isEmpty)
          }

          it("Should return triple2 or None when only triple2s are valid moves") {
            val validMoves = Moves(List(triple2))
            val result = GameEngine.getNextMove(validMoves, gameState)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
            assert(result.contains(triple2) || result.isEmpty)
          }

          it("Should return Joker or None when only Joker is a valid move") {
            val validMoves = Moves(List(joker))
            val result = GameEngine.getNextMove(validMoves, gameState)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
            assert(result.contains(joker) || result.isEmpty)
          }
        }
      }

      describe("When moves involving WildCards are available and wildCardPenalty is almost 0") {
        val gameState = Move(List.empty)

        it("Should play a single 3 as an ACE when no other singles are available") {
          val validMoves = Moves(List(Move(List(THREE_Club(14)))))
          assert(GameEngine.getNextMove(validMoves, gameState)(GameEngine.applyNormalCardMoveHeuristic,
            PlayerIndicators(Hand(List.empty))).contains(Move(List(THREE_Club(14)))))
        }

        it("Should pick the lower double involving a wildcard") {
          val move1 = Move(List(THREE_Club(5), FIVE_Spade))
          val move2 = Move(List(SIX_Diamond, SIX_Club))
          val validMoves = Moves(List(move1, move2))
          assert(GameEngine.getNextMove(validMoves, gameState)(GameEngine.applyNormalCardMoveHeuristic,
            PlayerIndicators(Hand(List.empty))).contains(move1))
        }

        it("Should pick the lower triple involving the wildcard") {
          val move1 = Move(List(THREE_Club(5), THREE_Heart(5), FIVE_Spade))
          val move2 = Move(List(SIX_Diamond, SIX_Club, SIX_Heart))
          val validMoves = Moves(List(move1, move2))
          assert(GameEngine.getNextMove(validMoves, gameState)(GameEngine.applyNormalCardMoveHeuristic,
            PlayerIndicators(Hand(List.empty))).contains(move1))
        }

        it("Should pick the lower quad involving the wildcard") {
          val move1 = Move(List(THREE_Club(5), THREE_Heart(5), THREE_Spade(5), FIVE_Spade))
          val move2 = Move(List(SIX_Diamond, SIX_Club, SIX_Heart, SIX_Club))
          val validMoves = Moves(List(move1, move2))
          assert(GameEngine.getNextMove(validMoves, gameState)(GameEngine.applyNormalCardMoveHeuristic,
            PlayerIndicators(Hand(List.empty))).contains(move1))
        }
      }

    }

    describe("When gameState is comprised only of NormalCards") {

      // When gameState is NON-EMPTY and a normal Card
      // Lowest possible delta between gameState and nextMove for singles
      // Picks a higher single card than a lower single that involves breaking a double
      // Picks a higher double than a lower doubles that involves breaking a triple
      // Picks a higher triple than a lower triple that involves breaking a quad
      // Plays a 2 when it is a validMove (no other normal cards) or None
      // Plays multiple twos or None
      // Plays Joker or NONE when it is a single, double
      // Plays Joker or NONE when it is a triple, quad

      val allSingles = Moves(List(
        Move(List(FIVE_Diamond)),
        Move(List(SIX_Spade)),
        Move(List(SEVEN_Diamond))))

      val sampleHand = Hand(List(
        THREE_Diamond, THREE_Club, THREE_Heart,  THREE_Spade,
        FOUR_Diamond, FOUR_Club, FOUR_Heart, FOUR_Spade,
        FIVE_Diamond, FIVE_Club, FIVE_Heart,
        SIX_Diamond, SIX_Club,
        SEVEN_Diamond
      ))
      val playerIndicators = PlayerIndicators(sampleHand)

      describe("When gameState is a low single") {
        it("Should pick the lowest single that minimizes the delta in faceValue") {
          val gameState = Move(List(FIVE_Club))
          assert(GameEngine.getNextMove(allSingles, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(Move(List(SEVEN_Diamond))))
        }

        it("Should pick a higher single without breaking a set than a lower single that involves breaking a set ") {
          val gameState = Move(List(FOUR_Diamond))
          val allValidMoves = GameUtilities.getValidMoves(GameUtilities.assignWildCardsOptimally(
            GameUtilities.getAllMoves(sampleHand.listOfSimilarCards), gameState), gameState)
          assert(GameEngine.getNextMove(allValidMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(Move(List(SEVEN_Diamond))))
        }

        it("Should pick the next move to suit-burn if possible") {
          val gameState = Move(List(FOUR_Diamond))
          val allValidMoves = Moves(List(Move(List(FOUR_Spade))))
          assert(GameEngine.getNextMove(allValidMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, PlayerIndicators(Hand(List(FOUR_Spade)))).contains(Move(List(FOUR_Spade))))
        }

        it("Should pick a single-3 to play as a suit burn") {
          val gameState = Move(List(TEN_Club))
          val allValidMoves = GameUtilities.getValidMoves(
            GameUtilities.assignWildCardsOptimally(GameUtilities.getAllMoves(sampleHand.listOfSimilarCards), gameState), gameState)
          assert(GameEngine.getNextMove(allValidMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, PlayerIndicators(Hand(List.empty))).contains(Move(List(THREE_Heart(10)))))
        }

        it("Should pick the real NormalCard to play as a suit burn over a 3 assuming same value") {
          val gameState = Move(List(THREE_Spade(10)))
          val allValidMoves = Moves(List(Move(List(TEN_Spade))))
          assert(GameEngine.getNextMove(allValidMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, PlayerIndicators(Hand(List(TEN_Spade)))).contains(Move(List(TEN_Spade))))
        }

      }

      describe("When gameState is a low double") {
        it("Should pick a higher double without breaking a set than a lower double that involves breaking a set ") {
          val gameState = Move(List(FOUR_Diamond, FOUR_Club))
          val allValidMoves = GameUtilities.getValidMoves(GameUtilities.assignWildCardsOptimally(
            GameUtilities.getAllMoves(sampleHand.listOfSimilarCards), gameState), gameState)
          assert(GameEngine.getNextMove(allValidMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(Move(List(SIX_Diamond, SIX_Club))))
        }

        it("Should pick the next move to suit-burn if possible") {
          val gameState = Move(List(FOUR_Diamond, FOUR_Club))
          val allValidMoves = Moves(List(Move(List(FOUR_Heart, FOUR_Spade))))
          assert(GameEngine.getNextMove(allValidMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, PlayerIndicators(Hand(List(FOUR_Heart, FOUR_Spade))))
            .contains(Move(List(FOUR_Heart, FOUR_Spade))))
        }

        it("Should pick a wildcard to make a double to play as a suit burn") {
          val gameState = Move(List(TEN_Club, TEN_Heart))
          val allValidMoves = GameUtilities.getValidMoves(
            GameUtilities.assignWildCardsOptimally(GameUtilities.getAllMoves(sampleHand.listOfSimilarCards), gameState), gameState)
          assert(GameEngine.getNextMove(allValidMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, PlayerIndicators(Hand(List.empty)))
            .contains(Move(List(THREE_Diamond(10), THREE_Spade(10)))))
        }

        it("Should pick the real NormalCard to play as a suit burn over a 3 assuming same value") {
          val gameState = Move(List(TEN_Club, THREE_Spade(10)))
          val allValidMoves = Moves(List(Move(List(TEN_Diamond, TEN_Spade))))
          assert(GameEngine.getNextMove(allValidMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, PlayerIndicators(Hand(List(TEN_Diamond, TEN_Spade)))).contains(Move(List(TEN_Diamond, TEN_Spade))))
        }

      }

      describe("When gameState is a low triple") {
        it("Should pick a higher triple without breaking a set than a lower triple that involves breaking a set ") {
          val gameState = Move(List(FOUR_Diamond, FOUR_Club, FOUR_Heart))
          val allValidMoves = GameUtilities.getValidMoves(GameUtilities.assignWildCardsOptimally(
            GameUtilities.getAllMoves(sampleHand.listOfSimilarCards), gameState), gameState)
          assert(GameEngine.getNextMove(allValidMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(Move(List(FIVE_Diamond, FIVE_Club, FIVE_Heart))))
        }

        it("Should pick the next move to suit-burn if possible") {
          val gameState = Move(List(FOUR_Diamond, FOUR_Club, FOUR_Heart))
          val allValidMoves = Moves(List(Move(List(THREE_Diamond(4), THREE_Club(4), FOUR_Spade))))
          assert(GameEngine.getNextMove(allValidMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, PlayerIndicators(Hand(List(FOUR_Spade))))
            .contains(Move(List(THREE_Diamond(4), THREE_Club(4), FOUR_Spade))))
        }

        it("Should pick a wildcard to make a triple to play as a suit burn") {
          val gameState = Move(List(TEN_Diamond, TEN_Club, TEN_Heart))
          val allValidMoves = GameUtilities.getValidMoves(
            GameUtilities.assignWildCardsOptimally(GameUtilities.getAllMoves(sampleHand.listOfSimilarCards), gameState), gameState)
          print(allValidMoves)
          assert(GameEngine.getNextMove(allValidMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, PlayerIndicators(Hand(List.empty)))
            .contains(Move(List(THREE_Diamond(10), THREE_Club(10), THREE_Spade(10)))))
        }

        it("Should pick the real NormalCard to play as a suit burn over a 3 assuming same value") {
          val gameState = Move(List(TEN_Club, THREE_Spade(10)))
          val allValidMoves = Moves(List(Move(List(TEN_Diamond, TEN_Spade))))
          assert(GameEngine.getNextMove(allValidMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, PlayerIndicators(Hand(List(TEN_Diamond, TEN_Spade)))).contains(Move(List(TEN_Diamond, TEN_Spade))))
        }
      }

      describe("When gameState is a quad with a Wildcard") {
        it("Should pick the next move to suit-burn if possible") {
          val gameState = Move(List(THREE_Diamond(4), THREE_Club(4), FOUR_Club, FOUR_Heart))
          val allValidMoves = Moves(List(Move(List(THREE_Heart(4), THREE_Spade(4),
            FOUR_Diamond, FOUR_Spade))))
          assert(GameEngine.getNextMove(allValidMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, PlayerIndicators(Hand(List(FOUR_Diamond, FOUR_Spade))))
            .contains(Move(List(THREE_Heart(4), THREE_Spade(4), FOUR_Diamond, FOUR_Spade))))
        }

        it("Should burn it with the real card of its type") {
          val gameState = Move(List(TEN_Diamond, TEN_Club, TEN_Heart, THREE_Spade(10)))
          val allValidMoves = Moves(List(Move(List(THREE_Diamond(10), THREE_Club(10), THREE_Heart(10),  TEN_Spade))))
          assert(GameEngine.getNextMove(allValidMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, PlayerIndicators(Hand(List(TEN_Spade)))).contains(allValidMoves.moves.head))
        }
      }

      describe("When specialMoves are available") {
        val gameState = Move(List(ACE_Spade))
        val randomHand = Hand(GameUtilities.dealNewHand(4, Consants.totalNumberOfCards).listOfCards.slice(0, 6))
        val playerInd = PlayerIndicators(randomHand)
        val joker = Move(List(Joker))
        val single2 = Move(List(TWO_Diamond))
        val double2 = Move(List(TWO_Diamond, TWO_Club))
        val triple2 = Move(List(TWO_Diamond, TWO_Club, TWO_Heart))

        it("Returns the single2 or none when played on top of a nonEmpty normal card"){
          val result = GameEngine.getNextMove(Moves(List(single2)), gameState)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
          assert(result.contains(single2) || result.isEmpty)
        }

        it("Returns the double2 or none when played on top of a nonEmpty normal card"){
          val currentGameState = Move(List(ACE_Diamond, ACE_Club, ACE_Heart))
          val result = GameEngine.getNextMove(Moves(List(double2)), currentGameState)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
          assert(result.contains(double2) || result.isEmpty)
        }

        it("Returns the triple2 or none when played on top of a nonEmpty normal cards"){
          val currentGameState = Move(List(ACE_Diamond, ACE_Club, ACE_Heart, ACE_Spade))
          val result = GameEngine.getNextMove(Moves(List(triple2)), currentGameState)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
          assert(result.contains(triple2) || result.isEmpty)
        }

        it("Returns the joker or none when played on top of a nonEmpty normal card(s)"){
          val currentGameState2 = Move(List(ACE_Diamond, ACE_Club, ACE_Heart))
          val currentGameState3 = Move(List(ACE_Diamond, ACE_Club, ACE_Heart, ACE_Spade))
          val result = GameEngine.getNextMove(Moves(List(joker)), gameState)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
          val result2 = GameEngine.getNextMove(Moves(List(joker)), currentGameState2)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
          val result3 = GameEngine.getNextMove(Moves(List(joker)), currentGameState3)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
          assert(result.contains(joker) || result.isEmpty)
          assert(result2.contains(joker) || result2.isEmpty)
          assert(result3.contains(joker) || result3.isEmpty)
        }
      }

      describe("When the only valid moves involve WildCards") {

        describe("When wilcardPenaltyModifier is sufficiently low enough") {

          it("Should return the wildcard move as an ACE if it cannot burn ") {
            val hand = Hand(List(THREE_Heart, FOUR_Spade, SIX_Club))
            val pi = PlayerIndicators(hand)
            val gameState = Move(List(TEN_Spade))
            val allValidMoves = Moves(List(Move(List(THREE_Heart(14)))))
            println(allValidMoves)
            assert(GameEngine.getNextMove(allValidMoves, gameState)
            (GameEngine.applyNormalCardMoveHeuristic, pi).contains(Move(List(THREE_Heart(14)))))
          }

          it("Should return the wildcard move as the same face value of gameState if it can burn") {
            val hand = Hand(List(THREE_Heart, FOUR_Spade, SIX_Club))
            val pi = PlayerIndicators(hand)
            val gameState = Move(List(TEN_Club))
            val allValidMoves = Moves(List(Move(List(THREE_Heart(10)))))
            println(allValidMoves)
            assert(GameEngine.getNextMove(allValidMoves, gameState)
            (GameEngine.applyNormalCardMoveHeuristic, pi).contains(Move(List(THREE_Heart(10)))))
          }
        }
      }

      describe("When wildcardPenaltyModifier is sufficiently high enough") {
        val aHand = Hand(List(THREE_Diamond, THREE_Club, THREE_Heart, THREE_Spade, FIVE_Diamond, FIVE_Club, FIVE_Heart,
          SIX_Club, SEVEN_Diamond, EIGHT_Spade, NINE_Spade, TEN_Club, JACK_Heart, QUEEN_Spade, KING_Diamond))
        val pi = PlayerIndicators(aHand)

        it("Should not return the singular wildcard move") {
          val gameState = Move(List(FOUR_Spade))
          val allValidMoves = Moves(List(Move(List(THREE_Heart(5)))))
          println(pi.wildCardPenaltyModifier)
          println(GameEngine.wildCardUsagePenalty(allValidMoves.moves.head, pi.wildCardPenaltyModifier))
          assert(GameEngine.getNextMove(allValidMoves, gameState)(GameEngine.applyNormalCardMoveHeuristic, pi).isEmpty)
        }

        it("Should not return the double involving a wildcard") {
          val gameState = Move(List(FOUR_Diamond, FOUR_Spade))
          val allValidMoves = Moves(List(Move(List(THREE_Heart(5), FIVE_Diamond))))
          assert(GameEngine.getNextMove(allValidMoves, gameState)(GameEngine.applyNormalCardMoveHeuristic, pi).isEmpty)
        }

        it("Should not return the triple involving two wildcards") {
          val gameState = Move(List(FOUR_Diamond, FOUR_Club, FOUR_Spade))
          val allValidMoves = Moves(List(Move(List(THREE_Heart(5), THREE_Spade(5), FIVE_Club))))
          assert(GameEngine.getNextMove(allValidMoves, gameState)(GameEngine.applyNormalCardMoveHeuristic, pi).isEmpty)
        }

        it("Should not return the quad involving three wildcards") {
          val gameState = Move(List(FOUR_Diamond, FOUR_Club, FOUR_Heart, FOUR_Spade))
          val allValidMoves = Moves(List(Move(List(THREE_Club(5), THREE_Heart(5), THREE_Spade(5), FIVE_Spade))))
          println(pi.wildCardPenaltyModifier)
          println(pi.getListSetSizeForCard(allValidMoves.moves.head))
          assert(GameEngine.getNextMove(allValidMoves, gameState)(GameEngine.applyNormalCardMoveHeuristic, pi).isEmpty)
        }

        it("Should not return the quad comprised fully of wildcards") {
          val gameState = Move(List(FOUR_Diamond, FOUR_Club, FOUR_Heart, FOUR_Spade))
          val allValidMoves = Moves(List(Move(List(THREE_Club(5), THREE_Heart(5),
            THREE_Club(5), THREE_Spade(5)))))
          println(pi.wildCardPenaltyModifier)
          println(GameEngine.wildCardUsagePenalty(allValidMoves.moves.head, pi.wildCardPenaltyModifier))
          assert(GameEngine.getNextMove(allValidMoves, gameState)(GameEngine.applyNormalCardMoveHeuristic, pi).isEmpty)
        }

      }

    }

    describe("When gameState is comprised of a single/multiple 2s") {
      // WHen gameState is a TWO
      // Prioritizes off-by-one suit-burns (Diamonds, Hearts)
      // Plays 2 or None otherwise
      // Plays Joker or None otherwise
      val twoGameState = Move(List(TWO_Diamond))
      val betterTwoGameState = Move(List(TWO_Club))
      val twoGameState2 = Move(List(TWO_Heart))
      val betterTwoGameState2 = Move(List(TWO_Spade))
      val two2GameState = Move(List(TWO_Diamond, TWO_Club))
      val betterTwo2GameState = Move(List(TWO_Heart, TWO_Spade))
      val randomHand = Hand(GameUtilities.dealNewHand(4, Consants.totalNumberOfCards).listOfCards.slice(0, 6))
      val playerInd = PlayerIndicators(randomHand)

      it("Should play a 2, if its suit is one-greater-than that of the gameState 2") {
        assert(GameEngine.getNextMove(Moves(List(betterTwoGameState, twoGameState2, betterTwoGameState2)), twoGameState)
        (GameEngine.applySpecialCardMoveHeuristic, playerInd).contains(betterTwoGameState))

        assert(GameEngine.getNextMove(Moves(List(betterTwoGameState2)), twoGameState2)
        (GameEngine.applySpecialCardMoveHeuristic, playerInd).contains(betterTwoGameState2))
      }

      it("Should return either a 2, or none, if the suit difference is > 1") {
        val result = GameEngine.getNextMove(Moves(List(twoGameState2)), twoGameState)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
        assert(result.contains(twoGameState2) || result.isEmpty)
      }

      it("Should return either two 2s, or none, if the gameState involves two 2s") {
        val result = GameEngine.getNextMove(Moves(List(betterTwo2GameState)), two2GameState)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
        assert(result.contains(betterTwo2GameState) || result.isEmpty)
      }

      it("Should return either Joker or None, if gameState involves single or two 2s"){
        val result1 = GameEngine.getNextMove(Moves(List(Move(List(Joker)))), betterTwo2GameState)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
        assert(result1.contains(Move(List(Joker))) || result1.isEmpty)

        val result2 = GameEngine.getNextMove(Moves(List(Move(List(Joker)))), betterTwoGameState2)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
        assert(result2.contains(Move(List(Joker))) || result2.isEmpty)
      }
    }

  }

  describe("tests for getNextMoveWrapper()") {
    // Test for the following
    // If only special moves available - choose other heuristic and return None or result
    // If only normal card moves available - choose normal heuristic and return result
    // If there is a mix of both - choose normal heuristic and return result
    describe("When validMoves is empty") {
      it("Should return None") {
       assert(GameEngine.getNextMoveWrapper(Moves(List.empty), Move(List.empty))(PlayerIndicators(Hand(List.empty))).isEmpty)
      }
    }

    describe("When only special moves are available") {
      it("Should return either None or a move involving special card") {
        val randomHand = Hand(GameUtilities.dealNewHand(4, Consants.totalNumberOfCards).listOfCards.slice(0, 6))
        val playerInd = PlayerIndicators(randomHand)
        val validMoves = Moves(List(
          Move(List(TWO_Diamond)),
          Move(List(TWO_Heart)),
          Move(List(TWO_Diamond, TWO_Heart)),
          Move(List(Joker)),
        ))
        val gameState = Move(List.empty)
        val result = GameEngine.getNextMoveWrapper(validMoves, gameState)(playerInd)
        assert(result.isEmpty || result.get.cards.map{
        case s: SpecialCard => true
        case n: NormalCard => false
        case w: WildCard => false
        case j: Card => true}.forall(x => x))
      }
    }

    describe("When only normal moves (moves with wildcards/normalcards) are available") {
      it("Should definitively return a move") {
        val hand = Hand(List(THREE_Spade, SIX_Diamond, QUEEN_Heart, KING_Club, KING_Heart))
        val playerInd = PlayerIndicators(hand)
        val validMoves = Moves(List(
          Move(List(THREE_Spade(14))),
          Move(List(SIX_Diamond)),
          Move(List(QUEEN_Heart)),
          Move(List(KING_Heart)),
          Move(List(KING_Club)),
          Move(List(KING_Club, KING_Heart))
        ))
        val gameState = Move(List.empty)
        val result = GameEngine.getNextMoveWrapper(validMoves, gameState)(playerInd)
        assert(result.contains(Move(List(SIX_Diamond))))
      }
    }

    describe("When both normal (moves with normalcard/wildcard) and special moves are available") {
      it("Should only return a normalCard move and NOT a specialCard move") {
        val hand = Hand(List(THREE_Spade, SIX_Diamond, QUEEN_Heart, KING_Diamond, KING_Heart, TWO_Spade, Joker))
        val playerInd = PlayerIndicators(hand)
        val validMoves = Moves(List(
          Move(List(THREE_Spade(14))),
          Move(List(SEVEN_Diamond)),
          Move(List(QUEEN_Heart)),
          Move(List(KING_Diamond)),
          Move(List(KING_Heart)),
          Move(List(KING_Diamond, KING_Heart)),
          Move(List(TWO_Spade)),
          Move(List(Joker)),
        ))
        val gameState = Move(List.empty)
        val result = GameEngine.getNextMoveWrapper(validMoves, gameState)(playerInd)
        assert(result.contains(Move(List(SEVEN_Diamond))))
        assert(result.get.cards match {
          case List(NormalCard(_,_), _*) | List(WildCard(_,_,_), _*) => true
          case _ => false
        })
      }
    }

  }

  describe("tests for applyNormalCardMoveHeuristic()") {

    describe("Throws exception when") {

      it("Move involves a joker") {
        assertThrows[IllegalHeuristicFunctionException](GameEngine.applyNormalCardMoveHeuristic(Move(List(Joker)), Move(List.empty)))
      }

      describe("When move involves a 2") {
        it("is a single 2") {
          assertThrows[IllegalHeuristicFunctionException](GameEngine.
            applyNormalCardMoveHeuristic(Move(List(TWO_Diamond)), Move(List.empty)))
        }
        it("is a double 2") {
          assertThrows[IllegalHeuristicFunctionException](GameEngine.
            applyNormalCardMoveHeuristic(Move(List(TWO_Diamond, TWO_Heart)),
              Move(List.empty)))
        }
        it("is a triple 2") {
          assertThrows[IllegalHeuristicFunctionException](GameEngine.
            applyNormalCardMoveHeuristic(Move(List(TWO_Diamond, TWO_Heart, TWO_Spade)),
              Move(List.empty)))
        }
      }

    }

    describe("When gameState is Empty") {
      val emptyGameState = Move(List.empty)

      describe("When validMove is a single 4")  {
        it("should return value") {
          assert(GameEngine.applyNormalCardMoveHeuristic(Move(List(FOUR_Heart)), emptyGameState).likelihood == 0.25)
        }
      }

      describe("When validMove is double4s")  {
        it("should return value") {
          assert(GameEngine.applyNormalCardMoveHeuristic(
            Move(List(FOUR_Heart, FOUR_Spade)),
            emptyGameState).likelihood === 0.3)
        }
      }

      describe("When validMove is triple4s")  {
        it("should return value") {
          assert(GameEngine.applyNormalCardMoveHeuristic(
            Move(List(FOUR_Club, FOUR_Heart, FOUR_Spade)),
            emptyGameState).likelihood === 0.35)
        }
      }

      describe("When validMove is quad4s")  {
        it("should return value") {
          assert(GameEngine.applyNormalCardMoveHeuristic(
            Move(List(FOUR_Diamond, FOUR_Club, FOUR_Heart, FOUR_Spade)),
            emptyGameState).likelihood === 0.4)
        }
      }

      describe("When validMove involves a WildCard in it") {
        it("Should return value less than when no wildcards are involved") {
          assert(GameEngine.applyNormalCardMoveHeuristic(
            Move(List(THREE_Heart(4), FOUR_Club, FOUR_Heart, FOUR_Spade)),
            emptyGameState).likelihood < 0.415)
        }

        it("Should return value less than when one wildcard is involved") {
          assert(GameEngine.applyNormalCardMoveHeuristic(
            Move(List(THREE_Heart(4), THREE_Spade(4), FOUR_Heart, FOUR_Spade)),
            emptyGameState).likelihood < 0.406)
        }

        it("Should return value less than when two wildcards are involved") {
          assert(GameEngine.applyNormalCardMoveHeuristic(
            Move(List(THREE_Diamond(4), THREE_Heart(4), THREE_Spade(4), FOUR_Spade)),
            emptyGameState).likelihood < 0.404)
        }
      }
    }

    describe("When gameState is NonEmpty") {

      describe("When gameState is a single") {
        val hand = Hand(List(NormalCard(NINE, Spade)))
        it("should return the right value") {
          val gameState = Move(List(FIVE_Spade))
          val validMove = Move(List(NINE_Diamond))
          assert(GameEngine.applyNormalCardMoveHeuristic(validMove, gameState, PlayerIndicators(hand)).likelihood === 0.824)
        }
      }

      describe("When gameState is a double") {
        val hand = Hand(List(NormalCard(NINE, Club), NormalCard(NINE, Spade)))
        it("should return the right value") {
          val gameState = Move(List(FIVE_Club, FIVE_Spade))
          val validMove = Move(List(NINE_Diamond, NINE_Spade))
          assert(GameEngine.applyNormalCardMoveHeuristic(validMove, gameState, PlayerIndicators(hand)).likelihood === 0.824)
        }
      }

      describe("When gameState is a triple") {
        it("should return the right value") {
          val hand = Hand(List(NINE_Diamond, NINE_Club, NINE_Spade))
          val gameState = Move(List(FIVE_Club, FIVE_Heart, FIVE_Spade))
          val validMove = Move(List(NINE_Diamond, NINE_Club, NINE_Spade))
          assert(GameEngine.applyNormalCardMoveHeuristic(validMove, gameState, PlayerIndicators(hand)).likelihood === 0.824)
        }
      }

      describe("When gameState is a quadruple") {
        val hand = Hand(List(NINE_Diamond, NINE_Club, NINE_Heart, NINE_Spade))
        it("should return the right value") {
          val gameState = Move(List(FIVE_Diamond, FIVE_Club, FIVE_Heart, FIVE_Spade))
          val validMove = Move(List(NINE_Diamond, NINE_Club, NINE_Heart, NINE_Spade))
          assert(GameEngine.applyNormalCardMoveHeuristic(validMove, gameState, PlayerIndicators(hand)).likelihood === 0.824)
        }
      }

      describe("When move involves WildCard(s) in it ") {
        it("Should return value less than without a wildcard") {
          val hand = Hand(List(NINE_Diamond, NINE_Club, NINE_Heart, NINE_Spade))
          val gameState = Move(List(FIVE_Diamond, FIVE_Club, FIVE_Heart, FIVE_Spade))
          val validMove = Move(List(THREE_Diamond(9), NINE_Club, NINE_Heart, NINE_Spade))
          assert(GameEngine.applyNormalCardMoveHeuristic(validMove, gameState, PlayerIndicators(hand)).likelihood < 0.824)
        }

        it("Should return value less than with one wildcard") {
          val hand = Hand(List(NINE_Diamond, NINE_Club, NINE_Heart, NINE_Spade))
          val gameState = Move(List(FIVE_Diamond, FIVE_Club, FIVE_Heart, FIVE_Spade))
          val validMove = Move(List(THREE_Diamond(9), THREE_Club(9), NINE_Heart, NINE_Spade))
          assert(GameEngine.applyNormalCardMoveHeuristic(validMove, gameState, PlayerIndicators(hand)).likelihood < 0.416)
        }

        it("Should return value less than with two wildcards") {
          val hand = Hand(List(NINE_Diamond, NINE_Club, NINE_Heart, NINE_Spade))
          val gameState = Move(List(FIVE_Diamond, FIVE_Club, FIVE_Heart, FIVE_Spade))
          val validMove = Move(List(THREE_Diamond(9), THREE_Club(9), THREE_Heart(9), NINE_Spade))
          assert(GameEngine.applyNormalCardMoveHeuristic(validMove, gameState, PlayerIndicators(hand)).likelihood < 0.277)
        }

      }
    }

  }

  describe("tests for getSpecialCardMoveHeuristic()") {

    val hand = Hand(List(
      SEVEN_Heart,
      NINE_Diamond,
      JACK_Club, JACK_Diamond,
      ACE_Spade,
      TWO_Diamond, TWO_Club, TWO_Heart, Joker
    ))

    val playerIndicators = PlayerIndicators(hand)

    describe("Throws exception when") {
      it("Move involves a single NormalCard") {
        assertThrows[IllegalHeuristicFunctionException](GameEngine.applySpecialCardMoveHeuristic(Move(List(EIGHT_Heart)), Move(List.empty)))
      }

      it("Move involves multiple of NormalCard"){
        assertThrows[IllegalHeuristicFunctionException](GameEngine.applySpecialCardMoveHeuristic(
          Move(List(NINE_Club, NINE_Heart, NINE_Spade)), Move(List.empty)))
      }
    }

    describe("When validMove comprises of a Joker") {

      val validMove = Move(List(Joker))

      describe("When gameState is empty") {
        it("Should return playerModifier.specialCardModifier or 0") {
          val result = GameEngine.applySpecialCardMoveHeuristic(validMove, Move(List.empty), playerIndicators)
          assert(result.likelihood == playerIndicators.specialCardModifier || result.likelihood == 0)
        }
      }

      describe("When gameState is a single or a double") {
        it("Should return playerModifier.specialCardModifier or 0") {
          val result = GameEngine.applySpecialCardMoveHeuristic(validMove, Move(List(ACE_Heart)), playerIndicators)
          val result2 = GameEngine.applySpecialCardMoveHeuristic(validMove, Move(List(ACE_Heart, ACE_Spade)), playerIndicators)
          assert(result.likelihood == playerIndicators.specialCardModifier || result.likelihood == 0)
          assert(result2.likelihood == playerIndicators.specialCardModifier || result2.likelihood == 0)
        }
      }

      describe("When gameState is a triple or higher") {
        it("Should return playerModifier.specialCardModifier or 0") {
          val result = GameEngine.applySpecialCardMoveHeuristic(validMove,
            Move(List(ACE_Club, ACE_Heart, ACE_Spade)), playerIndicators)
          val result2 = GameEngine.applySpecialCardMoveHeuristic(validMove,
            Move(List(ACE_Diamond , ACE_Club, ACE_Heart, ACE_Spade)), playerIndicators)
          assert(result.likelihood == playerIndicators.specialCardModifier || result.likelihood == 0)
          assert(result2.likelihood == playerIndicators.specialCardModifier || result2.likelihood == 0)
        }
      }

    }

    describe("When validMove comprises of a single/multiple 2s") {

      val single2 = Move(List(TWO_Heart))
      val double2 = Move(List(TWO_Heart, TWO_Spade))
      val triple2 = Move(List(TWO_Diamond, TWO_Club, TWO_Heart))

      describe("When validMove consists of a single 2") {
        describe("When gameState is empty") {
          it("Should return playerModifier.specialCardModifier or 0") {
            val result = GameEngine.applySpecialCardMoveHeuristic(single2, Move(List.empty), playerIndicators)
            assert(result.likelihood == playerIndicators.specialCardModifier || result.likelihood == 0)
          }
        }

        describe("When a single 2 is played on top of a single NormalCard") {
          it("Should return playerModifier.specialCardModifier or 0") {
            val result = GameEngine.applySpecialCardMoveHeuristic(single2, Move(List(ACE_Diamond)), playerIndicators)
            assert(result.likelihood == playerIndicators.specialCardModifier || result.likelihood == 0)
          }
        }

        describe("When a single 2 is played on top of a double NormalCard"){
          it("Should return playerModifier.specialCardModifier or 0") {
            val result = GameEngine.applySpecialCardMoveHeuristic(single2,
              Move(List(ACE_Diamond, ACE_Spade)), playerIndicators)
            assert(result.likelihood == playerIndicators.specialCardModifier || result.likelihood == 0)
          }
        }

        describe("When a single 2 is played on top of another single 2") {
          describe("When the 2 being played is one-suit-away from the 2 on top") {
            it("Should return playerModifier.specialCardModifier") {
              val result = GameEngine.applySpecialCardMoveHeuristic(single2, Move(List(TWO_Club)), playerIndicators)
              assert(result.likelihood == playerIndicators.specialCardModifier)
            }
          }

          describe("When the 2 being played is not one-suit-away from the 2 on top") {
            it("Should return playerModifier.specialCardModifier or 0") {
              val result = GameEngine.applySpecialCardMoveHeuristic(single2, Move(List(TWO_Diamond)), playerIndicators)
              assert(result.likelihood == playerIndicators.specialCardModifier || result.likelihood == 0)
            }
          }
        }

      }

      describe("When validMove consists of multiple 2s") {
        describe("When two 2s are being played") {
          it("Should return playerModifier.specialCardModifier or 0") {
            val result = GameEngine.applySpecialCardMoveHeuristic(double2,
              Move(List(EIGHT_Diamond, EIGHT_Club, EIGHT_Heart)), playerIndicators)
            assert(result.likelihood == playerIndicators.specialCardModifier || result.likelihood == 0)
          }
        }

        describe("When three 2s are being played") {
          it("Should return playerModifier.specialCardModifier or 0") {
            val result = GameEngine.applySpecialCardMoveHeuristic(triple2,
              Move(List(EIGHT_Diamond, EIGHT_Club, EIGHT_Heart, EIGHT_Spade)), playerIndicators)
            assert(result.likelihood == playerIndicators.specialCardModifier || result.likelihood == 0)
          }
        }

        describe("When two 2s are being played on top of existing two 2s"){
          it("should not care about off-by-one suit-burn preferences since it is still an expensive move and return 0/modifier value") {
            val result = GameEngine.applySpecialCardMoveHeuristic(double2,
              Move(List(TWO_Diamond, TWO_Club)), playerIndicators)
            assert(result.likelihood == playerIndicators.specialCardModifier || result.likelihood == 0)
          }
        }

      }
    }

  }

  describe("tests for applyNormalCardHeuristicWithMoveSizeModifier()") {
    describe("When multiple validMoves exist when gameState is Empty") {
      it("Should return the highest value to the most desirable move of the lot") {
        val move1 = Move(List(FOUR_Spade))
        val move2 = Move(List(FOUR_Heart, FOUR_Spade))
        val move3 = Move(List(FOUR_Club, FOUR_Heart, FOUR_Spade))
        val move4 = Move(List(FOUR_Diamond, FOUR_Club, FOUR_Heart, FOUR_Spade))
        val move5 = Move(List(QUEEN_Diamond, QUEEN_Club, QUEEN_Heart, QUEEN_Spade))
        val move6 = Move(List(QUEEN_Club, QUEEN_Heart, QUEEN_Spade))
        val move7 = Move(List(QUEEN_Heart, QUEEN_Spade))
        val move8 = Move(List(QUEEN_Spade))

        val r1 = GameEngine.applyNormalCardHeuristicWithMoveSizeModifier(move1)
        val r2 = GameEngine.applyNormalCardHeuristicWithMoveSizeModifier(move2)
        val r3 = GameEngine.applyNormalCardHeuristicWithMoveSizeModifier(move3)
        val r4 = GameEngine.applyNormalCardHeuristicWithMoveSizeModifier(move4)
        val r5 = GameEngine.applyNormalCardHeuristicWithMoveSizeModifier(move5)
        val r6 = GameEngine.applyNormalCardHeuristicWithMoveSizeModifier(move6)
        val r7 = GameEngine.applyNormalCardHeuristicWithMoveSizeModifier(move7)
        val r8 = GameEngine.applyNormalCardHeuristicWithMoveSizeModifier(move8)
        val results = List(move1.copy(likelihood=r1),move2.copy(likelihood=r2), move3.copy(likelihood=r3),
                        move4.copy(likelihood=r4), move5.copy(likelihood=r5), move6.copy(likelihood=r6),
                        move7.copy(likelihood=r7), move8.copy(likelihood=r8))
        val expected: List[Double] = List(0.25, 0.3, 0.35, 0.4, 0.2667, 0.2167, 0.1667, 0.1167)
        assert(results.maxBy(_.likelihood).likelihood == r4)
        assert(results.minBy(_.likelihood).likelihood == r8)
        assert(((results zip expected).map(tuple => tuple._1.likelihood === tuple._2).forall(x => x)))

      }
    }
  }

  describe("tests for applyNormalCardHeuristicWithPenaltyForBreakingSets()") {
    val single7 = Move(List(SEVEN_Heart))
    val double7s = Move(List(SEVEN_Heart, SEVEN_Spade))
    val triple7s = Move(List(SEVEN_Diamond, SEVEN_Club, SEVEN_Heart))
    val quad7s = Move(List(SEVEN_Diamond, SEVEN_Club, SEVEN_Heart, SEVEN_Spade))
    val gameState = Move(List(SIX_Heart))

    describe("When the moveFaceValue and the gameStateFaceValue are the same (suit-burn") {
      it("Should return 1") {
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(single7, Move(List(SEVEN_Club)), 1) == 1.0d)
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(double7s,
          Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club))), 2) == 1.0d)
      }
    }

    describe("When it is a single in a move and a single in the Hand") {
      it("Should not be penalized") {
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(single7, gameState, 1) ===
          ((0.22d * (1d / (single7.moveFaceValue - gameState.moveFaceValue + 1)))) + (0.78d * 1 / (1 - single7.parity + 1)))
      }
    }

    describe("When it is a single in a move and a double in the Hand"){
      it("Should be penalized"){
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(single7, gameState, 2) ===
          ((0.22d * (1d/(single7.moveFaceValue - gameState.moveFaceValue + 1)))) + (0.78d * 1/(2 - single7.parity + 1)))
      }
     }

    describe("When it is a single in a move and a triple in the Hand"){
      it("Should be penalized"){
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(single7, gameState, 3) ===
          ((0.22d * (1d/(single7.moveFaceValue - gameState.moveFaceValue + 1)))) + (0.78d * 1/(3 - single7.parity + 1)))
      }
    }

    describe("When it is a single in a move and a quadruple in the Hand"){
      it("Should be penalized"){
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(single7, gameState, 4) ===
          ((0.22d * (1d/(single7.moveFaceValue - gameState.moveFaceValue + 1)))) + (0.78d * 1/(4 - single7.parity + 1)))
      }
    }

    describe("When it is a double in a move and a triple in the Hand"){
      it("Should be penalized"){
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(double7s, gameState, 3) ===
          ((0.22d * (1d/(double7s.moveFaceValue - gameState.moveFaceValue + 1)))) + (0.78d * 1/(3 - double7s.parity + 1)))
      }
    }

    describe("When it is a double in a move and a quadruple in the Hand"){
      it("Should be penalized"){
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(double7s, gameState, 4) ===
          ((0.22d * (1d/(double7s.moveFaceValue - gameState.moveFaceValue + 1)))) + (0.78d * 1/(4 - double7s.parity + 1)))
      }
    }

    describe("When it is a triple in a move and a quadruple in the Hand"){
      it("Should be penalized"){
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(triple7s, gameState, 4) ===
          ((0.22d * (1d/(triple7s.moveFaceValue - gameState.moveFaceValue + 1)))) + (0.78d * 1/(4 - triple7s.parity + 1)))
      }
    }

    describe("When it is a quadruple in a move and a quadruple in the Hand"){
      it("Should not be penalized"){
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(quad7s, gameState, 4) ===
          ((0.22d * (1d/(quad7s.moveFaceValue - gameState.moveFaceValue + 1)))) + (0.78d * 1/(4 - quad7s.parity + 1)))
      }
    }

    describe("When it is a triple in a move and a triple in the Hand"){
      it("Should not be penalized"){
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(triple7s, gameState, 3) ===
          ((0.22d * (1d/(triple7s.moveFaceValue - gameState.moveFaceValue + 1)))) + (0.78d * 1/(3 - triple7s.parity + 1)))
      }
    }

    describe("When it is a double in a move and a double in the Hand"){
      it("Should not be penalized"){
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(double7s, gameState, 2) ===
          ((0.22d * (1d/(double7s.moveFaceValue - gameState.moveFaceValue + 1)))) + (0.78d * 1/(2 - double7s.parity + 1)))
      }
    }

    describe("When the move involves WildCards in it") {
     it("Should not be penalized when all cards of its kind are used"){
       val wildcardDouble7s = Move(List(THREE_Heart(7), SEVEN_Spade))
       assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(wildcardDouble7s, gameState, 1) ===
         ((0.22d * (1d/(double7s.moveFaceValue - gameState.moveFaceValue + 1)))) + (0.78d * 1/(1 - wildcardDouble7s.numberOfNormalcards + 1)))
     }

      it("Should be penalized when not all cards of its kind are used") {
        val wildcardQuad7s = Move(List(THREE_Diamond(7), THREE_Club(7), THREE_Heart(7), SEVEN_Spade))
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(wildcardQuad7s, gameState, 3) ===
          ((0.22d * (1d/(double7s.moveFaceValue - gameState.moveFaceValue + 1)))) + (0.78d * 1/(3 - wildcardQuad7s.numberOfNormalcards + 1)))
      }

      it("Should not be penalized when all cards involved are wildcards") {
        val wildcardQuad7s = Move(List(THREE_Diamond(7), THREE_Club(7), THREE_Heart(7), THREE_Spade(7)))
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(wildcardQuad7s, gameState, 0) ===
          ((0.22d * (1d/(double7s.moveFaceValue - gameState.moveFaceValue + 1)))) + (0.78d))
      }

    }

  }

  describe("tests for applyJokerModifierFunction()") {
    describe("When specialCardModifier is 0") {
      it("Should return 0") {
        assert(GameEngine.applyJokerModifierFunction(0, 2) == 0)
      }
    }

    describe("When specialCardModifier is 1") {
      it("Should return 1") {
        assert(GameEngine.applyJokerModifierFunction(1, 2) == 1)
      }
    }

    describe("When specialCardModifier is between 0 and 1") {
      it("Should return the expected value") {
        val specialCardModifier = 0.5
        val gameStateParity = 3
        val expectedValue = scala.math.pow(specialCardModifier, (2/(gameStateParity - 1)))
        assert(GameEngine.applyJokerModifierFunction(specialCardModifier, gameStateParity) == expectedValue)
      }
    }

  }

  describe("tests for applyMultipleTwoModifierFunction()") {
    describe("When specialCardModifier is 0") {
      it("Should return 0") {
        assert(GameEngine.applyMultipleTwoModifierFunction(0, 2) == 0)
      }
    }

    describe("When specialCardModifier is 1") {
      it("Should return 1") {
        assert(GameEngine.applyMultipleTwoModifierFunction(1, 2) == 1)
      }
    }

    describe("When specialCardModifier is between 0 and 1") {
      it("Should return the expected value") {
        val specialCardModifier = 0.5
        val validMoveParity = 3
        val expectedValue = scala.math.pow(specialCardModifier, validMoveParity)
        assert(GameEngine.applyMultipleTwoModifierFunction(specialCardModifier, validMoveParity) == expectedValue)
      }
    }
  }

  describe("Tests for wildCardUsagePenalty") {
    describe("When move does not involve a wildcard in it") {
      it("Should return 0 penalty") {
        val validMove = Move(List(SIX_Club, SIX_Heart, SIX_Spade))
        assert(GameEngine.wildCardUsagePenalty(validMove, 0) == 0)
      }
    }

    describe("When move involves a wildcard in it") {

      describe("When wildcardPenaltyModifier is (1)") {

        val wildcardPenaltyModifier = 1d

        describe("When move parity is the variable") {
          it("should return value when parity is 1") {
             val validMove = Move(List(THREE_Club(6)))
            assert(GameEngine.wildCardUsagePenalty(validMove, wildcardPenaltyModifier) === 0.7586)
          }

          it("should return value < when move parity is 1") {
            val validMove = Move(List(THREE_Club(6), SIX_Diamond))
            assert(GameEngine.wildCardUsagePenalty(validMove, wildcardPenaltyModifier) < 0.7587)
          }

          it("should return value < when move parity is 2") {
            val validMove = Move(List(THREE_Club(6), SIX_Diamond, SIX_Heart))
            assert(GameEngine.wildCardUsagePenalty(validMove, wildcardPenaltyModifier) < 0.6786)
          }

          it("should return value < when move parity is 3") {
            val validMove = Move(List(THREE_Club(6), SIX_Diamond, SIX_Heart, SIX_Spade))
            assert(GameEngine.wildCardUsagePenalty(validMove, wildcardPenaltyModifier) < 0.5986)
          }
        }

        describe("When number of wildcards is the variable") {
          it("Should return value when number of wildcards is 1") {
            val validMove = Move(List(THREE_Spade(6), SIX_Diamond, SIX_Club, SIX_Heart))
            assert(GameEngine.wildCardUsagePenalty(validMove, wildcardPenaltyModifier) === 0.5186)
          }

          it("Should return value > when number of wildcards is 1") {
            val validMove = Move(List(THREE_Club(6), THREE_Spade(6), SIX_Club, SIX_Heart))
            assert(GameEngine.wildCardUsagePenalty(validMove, wildcardPenaltyModifier) > 0.5186)
          }

          it("Should return value > when number of wildcards is 2") {
            val validMove = Move(List(THREE_Diamond(6), THREE_Club(6), THREE_Spade(6), SIX_Heart))
            assert(GameEngine.wildCardUsagePenalty(validMove, wildcardPenaltyModifier) > 0.5436)
          }

          it("Should return value > when number of wildcards is 3") {
            val validMove = Move(List(THREE_Diamond(6), THREE_Club(6), THREE_Heart(6), THREE_Spade(6)))
            assert(GameEngine.wildCardUsagePenalty(validMove, wildcardPenaltyModifier) > 0.5936)
          }

        }

        describe("When move face value is the variable") {
          it("Should return value when faceValue is a 4") {
            val validMove = Move(List(THREE_Heart(4), THREE_Spade(4), FOUR_Club))
            assert(GameEngine.wildCardUsagePenalty(validMove, wildcardPenaltyModifier) === 0.7145)
          }

          it("Should return value < when faceValue is a 4 but > when faceValue is an ACE ") {
            val validMove = Move(List(THREE_Heart(10), THREE_Spade(10), TEN_Club))
            val result = GameEngine.wildCardUsagePenalty(validMove, wildcardPenaltyModifier)
            assert(result < 0.7145 && result === 0.4419)
          }

          it("Should return value when faceValue is an ACE") {
            val validMove = Move(List(THREE_Heart(14), THREE_Spade(14), ACE_Club))
            assert(GameEngine.wildCardUsagePenalty(validMove, wildcardPenaltyModifier) === 0.26)
          }
        }

      }

      describe("When wildcardPenaltyModifier is (0)") {

        val wildCardPenaltyModifier = 0

        it("Should return 0 when move involves all wildcards in it") {
          val validMove = Move(List(THREE_Diamond(6), THREE_Club(6), THREE_Heart(6), THREE_Spade(6)))
          assert(GameEngine.wildCardUsagePenalty(validMove, wildCardPenaltyModifier) == 0)
        }

        it("Should return 0 when moveFaceValue is 4") {
          val validMove = Move(List(THREE_Diamond(4), THREE_Club(4), FOUR_Diamond))
          assert(GameEngine.wildCardUsagePenalty(validMove, wildCardPenaltyModifier) == 0)
        }

        it("Should return 0 when move.parity is 1") {
          val validMove = Move(List(THREE_Diamond(9)))
          assert(GameEngine.wildCardUsagePenalty(validMove, wildCardPenaltyModifier) == 0)
        }

      }

      describe("When wildcardPenaltyModifer is between 0-1  (0.5)") {
        it("Should return right value for a move of parity 4") {
          val validMove = Move(List(THREE_Heart(4), THREE_Spade(4), FOUR_Diamond, FOUR_Spade))
          assert(GameEngine.wildCardUsagePenalty(validMove, 0.5) === GameEngine.wildCardUsagePenalty(validMove, 1) * 0.5)
        }

        it("Should return right value for a move involving all wildcards") {
          val validMove = Move(List(THREE_Diamond(4), THREE_Club(4), THREE_Heart(4), THREE_Spade(4)))
          assert(GameEngine.wildCardUsagePenalty(validMove, 0.5) === GameEngine.wildCardUsagePenalty(validMove, 1) * 0.5)
        }

        it("Should return right value for a move with faceValue ACE") {
          val validMove = Move(List(THREE_Heart(14), THREE_Spade(14), ACE_Diamond, ACE_Spade))
          assert(GameEngine.wildCardUsagePenalty(validMove, 0.5) === GameEngine.wildCardUsagePenalty(validMove, 1) * 0.5)
        }
      }

    }
  }
}
