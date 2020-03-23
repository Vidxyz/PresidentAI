import game.FaceValue._
import game.GameEngine.IllegalHeuristicFunctionException
import game.{GameEngine, GameUtilities, Hand, Joker, Move, Moves, NormalCard, SpecialCard}
import game.Suits._
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.FunSpec
import player.PlayerIndicators

class GameEngineTest extends FunSpec{

  val epsilon = 1e-4f
  implicit val doubleEq: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(epsilon)

  describe("tests for getNextMove()"){

    implicit val playerIndicators: PlayerIndicators = PlayerIndicators(Hand(List.empty))

    describe("When validMoves is empty"){
      it("Should return an Empty game.Move") {
        assert(GameEngine.getNextMoveWrapper(Moves(List.empty), Move(List.empty)) == None)
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
          assert(GameEngine.getNextMoveWrapper(validMoves, gameState).contains(double6s))
        }
      }

      describe("When there is a slightly higher triple than a lower double") {
        it("Should pick the slightly higher triple") {
          val double5s = Move(List(NormalCard(FIVE, Club), NormalCard(FIVE, Spade)))
          val triple6s = Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club), NormalCard(SIX, Heart)))
          val validMoves: Moves = Moves(List(double5s, triple6s))
          val gameState = Move(List.empty)
          assert(GameEngine.getNextMoveWrapper(validMoves, gameState).contains(triple6s))
        }
      }

      describe("When there is a slightly higher quad than a lower triple") {
        it("Should pick the slightly higher quad") {
          val triple5s = Move(List(NormalCard(FIVE, Diamond), NormalCard(FIVE, Club), NormalCard(FIVE, Heart)))
          val quad7s = Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))
          val validMoves: Moves = Moves(List(triple5s, quad7s))
          val gameState = Move(List.empty)
          assert(GameEngine.getNextMoveWrapper(validMoves, gameState).contains(quad7s))
        }
      }

    }
  }

  describe("tests for getNextMoveWrapper()") {

  }

  describe("tests for getNormalCardMoveHeuristic()") {

    describe("Throws exception when") {

      it("game.Move involves a joker") {
        assertThrows[IllegalHeuristicFunctionException](GameEngine.getNormalCardMoveHeuristic(Move(List(Joker)), Move(List.empty)))
      }

      describe("When move involves a 2") {
        it("is a single 2") {
          assertThrows[IllegalHeuristicFunctionException](GameEngine.
            getNormalCardMoveHeuristic(Move(List(SpecialCard(TWO, Diamond))), Move(List.empty)))
        }
        it("is a double 2") {
          assertThrows[IllegalHeuristicFunctionException](GameEngine.
            getNormalCardMoveHeuristic(Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Heart))),
              Move(List.empty)))
        }
        it("is a triple 2") {
          assertThrows[IllegalHeuristicFunctionException](GameEngine.
            getNormalCardMoveHeuristic(Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Heart), SpecialCard(TWO, Spade))),
              Move(List.empty)))
        }
      }

    }

    describe("When gameState is Empty") {
      val emptyGameState = Move(List.empty)

      describe("When validMove is a single 4")  {
        it("should return value") {
          assert(GameEngine.getNormalCardMoveHeuristic(Move(List(NormalCard(FOUR,Heart))), emptyGameState) == 0.25)
        }
      }

      describe("When validMove is double4s")  {
        it("should return value") {
          assert(GameEngine.getNormalCardMoveHeuristic(
            Move(List(NormalCard(FOUR,Heart), NormalCard(FOUR,Spade))),
            emptyGameState) == 0.305)
        }
      }

      describe("When validMove is triple4s")  {
        it("should return value") {
          assert(GameEngine.getNormalCardMoveHeuristic(
            Move(List(NormalCard(FOUR,Club), NormalCard(FOUR,Heart), NormalCard(FOUR,Spade))),
            emptyGameState) == 0.36)
        }
      }

      describe("When validMove is quad4s")  {
        it("should return value") {
          assert(GameEngine.getNormalCardMoveHeuristic(
            Move(List(NormalCard(FOUR,Diamond), NormalCard(FOUR,Club),
              NormalCard(FOUR,Heart), NormalCard(FOUR,Spade))),
            emptyGameState) === 0.415)
        }
      }
    }

    describe("When gameState is NonEmpty") {

      describe("When gameState is a single") {
        val hand = Hand(List(NormalCard(NINE, Spade)))
        it("should return the right value") {
          val gameState = Move(List(NormalCard(FIVE, Spade)))
          val validMove = Move(List(NormalCard(NINE, Diamond)))
          assert(GameEngine.getNormalCardMoveHeuristic(validMove, gameState, PlayerIndicators(hand)) === 0.835)
        }
      }

      describe("When gameState is a double") {
        val hand = Hand(List(NormalCard(NINE, Club), NormalCard(NINE, Spade)))
        it("should return the right value") {
          val gameState = Move(List(NormalCard(FIVE, Club), NormalCard(FIVE, Spade)))
          val validMove = Move(List(NormalCard(NINE, Diamond), NormalCard(NINE, Spade)))
          assert(GameEngine.getNormalCardMoveHeuristic(validMove, gameState, PlayerIndicators(hand)) === 0.835)
        }
      }

      describe("When gameState is a triple") {
        it("should return the right value") {
          val hand = Hand(List(NormalCard(NINE, Diamond), NormalCard(NINE, Club), NormalCard(NINE, Spade)))
          val gameState = Move(List(NormalCard(FIVE, Club), NormalCard(FIVE, Heart), NormalCard(FIVE, Spade)))
          val validMove = Move(List(NormalCard(NINE, Diamond), NormalCard(NINE, Club), NormalCard(NINE, Spade)))
          assert(GameEngine.getNormalCardMoveHeuristic(validMove, gameState, PlayerIndicators(hand)) === 0.835)
        }
      }

      describe("When gameState is a quadruple") {
        val hand = Hand(List(NormalCard(NINE, Diamond), NormalCard(NINE, Club), NormalCard(NINE, Heart), NormalCard(NINE, Spade)))
        it("should return the right value") {
          val gameState = Move(List(NormalCard(FIVE, Diamond), NormalCard(FIVE, Club),
            NormalCard(FIVE, Heart), NormalCard(FIVE, Spade)))
          val validMove = Move(List(NormalCard(NINE, Diamond), NormalCard(NINE, Club),
            NormalCard(NINE, Heart), NormalCard(NINE, Spade)))
          assert(GameEngine.getNormalCardMoveHeuristic(validMove, gameState, PlayerIndicators(hand)) === 0.835)
        }
      }
    }

    //TODO - write unit tests for facevalue tests based on highCardModifier
  }

  describe("tests for getSpecialCardMoveHeuristic()") {

  }

  describe("tests for applyNormalCardHeuristicWithMoveSizeModifier()") {

  }

  describe("tests for applyNormalCardHeuristicWithPenaltyForBreakingSets()") {

  }
}
