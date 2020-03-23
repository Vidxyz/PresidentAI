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
    // Fill this in
  }

  describe("tests for getNormalCardMoveHeuristic()") {

    describe("Throws exception when") {

      it("Move involves a joker") {
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

  }

  describe("tests for getSpecialCardMoveHeuristic()") {

    val hand = Hand(List(
      NormalCard(SEVEN, Heart),
      NormalCard(NINE, Diamond),
      NormalCard(JACK, Club),
      NormalCard(JACK, Diamond),
      NormalCard(ACE, Spade),
      SpecialCard(TWO, Diamond),
      SpecialCard(TWO, Club),
      SpecialCard(TWO, Heart),
      Joker,
    ))

    val playerIndicators = PlayerIndicators(hand)

    describe("Throws exception when") {
      it("Move involves a NormalCard") {
        assertThrows[IllegalHeuristicFunctionException](GameEngine.getSpecialCardMoveHeuristic(Move(List(NormalCard(EIGHT, Heart))), Move(List.empty)))
      }

      it("Move involves a list of NormalCard"){
        assertThrows[IllegalHeuristicFunctionException](GameEngine.getSpecialCardMoveHeuristic(
          Move(List(NormalCard(NINE, Club), NormalCard(NINE, Heart), NormalCard(NINE, Spade))), Move(List.empty)))
      }
    }

    describe("When validMove comprises of a Joker") {

      val validMove = Move(List(Joker))

      describe("When gameState is empty") {
        it("Should return playerModifier.specialCardModifier or 0") {
          val result = GameEngine.getSpecialCardMoveHeuristic(validMove, Move(List.empty), playerIndicators)
          assert(result == playerIndicators.specialCardModifier || result == 0)
        }
      }

      describe("When gameState is a single or a double") {
        it("Should return playerModifier.specialCardModifier or 0") {
          val result = GameEngine.getSpecialCardMoveHeuristic(validMove, Move(List(NormalCard(ACE, Heart))), playerIndicators)
          val result2 = GameEngine.getSpecialCardMoveHeuristic(validMove, Move(List(NormalCard(ACE, Heart), NormalCard(ACE, Spade))), playerIndicators)
          assert(result == playerIndicators.specialCardModifier || result == 0)
          assert(result2 == playerIndicators.specialCardModifier || result2 == 0)
        }
      }

      describe("When gameState is a triple or higher") {
        it("Should return playerModifier.specialCardModifier or 0") {
          val result = GameEngine.getSpecialCardMoveHeuristic(validMove,
            Move(List(NormalCard(ACE, Club), NormalCard(ACE, Heart), NormalCard(ACE, Spade))), playerIndicators)
          val result2 = GameEngine.getSpecialCardMoveHeuristic(validMove,
            Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Club), NormalCard(ACE, Heart), NormalCard(ACE, Spade))), playerIndicators)
          assert(result == playerIndicators.specialCardModifier || result == 0)
          assert(result2 == playerIndicators.specialCardModifier || result2 == 0)
        }
      }

    }

    describe("When validMove comprises of a single/multiple 2s") {

      val single2 = Move(List(SpecialCard(TWO, Heart)))
      val double2 = Move(List(SpecialCard(TWO, Heart), SpecialCard(TWO, Spade)))
      val triple2 = Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club), SpecialCard(TWO, Heart)))

      describe("When validMove consists of a single 2") {
        describe("When gameState is empty") {
          it("Should return playerModifier.specialCardModifier or 0") {
            val result = GameEngine.getSpecialCardMoveHeuristic(single2, Move(List.empty), playerIndicators)
            assert(result == playerIndicators.specialCardModifier || result == 0)
          }
        }

        describe("When a single 2 is played on top of a single NormalCard") {
          it("Should return playerModifier.specialCardModifier or 0") {
            val result = GameEngine.getSpecialCardMoveHeuristic(single2, Move(List(NormalCard(ACE, Diamond))), playerIndicators)
            assert(result == playerIndicators.specialCardModifier || result == 0)
          }
        }

        describe("When a single 2 is played on top of a double NormalCard"){
          it("Should return playerModifier.specialCardModifier or 0") {
            val result = GameEngine.getSpecialCardMoveHeuristic(single2,
              Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Spade))), playerIndicators)
            assert(result == playerIndicators.specialCardModifier || result == 0)
          }
        }

        describe("When a single 2 is played on top of another single 2") {
          describe("When the 2 being played is one-suit-away from the 2 on top") {
            it("Should return playerModifier.specialCardModifier") {
              val result = GameEngine.getSpecialCardMoveHeuristic(single2, Move(List(SpecialCard(TWO, Club))), playerIndicators)
              assert(result == playerIndicators.specialCardModifier)
            }
          }

          describe("When the 2 being played is not one-suit-away from the 2 on top") {
            it("Should return playerModifier.specialCardModifier or 0") {
              val result = GameEngine.getSpecialCardMoveHeuristic(single2, Move(List(SpecialCard(TWO, Diamond))), playerIndicators)
              assert(result == playerIndicators.specialCardModifier || result == 0)
            }
          }
        }

      }

      describe("When validMove consists of multiple 2s") {
        describe("When two 2s are being played") {
          it("Should return playerModifier.specialCardModifier or 0") {
            val result = GameEngine.getSpecialCardMoveHeuristic(double2,
              Move(List(NormalCard(EIGHT, Diamond), NormalCard(EIGHT, Club), NormalCard(EIGHT, Heart))), playerIndicators)
            assert(result == playerIndicators.specialCardModifier || result == 0)
          }
        }

        describe("When three 2s are being played") {
          it("Should return playerModifier.specialCardModifier or 0") {
            val result = GameEngine.getSpecialCardMoveHeuristic(triple2,
              Move(List(NormalCard(EIGHT, Diamond), NormalCard(EIGHT, Club), NormalCard(EIGHT, Heart), NormalCard(EIGHT, Spade))), playerIndicators)
            assert(result == playerIndicators.specialCardModifier || result == 0)
          }
        }

        describe("When two 2s are being played on top of existing two 2s"){
          it("should not care about off-by-one suit-burn preferences since it is still an expensive move and return 0/modifier value") {
            val result = GameEngine.getSpecialCardMoveHeuristic(double2,
              Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club))), playerIndicators)
            assert(result == playerIndicators.specialCardModifier || result == 0)
          }
        }

      }
    }

  }

  describe("tests for applyNormalCardHeuristicWithMoveSizeModifier()") {
    describe("When multiple validMoves exist when gameState is Empty") {
      it("Should return the highest value to the most desirable move of the lot") {
        val move1 = Move(List(NormalCard(FOUR, Spade)))
        val move2 = Move(List(NormalCard(FOUR, Heart), NormalCard(FOUR, Spade)))
        val move3 = Move(List(NormalCard(FOUR, Club), NormalCard(FOUR, Heart), NormalCard(FOUR, Spade)))
        val move4 = Move(List(NormalCard(FOUR, Diamond), NormalCard(FOUR, Club), NormalCard(FOUR, Heart), NormalCard(FOUR, Spade)))
        val move5 = Move(List(NormalCard(QUEEN, Diamond), NormalCard(QUEEN, Club), NormalCard(QUEEN, Heart), NormalCard(QUEEN, Spade)))
        val move6 = Move(List(NormalCard(QUEEN, Club), NormalCard(QUEEN, Heart), NormalCard(QUEEN, Spade)))
        val move7 = Move(List(NormalCard(QUEEN, Heart), NormalCard(QUEEN, Spade)))
        val move8 = Move(List( NormalCard(QUEEN, Spade)))

        val r1 = GameEngine.applyNormalCardHeuristicWithMoveSizeModifier(move1)
        val r2 = GameEngine.applyNormalCardHeuristicWithMoveSizeModifier(move2)
        val r3 = GameEngine.applyNormalCardHeuristicWithMoveSizeModifier(move3)
        val r4 = GameEngine.applyNormalCardHeuristicWithMoveSizeModifier(move4)
        val r5 = GameEngine.applyNormalCardHeuristicWithMoveSizeModifier(move5)
        val r6 = GameEngine.applyNormalCardHeuristicWithMoveSizeModifier(move6)
        val r7 = GameEngine.applyNormalCardHeuristicWithMoveSizeModifier(move7)
        val r8 = GameEngine.applyNormalCardHeuristicWithMoveSizeModifier(move8)
        val results = List(r1, r2, r3, r4, r5, r6, r7, r8)
        val expected: List[Double] = List(0.25, 0.305, 0.36, 0.415, 0.285, 0.23, 0.175, 0.12)

        assert(results.max == r4)
        assert(results.min == r8)
        assert(((results zip expected).map(tuple => tuple._1 === tuple._2).forall(x => x)))

      }
    }
  }

  describe("tests for applyNormalCardHeuristicWithPenaltyForBreakingSets()") {
    val single7 = Move(List(NormalCard(SEVEN, Heart)))
    val double7s = Move(List(NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))
    val triple7s = Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart)))
    val quad7s = Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))
    val gameState = Move(List(NormalCard(SIX, Heart)))

    describe("When it is a single in a move and a single in the Hand") {
      it("Should not be penalized") {
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(single7, gameState, 1) ===
          ((0.22d * (1d / (single7.moveFaceValue - gameState.moveFaceValue)))) + (0.78d * 1 / (1 - single7.parity + 1)))
      }
    }

    describe("When it is a single in a move and a double in the Hand"){
      it("Should be penalized"){
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(single7, gameState, 2) ===
          ((0.22d * (1d/(single7.moveFaceValue - gameState.moveFaceValue)))) + (0.78d * 1/(2 - single7.parity + 1)))
      }
     }

    describe("When it is a single in a move and a triple in the Hand"){
      it("Should be penalized"){
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(single7, gameState, 3) ===
          ((0.22d * (1d/(single7.moveFaceValue - gameState.moveFaceValue)))) + (0.78d * 1/(3 - single7.parity + 1)))
      }
    }

    describe("When it is a single in a move and a quadruple in the Hand"){
      it("Should be penalized"){
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(single7, gameState, 4) ===
          ((0.22d * (1d/(single7.moveFaceValue - gameState.moveFaceValue)))) + (0.78d * 1/(4 - single7.parity + 1)))
      }
    }

    describe("When it is a double in a move and a triple in the Hand"){
      it("Should be penalized"){
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(double7s, gameState, 3) ===
          ((0.22d * (1d/(double7s.moveFaceValue - gameState.moveFaceValue)))) + (0.78d * 1/(3 - double7s.parity + 1)))
      }
    }

    describe("When it is a double in a move and a quadruple in the Hand"){
      it("Should be penalized"){
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(double7s, gameState, 4) ===
          ((0.22d * (1d/(double7s.moveFaceValue - gameState.moveFaceValue)))) + (0.78d * 1/(4 - double7s.parity + 1)))
      }
    }

    describe("When it is a triple in a move and a quadruple in the Hand"){
      it("Should be penalized"){
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(triple7s, gameState, 4) ===
          ((0.22d * (1d/(triple7s.moveFaceValue - gameState.moveFaceValue)))) + (0.78d * 1/(4 - triple7s.parity + 1)))
      }
    }

    describe("When it is a quadruple in a move and a quadruple in the Hand"){
      it("Should not be penalized"){
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(quad7s, gameState, 4) ===
          ((0.22d * (1d/(quad7s.moveFaceValue - gameState.moveFaceValue)))) + (0.78d * 1/(4 - quad7s.parity + 1)))
      }
    }

    describe("When it is a triple in a move and a triple in the Hand"){
      it("Should not be penalized"){
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(triple7s, gameState, 3) ===
          ((0.22d * (1d/(triple7s.moveFaceValue - gameState.moveFaceValue)))) + (0.78d * 1/(3 - triple7s.parity + 1)))
      }
    }

    describe("When it is a double in a move and a double in the Hand"){
      it("Should not be penalized"){
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(double7s, gameState, 2) ===
          ((0.22d * (1d/(double7s.moveFaceValue - gameState.moveFaceValue)))) + (0.78d * 1/(2 - double7s.parity + 1)))
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

  }
}
