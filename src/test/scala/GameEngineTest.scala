import game.FaceValue._
import game.GameEngine.IllegalHeuristicFunctionException
import game.{GameEngine, GameUtilities, Hand, Joker, Move, Moves, NormalCard, SpecialCard}
import game.Suits._
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.FunSpec
import player.PlayerIndicators
import utils.Consants

class GameEngineTest extends FunSpec {

  val epsilon = 1e-4f
  implicit val doubleEq: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(epsilon)

  describe("tests for getNextMove()"){

    describe("When validMoves is empty"){
      it("Should return an None") {
        val playerIndicators: PlayerIndicators = PlayerIndicators(Hand(List.empty))
        assert(GameEngine.getNextMove(Moves(List.empty), Move(List.empty))
        (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).isEmpty)
      }
    }

    describe("When validMoves.size is 1 and it is a normalCard") {
      it("Should return the validMove") {
        val gameState = Move(List(NormalCard(JACK, Heart)))
        val validMoves = Moves(List(Move(List(NormalCard(QUEEN, Diamond)))))
        assert(GameEngine.getNextMove(validMoves, gameState)(GameEngine.applyNormalCardMoveHeuristic, PlayerIndicators(Hand(List(NormalCard(QUEEN, Diamond)))))
          .contains(Move(List(NormalCard(QUEEN, Diamond)))))
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

      val allSingles = Moves(List(Move(List(NormalCard(SIX, Spade))), Move(List(NormalCard(SEVEN, Heart))), Move(List(NormalCard(EIGHT, Diamond)))))
      val allDoubles= Moves(List(
        Move(List(NormalCard(SIX, Heart), NormalCard(SIX, Spade))),
        Move(List(NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart))),
        Move(List(NormalCard(EIGHT, Diamond), NormalCard(EIGHT, Club)))))
      val allTriples = Moves(List(
        Move(List(NormalCard(SIX, Club), NormalCard(SIX, Heart), NormalCard(SIX, Spade))),
        Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart))),
        Move(List(NormalCard(EIGHT, Diamond), NormalCard(EIGHT, Club), NormalCard(EIGHT, Heart)))))
      val allQuads = Moves(List(
        Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club), NormalCard(SIX, Heart), NormalCard(SIX, Spade))),
        Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade))),
        Move(List(NormalCard(EIGHT, Diamond), NormalCard(EIGHT, Club), NormalCard(EIGHT, Heart), NormalCard(EIGHT, Spade)))))

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
          val double6s = Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club)))
          val validMoves: Moves = Moves(List(
            Move(List(NormalCard(FIVE, Spade))),
            double6s
          ))
          val gameState = Move(List.empty)
          assert(GameEngine.getNextMove(validMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(double6s))
        }
      }

      describe("When there is a slightly higher triple than a lower double") {
        it("Should pick the slightly higher triple") {
          val double5s = Move(List(NormalCard(FIVE, Club), NormalCard(FIVE, Spade)))
          val triple6s = Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club), NormalCard(SIX, Heart)))
          val validMoves: Moves = Moves(List(double5s, triple6s))
          val gameState = Move(List.empty)
          assert(GameEngine.getNextMove(validMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(triple6s))
        }
      }

      describe("When there is a slightly higher triple than a lower single") {
        it("Should pick the slightly higher triple") {
          val single5 = Move(List(NormalCard(FIVE, Club)))
          val triple6s = Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club), NormalCard(SIX, Heart)))
          val validMoves: Moves = Moves(List(single5, triple6s))
          val gameState = Move(List.empty)
          assert(GameEngine.getNextMove(validMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(triple6s))
        }
      }

      describe("When there is a slightly higher quad than a lower triple") {
        it("Should pick the slightly higher quad") {
          val triple5s = Move(List(NormalCard(FIVE, Diamond), NormalCard(FIVE, Club), NormalCard(FIVE, Heart)))
          val quad7s = Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))
          val validMoves: Moves = Moves(List(triple5s, quad7s))
          val gameState = Move(List.empty)
          assert(GameEngine.getNextMove(validMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(quad7s))
        }
      }

      describe("When there is a slightly higher quad than a lower double") {
        it("Should pick the slightly higher quad") {
          val double5s = Move(List(NormalCard(FIVE, Diamond), NormalCard(FIVE, Club)))
          val quad7s = Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))
          val validMoves: Moves = Moves(List(double5s, quad7s))
          val gameState = Move(List.empty)
          assert(GameEngine.getNextMove(validMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(quad7s))
        }
      }

      describe("When there is a slightly higher quad than a lower single") {
        it("Should pick the slightly higher quad") {
          val single5 = Move(List(NormalCard(FIVE, Diamond)))
          val quad7s = Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))
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
        val single2 = Move(List(SpecialCard(TWO, Diamond)))
        val double2 = Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club)))
        val triple2 = Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club), SpecialCard(TWO, Heart)))

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
        Move(List(NormalCard(FIVE, Diamond))),
        Move(List(NormalCard(SIX, Spade))),
        Move(List(NormalCard(SEVEN, Diamond)))))

      val sampleHand = Hand(List(
        NormalCard(FOUR, Diamond), NormalCard(FOUR, Club), NormalCard(FOUR, Heart), NormalCard(FOUR, Spade),
        NormalCard(FIVE, Diamond), NormalCard(FIVE, Club), NormalCard(FIVE, Heart),
        NormalCard(SIX, Diamond), NormalCard(SIX, Club),
        NormalCard(SEVEN, Diamond)
      ))
      val playerIndicators = PlayerIndicators(sampleHand)

      describe("When gameState is a low single") {
        it("Should pick the lowest single that minimizes the delta in faceValue") {
          val gameState = Move(List(NormalCard(FIVE, Club)))
          val allValidMoves = GameUtilities.getValidMoves(GameUtilities.getAllMoves(sampleHand.listOfSimilarCards), gameState)
          assert(GameEngine.getNextMove(allSingles, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(allSingles.moves.head))
        }

        it("Should pick a higher single without breaking a set than a lower single that involves breaking a set ") {
          val gameState = Move(List(NormalCard(THREE, Diamond)))
          val allValidMoves = GameUtilities.getValidMoves(GameUtilities.getAllMoves(sampleHand.listOfSimilarCards), gameState)
          assert(GameEngine.getNextMove(allValidMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(Move(List(NormalCard(SEVEN, Diamond)))))
        }

      }

      describe("When gameState is a low double") {
        it("Should pick a higher double without breaking a set than a lower double that involves breaking a set ") {
          val gameState = Move(List(NormalCard(THREE, Diamond), NormalCard(THREE, Club)))
          val allValidMoves = GameUtilities.getValidMoves(GameUtilities.getAllMoves(sampleHand.listOfSimilarCards), gameState)
          assert(GameEngine.getNextMove(allValidMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club)))))
        }
      }

      describe("When gameState is a low triple") {
        it("Should pick a higher triple without breaking a set than a lower triple that involves breaking a set ") {
          val gameState = Move(List(NormalCard(THREE, Diamond), NormalCard(THREE, Club), NormalCard(THREE, Heart)))
          val allValidMoves = GameUtilities.getValidMoves(GameUtilities.getAllMoves(sampleHand.listOfSimilarCards), gameState)
          assert(GameEngine.getNextMove(allValidMoves, gameState)
          (GameEngine.applyNormalCardMoveHeuristic, playerIndicators).contains(Move(List(NormalCard(FIVE, Diamond),
            NormalCard(FIVE, Club), NormalCard(FIVE, Heart)))))
        }
      }

      describe("When specialMoves are available") {
        val gameState = Move(List(NormalCard(ACE, Spade)))
        val randomHand = Hand(GameUtilities.dealNewHand(4, Consants.totalNumberOfCards).listOfCards.slice(0, 6))
        val playerInd = PlayerIndicators(randomHand)
        val joker = Move(List(Joker))
        val single2 = Move(List(SpecialCard(TWO, Diamond)))
        val double2 = Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club)))
        val triple2 = Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club), SpecialCard(TWO, Heart)))

        it("Returns the single2 or none when played on top of a nonEmpty normal card"){
          val result = GameEngine.getNextMove(Moves(List(single2)), gameState)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
          assert(result.contains(single2) || result.isEmpty)
        }

        it("Returns the double2 or none when played on top of a nonEmpty normal card"){
          val currentGameState = Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Club), NormalCard(ACE, Heart)))
          val result = GameEngine.getNextMove(Moves(List(double2)), currentGameState)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
          assert(result.contains(double2) || result.isEmpty)
        }

        it("Returns the triple2 or none when played on top of a nonEmpty normal cards"){
          val currentGameState = Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Club), NormalCard(ACE, Heart), NormalCard(ACE, Spade)))
          val result = GameEngine.getNextMove(Moves(List(triple2)), currentGameState)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
          assert(result.contains(triple2) || result.isEmpty)
        }

        it("Returns the joker or none when played on top of a nonEmpty normal card(s)"){
          val currentGameState2 = Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Club), NormalCard(ACE, Heart)))
          val currentGameState3 = Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Club), NormalCard(ACE, Heart), NormalCard(ACE, Spade)))
          val result = GameEngine.getNextMove(Moves(List(joker)), gameState)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
          val result2 = GameEngine.getNextMove(Moves(List(joker)), currentGameState2)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
          val result3 = GameEngine.getNextMove(Moves(List(joker)), currentGameState3)(GameEngine.applySpecialCardMoveHeuristic, playerInd)
          assert(result.contains(joker) || result.isEmpty)
          assert(result2.contains(joker) || result2.isEmpty)
          assert(result3.contains(joker) || result3.isEmpty)
        }
      }



    }

    describe("When gameState is comprised of a single/multiple 2s") {
      // WHen gameState is a TWO
      // Prioritizes off-by-one suit-burns (Diamonds, Hearts)
      // Plays 2 or None otherwise
      // Plays Joker or None otherwise
      val twoGameState = Move(List(SpecialCard(TWO, Diamond)))
      val betterTwoGameState = Move(List(SpecialCard(TWO, Club)))
      val twoGameState2 = Move(List(SpecialCard(TWO, Heart)))
      val betterTwoGameState2 = Move(List(SpecialCard(TWO, Spade)))
      val two2GameState = Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club)))
      val betterTwo2GameState = Move(List(SpecialCard(TWO, Heart), SpecialCard(TWO, Spade)))
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
          Move(List(SpecialCard(TWO, Diamond))),
          Move(List(SpecialCard(TWO, Heart))),
          Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Heart))),
          Move(List(Joker)),
        ))
        val gameState = Move(List.empty)
        val result = GameEngine.getNextMoveWrapper(validMoves, gameState)(playerInd)
        assert(result.contains(Move(List(SpecialCard(TWO, Diamond)))) || result.isEmpty)
      }
    }

    describe("When only normal moves are available") {
      it("Should definitively return a move") {
        val hand = Hand(List(
          NormalCard(SIX, Diamond),
          NormalCard(QUEEN, Heart),
          NormalCard(ACE, Diamond),
          NormalCard(ACE, Heart),
        ))
        val playerInd = PlayerIndicators(hand)
        val validMoves = Moves(List(
          Move(List(NormalCard(SIX, Diamond))),
          Move(List(NormalCard(QUEEN, Heart))),
          Move(List(NormalCard(ACE, Diamond))),
          Move(List(NormalCard(ACE, Heart))),
          Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Heart)))
        ))
        val gameState = Move(List.empty)
        val result = GameEngine.getNextMoveWrapper(validMoves, gameState)(playerInd)
        assert(result.contains(Move(List(NormalCard(SIX, Diamond)))))
      }
    }

    describe("When both normal and special moves are available") {
      it("Should only return a normalCard move and NOT a specialCard move") {
        val hand = Hand(List(
          NormalCard(SIX, Diamond),
          NormalCard(QUEEN, Heart),
          NormalCard(ACE, Diamond),
          NormalCard(ACE, Heart),
          SpecialCard(TWO, Spade),
          Joker
        ))
        val playerInd = PlayerIndicators(hand)
        val validMoves = Moves(List(
          Move(List(NormalCard(SEVEN, Diamond))),
          Move(List(NormalCard(QUEEN, Heart))),
          Move(List(NormalCard(ACE, Diamond))),
          Move(List(NormalCard(ACE, Heart))),
          Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Heart))),
          Move(List(SpecialCard(TWO, Spade))),
          Move(List(Joker)),
        ))
        val gameState = Move(List.empty)
        val result = GameEngine.getNextMoveWrapper(validMoves, gameState)(playerInd)
        assert(result.contains(Move(List(NormalCard(SEVEN, Diamond)))))
        assert(result.get.cards match {
          case List(NormalCard(_,_), _*) => true
          case _ => false
        })
      }
    }

  }

  describe("tests for getNormalCardMoveHeuristic()") {

    describe("Throws exception when") {

      it("Move involves a joker") {
        assertThrows[IllegalHeuristicFunctionException](GameEngine.applyNormalCardMoveHeuristic(Move(List(Joker)), Move(List.empty)))
      }

      describe("When move involves a 2") {
        it("is a single 2") {
          assertThrows[IllegalHeuristicFunctionException](GameEngine.
            applyNormalCardMoveHeuristic(Move(List(SpecialCard(TWO, Diamond))), Move(List.empty)))
        }
        it("is a double 2") {
          assertThrows[IllegalHeuristicFunctionException](GameEngine.
            applyNormalCardMoveHeuristic(Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Heart))),
              Move(List.empty)))
        }
        it("is a triple 2") {
          assertThrows[IllegalHeuristicFunctionException](GameEngine.
            applyNormalCardMoveHeuristic(Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Heart), SpecialCard(TWO, Spade))),
              Move(List.empty)))
        }
      }

    }

    describe("When gameState is Empty") {
      val emptyGameState = Move(List.empty)

      describe("When validMove is a single 4")  {
        it("should return value") {
          assert(GameEngine.applyNormalCardMoveHeuristic(Move(List(NormalCard(FOUR,Heart))), emptyGameState) == 0.25)
        }
      }

      describe("When validMove is double4s")  {
        it("should return value") {
          assert(GameEngine.applyNormalCardMoveHeuristic(
            Move(List(NormalCard(FOUR,Heart), NormalCard(FOUR,Spade))),
            emptyGameState) == 0.305)
        }
      }

      describe("When validMove is triple4s")  {
        it("should return value") {
          assert(GameEngine.applyNormalCardMoveHeuristic(
            Move(List(NormalCard(FOUR,Club), NormalCard(FOUR,Heart), NormalCard(FOUR,Spade))),
            emptyGameState) == 0.36)
        }
      }

      describe("When validMove is quad4s")  {
        it("should return value") {
          assert(GameEngine.applyNormalCardMoveHeuristic(
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
          assert(GameEngine.applyNormalCardMoveHeuristic(validMove, gameState, PlayerIndicators(hand)) === 0.835)
        }
      }

      describe("When gameState is a double") {
        val hand = Hand(List(NormalCard(NINE, Club), NormalCard(NINE, Spade)))
        it("should return the right value") {
          val gameState = Move(List(NormalCard(FIVE, Club), NormalCard(FIVE, Spade)))
          val validMove = Move(List(NormalCard(NINE, Diamond), NormalCard(NINE, Spade)))
          assert(GameEngine.applyNormalCardMoveHeuristic(validMove, gameState, PlayerIndicators(hand)) === 0.835)
        }
      }

      describe("When gameState is a triple") {
        it("should return the right value") {
          val hand = Hand(List(NormalCard(NINE, Diamond), NormalCard(NINE, Club), NormalCard(NINE, Spade)))
          val gameState = Move(List(NormalCard(FIVE, Club), NormalCard(FIVE, Heart), NormalCard(FIVE, Spade)))
          val validMove = Move(List(NormalCard(NINE, Diamond), NormalCard(NINE, Club), NormalCard(NINE, Spade)))
          assert(GameEngine.applyNormalCardMoveHeuristic(validMove, gameState, PlayerIndicators(hand)) === 0.835)
        }
      }

      describe("When gameState is a quadruple") {
        val hand = Hand(List(NormalCard(NINE, Diamond), NormalCard(NINE, Club), NormalCard(NINE, Heart), NormalCard(NINE, Spade)))
        it("should return the right value") {
          val gameState = Move(List(NormalCard(FIVE, Diamond), NormalCard(FIVE, Club),
            NormalCard(FIVE, Heart), NormalCard(FIVE, Spade)))
          val validMove = Move(List(NormalCard(NINE, Diamond), NormalCard(NINE, Club),
            NormalCard(NINE, Heart), NormalCard(NINE, Spade)))
          assert(GameEngine.applyNormalCardMoveHeuristic(validMove, gameState, PlayerIndicators(hand)) === 0.835)
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
        assertThrows[IllegalHeuristicFunctionException](GameEngine.applySpecialCardMoveHeuristic(Move(List(NormalCard(EIGHT, Heart))), Move(List.empty)))
      }

      it("Move involves a list of NormalCard"){
        assertThrows[IllegalHeuristicFunctionException](GameEngine.applySpecialCardMoveHeuristic(
          Move(List(NormalCard(NINE, Club), NormalCard(NINE, Heart), NormalCard(NINE, Spade))), Move(List.empty)))
      }
    }

    describe("When validMove comprises of a Joker") {

      val validMove = Move(List(Joker))

      describe("When gameState is empty") {
        it("Should return playerModifier.specialCardModifier or 0") {
          val result = GameEngine.applySpecialCardMoveHeuristic(validMove, Move(List.empty), playerIndicators)
          assert(result == playerIndicators.specialCardModifier || result == 0)
        }
      }

      describe("When gameState is a single or a double") {
        it("Should return playerModifier.specialCardModifier or 0") {
          val result = GameEngine.applySpecialCardMoveHeuristic(validMove, Move(List(NormalCard(ACE, Heart))), playerIndicators)
          val result2 = GameEngine.applySpecialCardMoveHeuristic(validMove, Move(List(NormalCard(ACE, Heart), NormalCard(ACE, Spade))), playerIndicators)
          assert(result == playerIndicators.specialCardModifier || result == 0)
          assert(result2 == playerIndicators.specialCardModifier || result2 == 0)
        }
      }

      describe("When gameState is a triple or higher") {
        it("Should return playerModifier.specialCardModifier or 0") {
          val result = GameEngine.applySpecialCardMoveHeuristic(validMove,
            Move(List(NormalCard(ACE, Club), NormalCard(ACE, Heart), NormalCard(ACE, Spade))), playerIndicators)
          val result2 = GameEngine.applySpecialCardMoveHeuristic(validMove,
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
            val result = GameEngine.applySpecialCardMoveHeuristic(single2, Move(List.empty), playerIndicators)
            assert(result == playerIndicators.specialCardModifier || result == 0)
          }
        }

        describe("When a single 2 is played on top of a single NormalCard") {
          it("Should return playerModifier.specialCardModifier or 0") {
            val result = GameEngine.applySpecialCardMoveHeuristic(single2, Move(List(NormalCard(ACE, Diamond))), playerIndicators)
            assert(result == playerIndicators.specialCardModifier || result == 0)
          }
        }

        describe("When a single 2 is played on top of a double NormalCard"){
          it("Should return playerModifier.specialCardModifier or 0") {
            val result = GameEngine.applySpecialCardMoveHeuristic(single2,
              Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Spade))), playerIndicators)
            assert(result == playerIndicators.specialCardModifier || result == 0)
          }
        }

        describe("When a single 2 is played on top of another single 2") {
          describe("When the 2 being played is one-suit-away from the 2 on top") {
            it("Should return playerModifier.specialCardModifier") {
              val result = GameEngine.applySpecialCardMoveHeuristic(single2, Move(List(SpecialCard(TWO, Club))), playerIndicators)
              assert(result == playerIndicators.specialCardModifier)
            }
          }

          describe("When the 2 being played is not one-suit-away from the 2 on top") {
            it("Should return playerModifier.specialCardModifier or 0") {
              val result = GameEngine.applySpecialCardMoveHeuristic(single2, Move(List(SpecialCard(TWO, Diamond))), playerIndicators)
              assert(result == playerIndicators.specialCardModifier || result == 0)
            }
          }
        }

      }

      describe("When validMove consists of multiple 2s") {
        describe("When two 2s are being played") {
          it("Should return playerModifier.specialCardModifier or 0") {
            val result = GameEngine.applySpecialCardMoveHeuristic(double2,
              Move(List(NormalCard(EIGHT, Diamond), NormalCard(EIGHT, Club), NormalCard(EIGHT, Heart))), playerIndicators)
            assert(result == playerIndicators.specialCardModifier || result == 0)
          }
        }

        describe("When three 2s are being played") {
          it("Should return playerModifier.specialCardModifier or 0") {
            val result = GameEngine.applySpecialCardMoveHeuristic(triple2,
              Move(List(NormalCard(EIGHT, Diamond), NormalCard(EIGHT, Club), NormalCard(EIGHT, Heart), NormalCard(EIGHT, Spade))), playerIndicators)
            assert(result == playerIndicators.specialCardModifier || result == 0)
          }
        }

        describe("When two 2s are being played on top of existing two 2s"){
          it("should not care about off-by-one suit-burn preferences since it is still an expensive move and return 0/modifier value") {
            val result = GameEngine.applySpecialCardMoveHeuristic(double2,
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

    describe("When the moveFaceValue and the gameStateFaceValue are the same (suit-burn") {
      it("Should return infinity") {
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(single7, Move(List(NormalCard(SEVEN, Club))), 1).isInfinite)
        assert(GameEngine.applyNormalCardHeuristicWithPenaltyForBreakingSets(single7,
          Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club))), 1).isInfinite)
      }
    }

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
}
