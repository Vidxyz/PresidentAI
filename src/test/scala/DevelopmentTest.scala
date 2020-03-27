import game.FaceValue._
import game.Suits._
import game._
import org.scalatest.FunSpec
import player.PlayerIndicators

class DevelopmentTest extends FunSpec {

  // TODO - delete when done
  describe("*********************************") {
    it("**** EXPERIMENTAL TEST TO VALIDATE BEHAVIOUR *****") {
      val hand = Hand(List(
        NormalCard(SIX, Club),
        NormalCard(SEVEN, Club),
        NormalCard(SEVEN, Heart),
        NormalCard(SEVEN, Spade),
        NormalCard(EIGHT, Heart),
        NormalCard(NINE, Diamond),
        NormalCard(NINE, Heart),
        NormalCard(TEN, Heart),
        NormalCard(JACK, Club),
        NormalCard(QUEEN, Diamond),
        NormalCard(QUEEN, Spade),
        NormalCard(ACE, Heart)
      ))
      val hand2 = Hand(List(
        NormalCard(EIGHT, Diamond),
        NormalCard(QUEEN, Diamond),
        NormalCard(QUEEN, Club),
        SpecialCard(TWO, Diamond),
        SpecialCard(TWO, Club),
        SpecialCard(TWO, Heart),
        Joker
      ))
      val playerIndicators = PlayerIndicators(hand2)
      //      val validMove1 = game.Move(List(game.SpecialCard(TWO, Diamond)))
      //      val validMove12 = game.Move(List(game.SpecialCard(TWO, Diamond), game.SpecialCard(TWO, Club)))
      val validMove2 = Move(List(NormalCard(FOUR, Spade)))
      val validMove3 = Move(List(NormalCard(ACE, Club), NormalCard(ACE, Spade)))
      val validMove4 = Move(List(NormalCard(QUEEN, Club), NormalCard(QUEEN, Heart), NormalCard(QUEEN, Spade)))
      val validMove5 = Move(List(NormalCard(SIX, Club), NormalCard(SIX, Heart), NormalCard(SIX, Spade)))
      val gameState = Move(List.empty)
      //      val gameState = game.Move(List(game.NormalCard(SIX, Spade)))
      val triple5s = Move(List(NormalCard(FIVE, Diamond), NormalCard(FIVE, Club), NormalCard(FIVE, Heart)))
      val quad8s = Move(List(NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)))
      val single7 = Move(List(NormalCard(SEVEN, Club)))
      val single8 = Move(List(NormalCard(EIGHT, Heart)))
      val single9 = Move(List(NormalCard(NINE, Diamond)))
      val single10 = Move(List(NormalCard(TEN, Heart)))
      val singleJack = Move(List(NormalCard(JACK, Club)))

      val single8D = Move(List(NormalCard(EIGHT, Diamond)))
      val doubleQueen = Move(List(NormalCard(QUEEN, Diamond), NormalCard(QUEEN, Club)))
      val single2D = Move(List(SpecialCard(TWO, Diamond)))
      val single2C = Move(List(SpecialCard(TWO, Club)))
      val single2H = Move(List(SpecialCard(TWO, Heart)))
      val two2s = Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club)))
      val three2s = Move(List(SpecialCard(TWO, Diamond), SpecialCard(TWO, Club), SpecialCard(TWO, Spade)))
      val joker = Move(List(Joker))
      //      val validMoves = game.Moves(List(validMove1, validMove2, validMove3))
      //      val gameState = game.Move(List(game.NormalCard(THREE, Club), game.NormalCard(THREE, Spade)))
      //      println(game.GameUtilities.getNextMoveWrapper(validMoves, gameState)(player.PlayerIndicators(Main.stackedHand)))
      println("Current game state : " + gameState)
      //      println(validMove2 +  " : " + game.GameUtilities.getNormalCardMoveHeuristic(validMove2, gameState).toString)
      //      println(validMove3 +  " : " + game.GameUtilities.getNormalCardMoveHeuristic(validMove3, gameState).toString)
      //      println(validMove4 +  " : " + game.GameUtilities.getNormalCardMoveHeuristic(validMove4, gameState).toString)
      //      println(validMove5 +  " : " + game.GameUtilities.getNormalCardMoveHeuristic(validMove5, gameState).toString)
      //      println(triple5s +  " : " + game.GameUtilities.getNormalCardMoveHeuristic(triple5s, gameState).toString)
      //      println(quad8s +  " : " + game.GameUtilities.getNormalCardMoveHeuristic(quad8s, gameState).toString)
      //      println(single7 +  " : " + game.GameUtilities.getNormalCardMoveHeuristic(single7, gameState, playerIndicators).toString)
      //      println(single8 +  " : " + game.GameUtilities.getNormalCardMoveHeuristic(single8, gameState, playerIndicators).toString)
      //      println(single9 +  " : " + game.GameUtilities.getNormalCardMoveHeuristic(single9, gameState, playerIndicators).toString)
      //      println(single10 +  " : " + game.GameUtilities.getNormalCardMoveHeuristic(single10, gameState, playerIndicators).toString)
      //      println(singleJack +  " : " + game.GameUtilities.getNormalCardMoveHeuristic(singleJack, gameState, playerIndicators).toString)
      println(single8D +  " : " + GameEngine.applyNormalCardMoveHeuristic(single8D, gameState, playerIndicators).toString)
      println(doubleQueen +  " : " + GameEngine.applyNormalCardMoveHeuristic(doubleQueen, gameState, playerIndicators).toString)
      println(single2D +  " : " + GameEngine.applySpecialCardMoveHeuristic(single2D, gameState, playerIndicators).toString)
      println(single2C +  " : " + GameEngine.applySpecialCardMoveHeuristic(single2C, gameState, playerIndicators).toString)
      println(single2H +  " : " + GameEngine.applySpecialCardMoveHeuristic(single2H, gameState, playerIndicators).toString)
      println(joker +  " : " + GameEngine.applySpecialCardMoveHeuristic(joker, gameState, playerIndicators).toString)

      val validMoves = Moves(List(single8D, doubleQueen, single2D, single2C, single2H, two2s, three2s, joker))
      println("Is only special moves available " + GameUtilities.isOnlySpecialMovesAvailable(validMoves) )
      println("Next move is " + GameEngine.getNextMoveWrapper(validMoves, gameState)(playerIndicators))

      //      println(game.GameUtilities.getNextMoveV2(game.Moves(List(validMove1, validMove12, validMove3)), gameState))

    }

    it("is used for developing wildcard"){

      val allMoves = Moves(List(
        Move(List(NormalCard(SEVEN, Spade))),
        Move(List(NormalCard(EIGHT, Heart), NormalCard(EIGHT, Spade))),
        Move(List(NormalCard(NINE, Club), NormalCard(NINE, Heart), NormalCard(NINE, Spade))),
        Move(List(NormalCard(TEN, Diamond), NormalCard(TEN, Club), NormalCard(TEN, Heart), NormalCard(TEN, Spade))),
        Move(List(SpecialCard(TWO, Heart))),
        Move(List(Joker))
      ))

      val threes = List(WildCard(THREE, Diamond), WildCard(THREE, Spade))
      val allMovesWithThrees = GameUtilities.addThreesToMoves(allMoves, threes)

      println(allMovesWithThrees)

      println(GameUtilities.getValidMoves(allMovesWithThrees,
        Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Club)))))


      val testMove = Move(List(WildCard(THREE, Diamond, 7), WildCard(THREE, Club, 7), WildCard(THREE, Heart, 7), WildCard(THREE, Spade, 7),
        NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)
      ))

      val testMove2 = Move(List(
        NormalCard(FOUR, Diamond), NormalCard(FOUR, Club), NormalCard(FOUR, Heart), NormalCard(FOUR, Spade)
      ))

      val threeMove = Move(List(WildCard(THREE, Spade, 14)))

      println("--------------------------")
      println(testMove.highestCard)
      println(testMove.cards.forall(card => card match {case n: NormalCard => true; case _ => false}))


      println(GameUtilities.getNumberOfWildCardsInMove(testMove))
      println("--------------------------")

      println(GameEngine.applyNormalCardHeuristicWithMoveSizeModifier(testMove2))
      println(GameEngine.applyNormalCardHeuristicWithMoveSizeModifier(threeMove))
//      println(GameEngine.wildCardUsagePenalty(threeMove))


      println("--------------------------")


    }

    it("Observer game error secnario where 3-K was preferred over 10-10 for gamestate 9-9") {

      val testMove = Move(List(WildCard(THREE, Diamond, 7), WildCard(THREE, Club, 7), WildCard(THREE, Heart, 7), WildCard(THREE, Spade, 7),
        NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade)
      ))
      //<TEN,Diamond>, <TEN,Heart>
      /*
      List(<THREE,Heart(0)>, <SEVEN,Club>, <EIGHT,Club>, <EIGHT,Heart>, <TEN,Diamond>, <TEN,Heart>, <KING,Heart>)
        List(<ACE,Diamond>, <TWO,Club>)
       */
      val observedHand = Hand(List(
        WildCard(THREE, Heart), NormalCard(SEVEN, Club), NormalCard(EIGHT, Club), NormalCard(EIGHT, Heart),
        NormalCard(TEN, Diamond), NormalCard(TEN, Heart), NormalCard(KING, Heart), NormalCard(ACE, Diamond),
        SpecialCard(TWO, Club)
      ))
      val double10s = Move(List(NormalCard(TEN, Diamond), NormalCard(TEN, Heart)))
      val otherMove = Move(List(WildCard(THREE, Heart, 13), NormalCard(KING, Heart)))
      val gs = Move(List(NormalCard(NINE, Heart), NormalCard(NINE, Spade)))

      println(GameEngine.applyNormalCardMoveHeuristic(double10s, gs, PlayerIndicators(observedHand)))
      println(GameEngine.applyNormalCardMoveHeuristic(otherMove, gs, PlayerIndicators(observedHand)))

      val single3 = Move(List(WildCard(THREE, Diamond, 14)))

      println(testMove.numberOfNormalcards)

      val aHand = Hand(List(WildCard(THREE, Diamond), SpecialCard(TWO, Diamond)))

      println(GameEngine.applyNormalCardMoveHeuristic(single3, Move(List.empty), PlayerIndicators(aHand)))
      println(GameEngine.applyNormalCardHeuristicWithMoveSizeModifier(single3))
//      println(GameEngine.wildCardUsagePenalty(single3))

      val move1 = Move(List(WildCard(THREE, Spade, 7), NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart)))
      val move2 = Move(List(WildCard(THREE, Diamond, 7), WildCard(THREE, Club, 7), WildCard(THREE, Heart, 7), NormalCard(SEVEN, Spade)))
      //checkIfBetter
      println(move1.highestCard)
      println(move2.highestCard)
      println(GameUtilities.checkIfBetter(move1, move2))
      print(GameUtilities.getNextGameState(move1, Some(move2)))



    }

    it("is a new test") {
      val m = Move(List(WildCard(THREE, Diamond), WildCard(THREE, Spade, 8), NormalCard(SEVEN, Club)))
      println(m.highestCard)

      //checkIfBetter
      val m1 = Move(List(WildCard(THREE, Diamond, 7), NormalCard(SEVEN, Club)))
      val m2 = Move(List(WildCard(THREE, Club, 14), WildCard(THREE, Spade, 7)))

      println(GameUtilities.checkIfBetter(m1, m2))


      val gs = Move(List(NormalCard(SIX, Spade)))
      val pHand = Hand(List(
        WildCard(THREE, Diamond),
        NormalCard(EIGHT, Diamond), NormalCard(EIGHT, Heart),NormalCard(EIGHT, Spade),
        NormalCard(KING, Diamond),NormalCard(KING, Spade),
        SpecialCard(TWO, Club)
      ))

      // Here, K-diamond is preferred over playing the 3 - this is WRONG!
//      val m1 =

      // TODO - Make suitBurns not INFINITY- but a real high value

    }

  }

}
