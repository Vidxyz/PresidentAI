import game.FaceValue._
import game.Suits._
import game.{GameUtilities, Hand, Joker, Move, Moves, NormalCard, SpecialCard}
import org.scalatest.FunSpec
import player.PlayerIndicators

class DevelopmentTest extends FunSpec{

  // TEST FOR EXPERIMENT
  // TODO - delete this once done. Currently, a 2Diamond is favored over a 4Spade, this is WRONG and needs changing
  describe("These are experimental tests") {
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
      println(single8D +  " : " + GameUtilities.getNormalCardMoveHeuristic(single8D, gameState, playerIndicators).toString)
      println(doubleQueen +  " : " + GameUtilities.getNormalCardMoveHeuristic(doubleQueen, gameState, playerIndicators).toString)
      println(single2D +  " : " + GameUtilities.getSpecialCardMoveHeuristic(single2D, gameState, playerIndicators).toString)
      println(single2C +  " : " + GameUtilities.getSpecialCardMoveHeuristic(single2C, gameState, playerIndicators).toString)
      println(single2H +  " : " + GameUtilities.getSpecialCardMoveHeuristic(single2H, gameState, playerIndicators).toString)
      println(joker +  " : " + GameUtilities.getSpecialCardMoveHeuristic(joker, gameState, playerIndicators).toString)

      val validMoves = Moves(List(single8D, doubleQueen, single2D, single2C, single2H, two2s, three2s, joker))
      println("Is only special moves available " + GameUtilities.isOnlySpecialMovesAvailable(validMoves) )
      println("Next move is " + GameUtilities.getNextMoveWrapper(validMoves, gameState)(playerIndicators))

      //      println(game.GameUtilities.getNextMoveV2(game.Moves(List(validMove1, validMove12, validMove3)), gameState))

    }
  }
  // TODO - delete above when done

}
