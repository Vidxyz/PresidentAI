package game

import player.PlayerIndicators
import utils.Consants

import scala.util.Random

case object GameEngine {
  /*
 Gets a 0-1 value signifying how desirable a move is compared to given game state
 Assumption :- validMove is a valid move given the current gameState
 TODO -  update unit tests
  */
  def getNormalCardMoveHeuristic(validMove: Move, gameState: Move,
                                 playerIndicators: PlayerIndicators = PlayerIndicators(Hand(List.empty))): Double = {
    val randomValue = Random.nextDouble()
    validMove.cards match {
      case List(NormalCard(_,_), _*) =>
        if(gameState.isEmpty) applyNormalCardHeuristicWithMoveSizeModifier(validMove, gameState)
        else applyNormalCardHeuristicWithPenaltyForBreakingSets(validMove, gameState, playerIndicators.getListSetSizeForCard(validMove))
      case _ => throw IllegalHeuristicFunctionException("Incorrect heuristic supplied to evaluate normal card")
    }
  }

  /*
  Readily plays a 2 if it is ONE away from the 2 in question in the gameState. Otherwise, uses probability function
  Prioritizes playing jokers on triples/quads over using probability function
  TODO - write unit tests for this too
   */
  def getSpecialCardMoveHeuristic(validMove: Move, gameState: Move, playerIndicators: PlayerIndicators): Double = {
    val randomValue = Random.nextDouble()
    validMove match {
      case Move(List(NormalCard(_,_), _*)) => throw IllegalHeuristicFunctionException("Incorrect heuristic supplied to evaluate special card")
      case Move(List(Joker, _*)) =>  if (gameState.isEmpty || gameState.parity < 3 ) {
        /* We don't care about playing jokers any differently if its a double/single */
        if (randomValue < playerIndicators.specialCardModifier) playerIndicators.specialCardModifier else 0
      }
      else{
        /* Modifying probability of playing a joker according to :- modifier^(2/(parity-1))
        This is to incentivize playing jokers for triples/quads
        */
        if (randomValue < scala.math.pow(playerIndicators.specialCardModifier, (2/(gameState.parity - 1)))) playerIndicators.specialCardModifier
        else 0
      }
      case validSpecialMove =>
        // Meaning that a single 2 is being played
        if(validSpecialMove.parity == 1) {
          // Meaning that the special card is gonna be played on top of (a) game.NormalCard(s)
          if(gameState.isEmpty || gameState.highestCard.intValue > 2) {
            if (randomValue < playerIndicators.specialCardModifier) playerIndicators.specialCardModifier else 0
          }
          // Meaning that the special card is gonna be played on top of a 2
          else {
            // Prioritizing off-by-one 2-suit burns
            if(GameUtilities.cardOrderValue(validSpecialMove.highestCard) - GameUtilities.cardOrderValue(gameState.highestCard) == 1) playerIndicators.specialCardModifier
            else if(randomValue < playerIndicators.specialCardModifier) playerIndicators.specialCardModifier else 0
          }
        }
        // Meaning that multiple 2s are being played
        else {
          // This is being done to de-incentivize playing multiple 2s at once, since that is a pretty expensive move
          if (randomValue < scala.math.pow(playerIndicators.specialCardModifier, validSpecialMove.parity)) playerIndicators.specialCardModifier
          else 0
        }
    }
  }

  @Deprecated
  def applyNormalCardHeuristic(validMove: Move, gameState: Move): Double = 1d/(validMove.moveFaceValue - gameState.moveFaceValue)

  // TODO - add tests
  def applyNormalCardHeuristicWithMoveSizeModifier(validMove: Move, gameState: Move): Double = {
    (0.78d * (1d/(validMove.moveFaceValue - gameState.moveFaceValue)) + (0.22d * validMove.parity/Consants.maxMoveSize))
  }

  // TODO - add tests for this
  def applyNormalCardHeuristicWithPenaltyForBreakingSets(validMove: Move, gameState: Move, maxCards: Int): Double = {
    (0.22d * (1d/(validMove.moveFaceValue - gameState.moveFaceValue))
      + (0.78d * 1/(maxCards - validMove.parity + 1)))
  }

  /*
  Fetches the next best move possible, from list of valid moves, given current game state and current hand
  Applies heuristic value on each move, and picks the best
  Returns Empty move is there are no valid moves to choose from
  NOTE:-
  1. validMoves will comprise of moves with Special Cards (2s, Jokers) IFF no game.NormalCard moves are available
    1.1 Even then, there isnt a guarantee that the special card will be chosen
   */
  def getNextMove(validMoves: Moves, gameState: Move)(heuristic: (Move, Move, PlayerIndicators) => Double, playerIndicators: PlayerIndicators): Option[Move] = {
    try {
      Some(
        validMoves.moves(validMoves.moves
          .map(m => heuristic(m, gameState, playerIndicators))
          .filter(value => value > 0)
          .zipWithIndex
          .maxBy(_._1)
          ._2))
    } catch {
      case _: Exception => None
    }
  }

  // TODO - unit tests
  def getNextMoveWrapper(validMoves: Moves, gameState: Move)(implicit playerIndicators: PlayerIndicators): Option[Move] = {
    // If normal moves are available, play them first!
    if(!GameUtilities.isOnlySpecialMovesAvailable(validMoves)) {
      val filteredValidMoves = Moves(validMoves.moves.filter(m => m.cards match {
        case List(NormalCard(_,_), _*) => true
        case _ => false
      }))
      getNextMove(filteredValidMoves, gameState)(getNormalCardMoveHeuristic, playerIndicators)
    } else {
      // If comprising ONLY of special moves, do nothing
      getNextMove(validMoves, gameState)(getSpecialCardMoveHeuristic, playerIndicators)
    }
  }

  case class IllegalHeuristicFunctionException(s: String) extends IllegalStateException(s)

}