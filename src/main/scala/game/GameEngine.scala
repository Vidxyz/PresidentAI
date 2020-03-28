package game

import game.FaceValue.THREE
import game.Suits.Diamond
import player.PlayerIndicators
import utils.Consants

import scala.util.Random

case object GameEngine {
  /*
 Gets a 0-1 value signifying how desirable a move is compared to given game state
 Assumption :- validMove is a valid move given the current gameState
  */
  def applyNormalCardMoveHeuristic(validMove: Move, gameState: Move,
                                   playerIndicators: PlayerIndicators = PlayerIndicators(Hand(List.empty))): Move = {
    validMove.cards match {
      case List(NormalCard(_,_), _*) | List(WildCard(_,_,_), _*) =>
        if(gameState.isEmpty)
          validMove.copy(likelihood = scala.math.max(0d,
            applyNormalCardHeuristicWithMoveSizeModifier(validMove) - wildCardUsagePenalty(validMove, playerIndicators.wildCardPenaltyModifier)))
        else validMove.copy(likelihood = scala.math.max(0d, applyNormalCardHeuristicWithPenaltyForBreakingSets(validMove, gameState,
          playerIndicators.getListSetSizeForCard(validMove)) - wildCardUsagePenalty(validMove, playerIndicators.wildCardPenaltyModifier)))
      case _ => throw IllegalHeuristicFunctionException("Incorrect heuristic supplied to evaluate special card")
    }
  }

  /*
  Readily plays a 2 if it is ONE away from the 2 in question in the gameState. Otherwise, uses probability function
  Prioritizes playing jokers on triples/quads over using probability function
  TODO - Clean up code by using match with guards instead of nested if-else
   */
  def applySpecialCardMoveHeuristic(validMove: Move, gameState: Move,
                                    playerIndicators: PlayerIndicators = PlayerIndicators(Hand(List.empty))): Move = {
    val randomValue = Random.nextDouble()
    validMove match {
      case Move(List(NormalCard(_,_), _*), _) => throw IllegalHeuristicFunctionException("Incorrect heuristic supplied to evaluate normal card")
      case Move(List(Joker, _*), _) =>  if (gameState.isEmpty || gameState.parity < 3 ) {
        /* We don't care about playing jokers any differently if its a double/single */
        val l = if (randomValue < playerIndicators.specialCardModifier) playerIndicators.specialCardModifier else 0
        validMove.copy(likelihood = l)
      }
      else{
        /* Modifying probability of playing a joker according to :- modifier^(2/(parity-1))
        This is to incentivize playing jokers for triples/quads
        */
        val l = if (randomValue < applyJokerModifierFunction(playerIndicators.specialCardModifier, gameState.parity)) playerIndicators.specialCardModifier else 0
        validMove.copy(likelihood = l)
      }
      case validSpecialMove =>
        // Meaning that a single 2 is being played
        if(validSpecialMove.parity == 1) {
          // Meaning that the special card is gonna be played on top of (a) game.NormalCard(s)
          if(gameState.isEmpty || gameState.highestCard.intValue > 2) {
            val l = if (randomValue < playerIndicators.specialCardModifier) playerIndicators.specialCardModifier else 0
            validMove.copy(likelihood = l)
          }
          // Meaning that the special card is gonna be played on top of a 2
          else {
            // Prioritizing off-by-one 2-suit burns
            val l = {
              if(GameUtilities.cardOrderValue(validSpecialMove.highestCard) - GameUtilities.cardOrderValue(gameState.highestCard) == 1) playerIndicators.specialCardModifier
              else if(randomValue < playerIndicators.specialCardModifier) playerIndicators.specialCardModifier
              else 0
            }
            validMove.copy(likelihood = l)
          }
        }
        // Meaning that multiple 2s are being played
        else {
          // This is being done to de-incentivize playing multiple 2s at once, since that is a pretty expensive move
          val l = if (randomValue < applyMultipleTwoModifierFunction(playerIndicators.specialCardModifier, validSpecialMove.parity)) playerIndicators.specialCardModifier else 0
          validSpecialMove.copy(likelihood = l)
        }
    }
  }

  @Deprecated
  def applyNormalCardHeuristic(validMove: Move, gameState: Move): Double = 1d/(validMove.moveFaceValue - gameState.moveFaceValue)

  /*
  Penalizes the usage of WildCards. Subtracted from heuristic value obtained based on move
  Greatly Favours higher assumedValue
  Slightly Favours higher parity
  Does not favour multiple 3s
   */
  def wildCardUsagePenalty(validMove: Move, wildCardPenaltyModifier: Double): Double = {
    if(validMove.cards.forall(card => card match {case n: NormalCard => true; case _ => false})) 0
    else
      wildCardPenaltyModifier *
        ((0.35 * (1/(validMove.moveFaceValue - WildCard(THREE, Diamond).intValue)))
         + (0.2 * (1/validMove.parity))
         + (0.45 * 1 / (Consants.maxMoveSize - GameUtilities.getNumberOfWildCardsInMove(validMove))))
  }

  /*
  Assumes that gameState is empty. If non-empty, use the heuristic function below this instead
  Assumes validMove comprises only of NormalCards
  Using base as 3-intValue here because it is the lowest possible to play
   */
  def applyNormalCardHeuristicWithMoveSizeModifier(validMove: Move): Double = {
    (0.78d * (1d/(validMove.moveFaceValue)) + (0.22d * validMove.parity/Consants.maxMoveSize))
  }

  /*
  Penalizing the breaking of sets to play this move by giving a 0.78 weighting to holding on to sets
   */
  def applyNormalCardHeuristicWithPenaltyForBreakingSets(validMove: Move, gameState: Move, maxCards: Int): Double = {
    ((0.22d * (1d/(validMove.moveFaceValue - gameState.moveFaceValue + 1)))
      + (0.78d * 1/(maxCards - validMove.numberOfNormalcards + 1)))
  }

  /*
  Modifying probability of playing a joker according to :- modifier^(2/(parity-1))
  This is to incentivize playing jokers for triples/quads
  */
  def applyJokerModifierFunction(specialCardModifier: Double, gameStateParity: Int): Double = scala.math.pow(specialCardModifier, (2/(gameStateParity - 1)))

  /*
  Method to de-incentivize playing multiple 2s at once, since it is an expensive move
  Based on the formula :- modifier^(validMoveParity)
   */
  def applyMultipleTwoModifierFunction(specialCardModifier: Double, validMoveParity: Int): Double = scala.math.pow(specialCardModifier, validMoveParity)

  /*
  Fetches the next best move possible, from list of valid moves, given current game state and current hand
  Applies heuristic value on each move, and picks the best
  Returns Empty move is there are no valid moves to choose from
  NOTE:-
  1. validMoves will comprise of moves with Special Cards (2s, Jokers) IFF no game.NormalCard moves are available
    1.1 Even then, there isnt a guarantee that the special card will be chosen
   */
  def getNextMove(validMoves: Moves, gameState: Move)(heuristic: (Move, Move, PlayerIndicators) => Move,
                                                      playerIndicators: PlayerIndicators): Option[Move] = {
    try {
      Some(validMoves.moves
          .map(m => heuristic(m, gameState, playerIndicators))
          .map(m => { println(m); m})
          .filter(validMoves => validMoves.likelihood > 0)
          .maxBy(_.likelihood))
    } catch {
      case e: Exception =>
//        e.printStackTrace()
        None
    }
  }

  // TODO - It was noticed during testing that A,A was favored over a single 8, when the hand was 8, Q, A, A - this isnt the most desirable
  def getNextMoveWrapper(validMoves: Moves, gameState: Move)(implicit playerIndicators: PlayerIndicators): Option[Move] = {
    // If normal (non-special, including 3s) moves are available, play them first!
    if(!GameUtilities.isOnlySpecialMovesAvailable(validMoves)) {
      val filteredNonSpecialValidMoves = GameUtilities.filterNonSpecialCardMoves(validMoves)
      getNextMove(filteredNonSpecialValidMoves, gameState)(applyNormalCardMoveHeuristic, playerIndicators)
    } else {
      // If comprising ONLY of special moves, do nothing
      getNextMove(validMoves, gameState)(applySpecialCardMoveHeuristic, playerIndicators)
    }
  }

  case class IllegalHeuristicFunctionException(s: String) extends IllegalStateException(s)

}
