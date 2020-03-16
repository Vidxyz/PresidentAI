import Consants._
import FaceValue.TWO

import scala.annotation.tailrec
import scala.util.Random

case object GameUtilities {

  /*
  Deals a new hand by randomly selecting non-repeating numbers in the range [0, 54)
  and assigning them in a round robin format to each of the players.

  Return - Hand comprising of cards dealt to player1
  */
  def dealNewHand(numberOfPlayers: Int, totalNormalCards: Int): Hand = {
    @tailrec
    def dealNextCard(currentPlayer: Int, seenSoFar: List[Int], dealtSoFar: List[Card]): List[Card] = {
      if(seenSoFar.size == totalNormalCards) return dealtSoFar

      val nextCardNum = Random.nextInt(totalNormalCards)

      if(seenSoFar.contains(nextCardNum))
        dealNextCard(currentPlayer, seenSoFar, dealtSoFar)
      else {
        dealNextCard(
          if(currentPlayer == numberOfPlayers) 1
          else currentPlayer + 1,
          seenSoFar :+ nextCardNum,
          if (currentPlayer == 1) dealtSoFar ++ numberToCardMap.get(nextCardNum)
          else dealtSoFar
        )
      }
    }

    Hand(dealNextCard(1, List.empty[Int], List.empty[Card]))
  }

  /*
  Sort cards according to their (faceValue, suit)
  Sorting logic is as follows :-
  Diamonds < Clubs < Hearts < Spades
  3 < 4 < 5 < ..... < K < A < 2 < JOKER
  3_Diamonds < 3_Clubs < 3_Hears < 3_Spades
   */
  def sortCards(listOfCards: List[Card]): List[Card] = {
    listOfCards.sortWith(
      (card1, card2) =>
        numberToCardMap.find(_._2 == card1).map(_._1).getOrElse(-1) <
          numberToCardMap.find(_._2 == card2).map(_._1).getOrElse(-1)
    )
  }

  /*
  Generate Lists of Similar cards
  Similar cards include cards with same FaceValue but differing Suit
   */
  def getListsOfSimilarCards(hand: Hand): List[List[Card]] = {
    @tailrec
    def getListsOfSimilarCardsHelper(lastCardSeen: Card, startIndex: Int,
                                        endIndex: Int, listSoFar: List[List[Card]]): List[List[Card]] = {
      if (endIndex + 1 == hand.listOfCards.size)
        listSoFar :+ List.empty ++ hand.listOfCards.drop(startIndex)
      else {
        if(lastCardSeen.value == hand.listOfCards(endIndex).value)
          getListsOfSimilarCardsHelper(hand.listOfCards(endIndex), startIndex, endIndex + 1, listSoFar)
        else
          getListsOfSimilarCardsHelper(hand.listOfCards(endIndex), endIndex, endIndex + 1,
            listSoFar :+ List.empty ++ hand.listOfCards.slice(startIndex, endIndex))
      }
    }
    getListsOfSimilarCardsHelper(hand.listOfCards.head, 0, 1, List.empty)
  }

  /*
  Parse current listOfCards to make a set of valid moves
  Shortcoming - JOKERs will also be paired up into 1s/2s - Gotta make this work out
   */
  def getAllMoves(intermediateSetsOfCards: List[List[Card]]): Moves = {
    @tailrec
    def createListOfMoves(currentSetIndex: Int, movesSoFar: List[Move]): List[Move] = {
      if (currentSetIndex == intermediateSetsOfCards.size) return movesSoFar

      if (intermediateSetsOfCards(currentSetIndex).head == Joker) {
        val splitUpJokers = intermediateSetsOfCards(currentSetIndex)
                                        .map(e => Move(List(e)))
        createListOfMoves(currentSetIndex + 1, movesSoFar ++ splitUpJokers)
      }
      else {
        val allCombinations: List[Move] = intermediateSetsOfCards(currentSetIndex)
                                          .toSet
                                          .subsets()
                                          .toList
                                          .filter(e => e.nonEmpty)
                                          .map(set => Move(set.toList))
        createListOfMoves(currentSetIndex + 1, movesSoFar ++ allCombinations)
      }
    }
    Moves(createListOfMoves(0, List.empty))
  }

  /*
  Gets valid moves from ListOfAllMoves
  Current limitations :-
  1. No special logic for 3s
   */
  def getValidMoves(allMoves: Moves, state: Move): Moves = {
    if (state.begin) allMoves
    else Moves(allMoves
                  .moves
                  .filter(move => isValidMove(move, state)))
  }

  private def isValidMove(move: Move, gameState: Move): Boolean = {
    if(move.highestCard == Joker) return true

    // Need max(1, n-1) 2s to be played when state.size = n
    if(move.moveFaceValue == 2) {
      gameState.moveFaceValue match {
        case 2 => return move.numberOfCards == gameState.numberOfCards &&
                          checkIfBetter(move, gameState)
        case _ => gameState.numberOfCards match {
                    case 1 => return move.numberOfCards == 1
                    case 2 => return move.numberOfCards == 1
                    case other => return move.numberOfCards == other - 1
                  }
      }

    }

    if(move.numberOfCards != gameState.numberOfCards) false

    // This only happens when the Move in question doesn't involve 2s/JOKERs and is of the same numberOfCards
    else checkIfBetter(move, gameState)
  }

  // Returns true if move1 is "better" than move2 :- Higher value in numberToCardMap
  private def checkIfBetter(move1: Move, move2: Move) =
    numberToCardMap.find(_._2 == move1.highestCard).map(_._1).getOrElse(-1) >
    numberToCardMap.find(_._2 == move2.highestCard).map(_._1).getOrElse(-1)

  /*
  Gets a 0-1 value signifying how desirable a move is compared to given game state
   */
  def getHeuristicValue(move: Move, gameState: Move): Float = {
    move.cards match {
      case List(Joker, _*) => 0
      case List(NormalCard(TWO, _), _*) => 0
      case _ => 1f/(move.moveFaceValue - gameState.moveFaceValue)
    }
  }

  /*
  Fetches the next best move possible, given current game state and current hand
  Applies heuristic value on each move, and picks the best
   */
  def getNextMove(validMoves: Moves, gameState: Move): Move = {
    val bestMoveIndex = validMoves.moves
      .map(m => getHeuristicValue(m, gameState))
      .zipWithIndex
      .maxBy(_._1)
      ._2

    validMoves.moves(bestMoveIndex)
  }

  /*
  Returns the next game state having processed the nextMove
  Returns Move(List.empty) if it is a suit burn or a joker, ie a valid move with same moveFaceValue
  Returns gameState otherwise
  Assumption - nextMove is a validMove
   */
  def getNextGameState(gameState: Move, nextMove: Move): Move = {
    if(nextMove.moveFaceValue > gameState.moveFaceValue) nextMove
    else Move(List.empty)
  }

}