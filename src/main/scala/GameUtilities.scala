import Consants.numberToCardMap
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

    def dealNextCard(currentPlayer: Int, seenSoFar: List[Int], dealtSoFar: List[Card] ): List[Card] = {
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
  def sortCards(listOfCards: List[Card]): Hand = {
    Hand(listOfCards.sortWith(
      (card1, card2) =>
        numberToCardMap.find(_._2 == card1).map(_._1).getOrElse(-1) <
          numberToCardMap.find(_._2 == card2).map(_._1).getOrElse(-1)
    ))
  }

  /*
  Makes clumped sets out of a sorted list
  WARNING - thereis a bug here
   */
  def getListOfIntermediateSets(listOfCards: List[Card]): List[List[Card]] = {
    @tailrec
    def getListOfIntermediateSetsHelper(lastCardSeen: Card, startIndex: Int,
                                        endIndex: Int, listSoFar: List[List[Card]]): List[List[Card]] = {
      if (endIndex + 1 == listOfCards.size)
        listSoFar :+ List.empty ++ listOfCards.drop(startIndex)
      else {
        if(lastCardSeen.value == listOfCards(endIndex).value)
          getListOfIntermediateSetsHelper(listOfCards(endIndex), startIndex, endIndex + 1, listSoFar)
        else
          getListOfIntermediateSetsHelper(listOfCards(endIndex), endIndex, endIndex + 1,
            listSoFar :+ List.empty ++ listOfCards.slice(startIndex, endIndex))
      }
    }
    getListOfIntermediateSetsHelper(listOfCards.head, 0, 1, List.empty)
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
                                        .map(e => List(e))
                                        .map(l => Move(l))
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
    Moves(allMoves.moves
                  .filter(move => isValidMove(move, state)))
  }

  private def isValidMove(move: Move, state: Move): Boolean = {
    if(move.cards.last == Joker) return true

    // Need max(1, n-1) 2s to be played when state.size = n
    if(move.cards.head.value == "TWO") {
      if(state.cards.size - move.cards.size == 1) return true
      else return false
    }

    if(move.cards.size != state.cards.size) false
    // Else check value comparison
    else {
      if (numberToCardMap.find(_._2 == move.cards.last).map(_._1).getOrElse(-1) >
        numberToCardMap.find(_._2 == state.cards.last).map(_._1).getOrElse(-1)) true
      else false
    }
  }

  /*
  Fetches the next best move possible, given current game state and current hand
   */
  def getNextMove(validMoves: Moves, state: Move): Move = {
    ???
  }

  /*
  Returns a new hand, having played the next best possible move
   */
  def playNextMove(currentHand: Hand): Hand = {
    ???
    /*
    Get intermediate sets of cards
    Get all possible moves
    Get valid moves

     */
  }



}