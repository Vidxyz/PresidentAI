import Consants.numberToCardMap

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
      // Base case
      if(seenSoFar.size == totalNormalCards) return dealtSoFar

      val nextCardNum = Random.nextInt(totalNormalCards)

      // Repeat if encounter same number
      if(seenSoFar.contains(nextCardNum)) dealNextCard(currentPlayer, seenSoFar, dealtSoFar)
      // Else increment player and continue
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
      if (endIndex + 1 == listOfCards.size) listSoFar :+ List.empty ++ listOfCards.drop(startIndex)
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

  // Parse current listOfCards to make a set of valid moves
  def getAllMoves(intermediateMoves: List[List[Card]]): Moves = {
    @tailrec
    def createListOfMoves(currentSetIndex: Int, movesSoFar: List[Move]): List[Move] = {
      if (currentSetIndex == intermediateMoves.size) return movesSoFar
      val allCombinations: List[Move] =
        intermediateMoves(currentSetIndex).toSet.subsets().toList.filter(e => e.nonEmpty).map(set => Move(set.toList))
      createListOfMoves(currentSetIndex + 1, movesSoFar ++ allCombinations)
    }
    Moves(createListOfMoves(0, List.empty))
  }

  /*
  Gets valid moves from ListOfAllMoves
  Current limitations :-
  1. No special logic for 2s
  2. No special logic for 3s
  3. No special logic for JOKERS
   */
  def getValidMoves(allMoves: Moves, state: Move): Moves = {
    Moves(allMoves.moves.filter(move => isValidMove(move, state)))
  }

  private def isValidMove(move: Move, state: Move): Boolean = {
    if(move.cards.size != state.cards.size) false
    // Else check value comparison
    else {
      if (numberToCardMap.find(_._2 == move.cards.last).map(_._1).getOrElse(-1) >
        numberToCardMap.find(_._2 == state.cards.last).map(_._1).getOrElse(-1)) true
      else false
    }
  }
}