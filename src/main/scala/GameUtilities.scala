import Consants._
import FaceValue.TWO

import scala.annotation.tailrec
import scala.util.Random

case class Round(gameState: Move,
                 lastMovePlayedBy: String,
                 totalNumberOfPlayers: Int,
                 currentPlayerTurn: Int,
                 listOfPlayers: List[Player],
                 roundPassStatus: List[Boolean]) {

  /*
  Return TRUE iff everyone has passed, except for the person who played the last move
  Returns FALSE otherwise
   */
  def hasEveryonePassed: Boolean = {
    (listOfPlayers zip roundPassStatus)
      .filter(tuple => tuple match {
        case (player, _passStatus) => !(player.name == lastMovePlayedBy)
      })
      .foldLeft(true)((acc, tuple1) => acc && tuple1._2)
  }

  def checkIfLastMovePlayedBy(name: String): Boolean = name == lastMovePlayedBy

  def playerFinishedTheirTurnOnABurn: Boolean =
    !this.listOfPlayers.map(player => player.name).contains(this.lastMovePlayedBy) && this.lastMovePlayedBy != ""

  /*
  Since name is unique, this should only return a list of size 1
   */
  def hasAlreadySkippedTurn(name: String): Boolean = {
    (listOfPlayers zip roundPassStatus)
      .filter(tuple => tuple match {
        case (player, _passStatus) => player.name == name
      })
      .map {
        case (_, status) =>
          status
      }
      .head
  }
}

object Round {
  def getNoPassList(numberOfPlayers: Int): List[Boolean] = {
    (1 to numberOfPlayers).toList.map(_ => false)
  }
}

case object GameUtilities {

  def generatePlayersAndDealHands(listOfNames: List[String]): List[Player] = {
    val hands: List[Hand] = dealHands(listOfNames.size)
   (hands zip listOfNames)
              .map(tuple => Player(tuple._2, tuple._1))
  }

  def dealHands(numberOfPlayers: Int): List[Hand] = {
    @tailrec
    def dealHandsHelper(currentPlayer: Int, playerHands: List[Hand], seenSoFar: List[Int]): List[Hand] = {
      if(seenSoFar.size == Consants.totalNumberOfCards) return playerHands

      val nextCardNum = Random.nextInt(Consants.totalNumberOfCards)

      if(seenSoFar.contains(nextCardNum)) dealHandsHelper(currentPlayer, playerHands, seenSoFar)
      else {
        dealHandsHelper(
          if(currentPlayer + 1 == numberOfPlayers) 0 else currentPlayer + 1,
          playerHands
            .zipWithIndex
            .map(tuple =>
              if(tuple._2 == currentPlayer) tuple._1.copy(tuple._1.listOfCards ++ numberToCardMap.get(nextCardNum))
              else tuple._1),
          seenSoFar :+ nextCardNum
        )
      }

    }

    val emptyPlayerHands: List[Hand] = 1.to(numberOfPlayers).toList.map(x => Hand(List.empty))
    dealHandsHelper(0, emptyPlayerHands, List.empty)

  }


  /*
  Deals a new hand by randomly selecting non-repeating numbers in the range [0, 54)
  and assigning them in a round robin format to each of the players.
  // --- This is currently not being used -----
  Return - Hand comprising of cards dealt to player1, discards all other "dealt cards"
  */
  def dealNewHand(numberOfPlayers: Int, totalNormalCards: Int): Hand = {
    @tailrec
    def dealNewHandHelper(currentPlayer: Int, seenSoFar: List[Int], dealtSoFar: List[Card]): List[Card] = {
      if(seenSoFar.size == totalNormalCards) return dealtSoFar

      val nextCardNum = Random.nextInt(totalNormalCards)

      if(seenSoFar.contains(nextCardNum))
        dealNewHandHelper(currentPlayer, seenSoFar, dealtSoFar)
      else {
        dealNewHandHelper(
          if(currentPlayer == numberOfPlayers) 1
          else currentPlayer + 1,
          seenSoFar :+ nextCardNum,
          if (currentPlayer == 1) dealtSoFar ++ numberToCardMap.get(nextCardNum)
          else dealtSoFar
        )
      }
    }

    Hand(dealNewHandHelper(1, List.empty[Int], List.empty[Card]))
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
  Assumption :- hand is sorted
   */
  def getListsOfSimilarCards(hand: Hand): List[List[Card]] = {
    @tailrec
    def getListsOfSimilarCardsHelper(lastCardSeen: Option[Card], startIndex: Int,
                                        endIndex: Int, listSoFar: List[List[Card]]):
                                      List[List[Card]] = {
      if (endIndex == hand.listOfCards.size)
        listSoFar :+ List.empty ++ hand.listOfCards.drop(startIndex)
      else {
        lastCardSeen.getOrElse(None) match {
          case card: Card  =>
            // Still on the same value, increment endIndex
            if(card.value == hand.listOfCards(endIndex).value)
              getListsOfSimilarCardsHelper(Some(hand.listOfCards(endIndex)), startIndex, endIndex + 1, listSoFar)
            // Values are different. Slice from [start, end),
            else
              getListsOfSimilarCardsHelper(Some(hand.listOfCards(endIndex)), endIndex, endIndex + 1,
                listSoFar :+ List.empty ++ hand.listOfCards.slice(startIndex, endIndex))

          case None => getListsOfSimilarCardsHelper(Some(hand.listOfCards(endIndex)), startIndex, endIndex + 1, listSoFar)
        }

      }
    }
    if(hand.listOfCards.isEmpty) List.empty
    else if(hand.listOfCards.size == 1) List(hand.listOfCards)
    else getListsOfSimilarCardsHelper(None, 0, 0, List.empty)
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

  def isValidMove(move: Move, gameState: Move): Boolean = {
    if(move.cards.isEmpty) return false

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
  def checkIfBetter(move1: Move, move2: Move): Boolean =
    numberToCardMap.find(_._2 == move1.highestCard).map(_._1).getOrElse(-1) >
    numberToCardMap.find(_._2 == move2.highestCard).map(_._1).getOrElse(-1)

  /*
  Gets a 0-1 value signifying how desirable a move is compared to given game state
   */
  def getHeuristicValue(move: Move, gameState: Move): Float = {
    move.cards match {
      case List(Joker, _*) => 0
      case List(NormalCard(TWO, _), _*) => 0
      case _ => (0.5f * (1f/(move.moveFaceValue - gameState.moveFaceValue))) + (0.5f * move.cards.size/Consants.maxMoveSize)
    }
  }

  /*
  Fetches the next best move possible, from list of valid moves, given current game state and current hand
  Applies heuristic value on each move, and picks the best
  Returns Empty move is there are no valid moves to choose from
   */
  def getNextMove(validMoves: Moves, gameState: Move): Option[Move] = {
      try {
        Some(
          validMoves.moves(validMoves.moves
            .map(m => getHeuristicValue(m, gameState))
            .zipWithIndex
            .maxBy(_._1)
            ._2))
      } catch {
        case e: UnsupportedOperationException => None
      }
    }


  /*
  Returns the next game state having processed the nextMove
  Returns Move(List.empty) if it is a suit burn or a joker, ie a valid move with same moveFaceValue
  Returns gameState otherwise
  Assumption - nextMove is a validMove
   */
  def getNextGameState(gameState: Move, nextValidMove: Option[Move]): Move = {
    nextValidMove.getOrElse(None) match {
      case move: Move =>
        if (move.moveFaceValue == 2) Move(List.empty) // 2s
        else if (move.moveFaceValue == -1) Move(List.empty) // Joker
        else if(move.moveFaceValue > gameState.moveFaceValue) move
        else Move(List.empty) // Suit Burn
      case None => gameState
    }
  }

}