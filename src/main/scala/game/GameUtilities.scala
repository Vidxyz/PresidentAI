package game

import player.{Player, PlayerIndicators}
import utils.Consants
import utils.Consants._

import scala.annotation.tailrec
import scala.util.Random

case object GameUtilities {

  def generatePlayersAndDealHands(listOfNames: List[String], seed: Int = 0): List[Player] = {
    val hands: List[Hand] = dealHands(listOfNames.size, seed)
   (hands zip listOfNames)
              .map(tuple => Player(tuple._2, tuple._1))
  }

  def dealHands(numberOfPlayers: Int, seed: Int = 0): List[Hand] = {
    val random = if(seed > 0) new Random(seed) else new Random()
    @tailrec
    def dealHandsHelper(currentPlayer: Int, playerHands: List[Hand], seenSoFar: List[Int]): List[Hand] = {
      if(seenSoFar.size == Consants.totalNumberOfCards) return playerHands

      val nextCardNum = random.nextInt(Consants.totalNumberOfCards)

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


  @Deprecated
  /*
  Deals a new hand by randomly selecting non-repeating numbers in the range [0, 54)
  and assigning them in a round robin format to each of the players.
  Return - game.Hand comprising of cards dealt to player1, discards all other "dealt cards"
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
  Similar cards include cards with same game.FaceValue but differing game.Suit
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
  Generate all possible combinations of moves that can be played from give intermediate list
  Restriction :-
   - Jokers can only be played one-a-turn
   - However since it is a burn you can play next joker again immediately
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
  def getValidMoves(allMoves: Moves, gameState: Move): Moves = {
    if (gameState.cards.isEmpty) allMoves
    else Moves(allMoves
                  .moves
                  .filter(move => isValidMove(move, gameState)))
  }

  def isValidMove(move: Move, gameState: Move): Boolean = {
    if(move.cards.isEmpty) return false
    if(gameState.cards.isEmpty) return true
    if(gameState.highestCard == Joker) return false
    if(move.highestCard == Joker) return true

    // Need max(1, n-1) 2s to be played when state.size = n
    if(move.moveFaceValue == 2) {
      gameState.moveFaceValue match {
          // If gameState is a two, then you need to play same number of twos on top
          case 2 => return move.parity == gameState.parity && checkIfBetter(move, gameState)
          case _ => gameState.parity match {
                    case 1 => return move.parity == 1
                    case 2 => return move.parity == 1
                    case other => return move.parity == other - 1
                  }
      }

    }

    if(move.parity != gameState.parity) false

    // This only happens when the game.Move in question doesn't involve 2s/JOKERs and is of the same numberOfCards
    else checkIfBetter(move, gameState)
  }

  // Returns true if move1 is "better" than move2 :- Higher value in numberToCardMap
  // Assumptions :-
  // 1. move1 and move2 are not EMPTY moves
  // 2. move1 and move2 have same parity - same size of cards
  //    - only exception to the above is 2s and JOKERs
  def checkIfBetter(move1: Move, move2: Move): Boolean =
    cardOrderValue(move1.highestCard) > cardOrderValue(move2.highestCard)

  //TODO -  write tests for this
  def cardOrderValue(card: Card): Int = numberToCardMap.find(_._2 == card).map(_._1).getOrElse(-1)

  //TODO -  write tests for this
  def isOnlySpecialMovesAvailable(validMoves: Moves): Boolean = {
    validMoves.moves.foldLeft(true)((acc, move) => move.cards match {
      case List(SpecialCard(_, _), _*) => acc
      case List(Joker,_*) => acc
      case _ => false
    })
  }

  /*
  Returns the next game state having processed the nextMove
  Returns game.Move(List.empty) if it is a suit burn or a joker, ie a valid move with same moveFaceValue
  Returns gameState otherwise
  Assumption - nextMove is a validMove - this includes the (n-1) restriction for 2s as a special card
   */
  def getNextGameState(gameState: Move, nextValidMove: Option[Move]): Move = {
    nextValidMove.getOrElse(None) match {
      case move: Move =>
        if (move.moveFaceValue == -1) Move(List.empty) // game.Joker
        else if(move.moveFaceValue != gameState.moveFaceValue) move // Higher card, or 2, replaces gameState
        else Move(List.empty) // game.Suit Burn, since it is a valid move and faceValues are the same as gameState
      case None => gameState
    }
  }

}