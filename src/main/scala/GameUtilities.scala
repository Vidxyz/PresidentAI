import Consants._
import FaceValue.TWO

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

case class Round(gameState: Move,
                 lastMovePlayedBy: String,
                 totalNumberOfPlayers: Int,
                 currentPlayerTurn: Int,
                 listOfPlayers: List[Player],
                 roundPassStatus: List[Boolean]) {

  /*
  Return TRUE if everyone has passed, except for the person who played the last move
  Returns FALSE otherwise
   */
  def hasEveryoneExceptThePlayerWhoPlayedTheLastMovePassed: Boolean = {
    (listOfPlayers zip roundPassStatus)
      .filter(tuple => tuple match {
        case (player, _passStatus) => !(player.name == lastMovePlayedBy)
      })
      .foldLeft(true)((acc, tuple1) => acc && tuple1._2)
  }

  def checkIfLastMovePlayedBy(name: String): Boolean = name == lastMovePlayedBy

  def playerEndedTheGameOnABurn: Boolean =
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

  /*
 Since name is unique, this should only return a list of size 1
  */
  def getIndexOf(name: String): Int = {
    listOfPlayers
      .zipWithIndex
      .filter {
        case (player, _) => player.name == name
      }
      .map {
        case (_, index) => index
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

    // This only happens when the Move in question doesn't involve 2s/JOKERs and is of the same numberOfCards
    else checkIfBetter(move, gameState)
  }

  // Returns true if move1 is "better" than move2 :- Higher value in numberToCardMap
  // Assumptions :-
  // 1. move1 and move2 are not EMPTY moves
  // 2. move1 and move2 have same parity - same size of cards
  //    - only exception to the above is 2s and JOKERs
  def checkIfBetter(move1: Move, move2: Move): Boolean =
    numberToCardMap.find(_._2 == move1.highestCard).map(_._1).getOrElse(-1) >
    numberToCardMap.find(_._2 == move2.highestCard).map(_._1).getOrElse(-1)

  /*
  Gets a 0-1 value signifying how desirable a move is compared to given game state
  Assumption :- validMove is a valid move given the current gameState
  TODO - penalize  breaking of sets, and prioritize suit burns  if available
  I am more inclined to play higher cards when I have more special cards to fall back on
  I am more inclined to play higher cards when I see that my opponents are closer to finishing
   */
  def getNormalCardMoveHeuristic(validMove: Move, gameState: Move, highCardModifier: Double = 0): Double = {
    val randomValue = Random.nextDouble()
    validMove.cards match {
      case List(NormalCard(_,_), _*) =>
        if(validMove.cards.head.isFaceCard) {
          if(randomValue < highCardModifier) applyNormalCardHeuristic(validMove, gameState)
          else 0
        }
        else applyNormalCardHeuristic(validMove, gameState)
      case _ => throw IllegalHeuristicFunctionException("Incorrect heuristic supplied to evaluate normal card")
    }
  }

  // TODO - this can be specificly weighted towards the various suits of 2
  def getSpecialCardMoveHeuristic(validMove: Move, gameState: Move, specialCardModifier: Double = 0): Double = {
    val randomValue = Random.nextDouble()
    validMove.cards match {
      case List(NormalCard(_,_), _*) => throw IllegalHeuristicFunctionException("Incorrect heuristic supplied to evaluate special card")
      case _ => if (randomValue < specialCardModifier) specialCardModifier else 0
    }
  }

  def applyNormalCardHeuristic(validMove: Move, gameState: Move): Double = {
    (0.78d * (1d/(validMove.moveFaceValue - gameState.moveFaceValue)) + (0.22d * validMove.parity/Consants.maxMoveSize))
  }

  /*
  Fetches the next best move possible, from list of valid moves, given current game state and current hand
  Applies heuristic value on each move, and picks the best
  Returns Empty move is there are no valid moves to choose from
  NOTE:-
  1. validMoves will comprise of moves with Special Cards (2s, Jokers) IFF no NormalCard moves are available
    1.1 Even then, there isnt a guarantee that the special card will be chosen
   */
  def getNextMove(validMoves: Moves, gameState: Move)(heuristic: (Move, Move, Double) => Double, modifier: Double): Option[Move] = {
    try {
      Some(
        validMoves.moves(validMoves.moves
          .map(m => heuristic(m, gameState, modifier))
          .filter(value => value > 0)
          .zipWithIndex
          .maxBy(_._1)
          ._2))
    } catch {
      case _: Exception => None
    }
  }

  def getNextMoveWrapper(validMoves: Moves, gameState: Move)(implicit playerIndicators: PlayerIndicators): Option[Move] = {
    // Check if validMoves comprises ONLY of special cards
    val isOnlySpecialMovesAvailable: Boolean =
      validMoves.moves.foldLeft(true)((acc, move) => move.cards match {
      case List(SpecialCard(_, _), _*) => acc
      case List(Joker,_*) => acc
      case _ => false
    })

    // If normal moves are available, play them first!
    if(!isOnlySpecialMovesAvailable) {
      val filteredValidMoves = Moves(validMoves.moves.filter(m => m.cards match {
        case List(NormalCard(_,_), _*) => true
        case _ => false
      }))
      getNextMove(filteredValidMoves, gameState)(getNormalCardMoveHeuristic, playerIndicators.highCardModifier)
    } else {
      // If comprising ONLY of special moves, do nothing
      getNextMove(validMoves, gameState)(getSpecialCardMoveHeuristic, playerIndicators.specialCardModifier)
    }
  }



  /*
  Returns the next game state having processed the nextMove
  Returns Move(List.empty) if it is a suit burn or a joker, ie a valid move with same moveFaceValue
  Returns gameState otherwise
  Assumption - nextMove is a validMove - this includes the (n-1) restriction for 2s as a special card
   */
  def getNextGameState(gameState: Move, nextValidMove: Option[Move]): Move = {
    nextValidMove.getOrElse(None) match {
      case move: Move =>
        if (move.moveFaceValue == -1) Move(List.empty) // Joker
        else if(move.moveFaceValue != gameState.moveFaceValue) move // Higher card, or 2, replaces gameState
        else Move(List.empty) // Suit Burn, since it is a valid move and faceValues are the same as gameState
      case None => gameState
    }
  }



  case class IllegalHeuristicFunctionException(s: String) extends IllegalStateException(s)
}