package game

import game.FaceValue._
import game.Suits._
import player.{Player, PlayerIndicators}
import utils.Consants
import utils.Consants._

import scala.annotation.tailrec
import scala.util.Random

case object GameUtilities {

  private val wildcardMatcher = """3\([0-9].+\)""".r

  implicit class Crossable[X](xs: List[List[Card]]) {
    def cross(ys: List[List[Card]]): List[List[Card]] = for { x <- xs; y <- ys } yield x ++ y
  }

  def getFaceValueFromString(value: String): Value = {
    value  match {
      case "2" => TWO
      case wildcardMatcher(_*) => THREE
      case "4" => FOUR
      case "5" => FIVE
      case "6" => SIX
      case "7" => SEVEN
      case "8" => EIGHT
      case "9" => NINE
      case "10" => TEN
      case "J" => JACK
      case "Q" => QUEEN
      case "K" => KING
      case "A" => ACE
      case s => throw IllegalMoveSuppliedException("Bad FaceValue: " + s)
    }
  }

  def getCardFromMoveStrings(value: String, suit: String): Card = {
    if(value match { case "Joker" => true; case _ => false}) return Joker

    val faceValue = getFaceValueFromString(value)

    val assumedValue: Int = faceValue match {
      case THREE => value.drop(1).drop(1).dropRight(1).toInt
      case _ => -1
    }

    val moveSuit = suit.toLowerCase match {
      case "diamond" => Diamond
      case "club" => Club
      case "heart" => Heart
      case "spade" => Spade
    }

    if(faceValue == TWO) SpecialCard(TWO, moveSuit)
    else if(faceValue == THREE) WildCard(THREE, moveSuit, assumedValue)
    else NormalCard(faceValue, moveSuit)
  }

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
                                          .subsets
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

    // This only happens when the Move in question doesn't involve 2s/JOKERs
    // AND is of the same parity
    // This implies one of three things
    // 1. Move is comprised of all NormalCard
    // 2. Move is comprised of NormalCard(s) + WildCard(s)
    // 3. Move is comprised of all WildCard(s)
    else checkIfBetter(move, gameState)
  }

  // Returns true if move1 is "better" than move2 :- Higher value in numberToCardMap
  // Assumptions :-
  // 1. move1 and move2 are not EMPTY moves
  // 2. move1 and move2 have same parity - same size of cards
  //    - only exception to the above is 2s and JOKERs
  def checkIfBetter(move1: Move, move2: Move): Boolean = {
    // This is the case in which 3s are involved, leading to assumedCard being same as original NormalCard
    if (cardOrderValue(move1.highestCard) == cardOrderValue(move2.highestCard)) {
      move2.highestCard match {
        case w: WildCard => true
        case _ => false
      }
    }
    else cardOrderValue(move1.highestCard) > cardOrderValue(move2.highestCard)
  }


  def cardOrderValue(card: Card): Int = {
    card match {
      case w: WildCard => numberToCardMap.find(_._2 == getCardAssumedByWildCard(w)).map(_._1).getOrElse(-1)
      case _ => numberToCardMap.find(_._2 == card).map(_._1).getOrElse(-1)
    }
  }

  /*
  Returns the NormalCard assumed by a wildcard, given its assumed value
  Defaulting to a FOUR for now, look over this later. Theoretically, this shouldn't happen ever.
  */

  def getCardAssumedByWildCard(w: WildCard): NormalCard = NormalCard(numberToFaceValueMap.getOrElse(w.assumedValue, FOUR), w.suit)

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

  /*
  Returns a list of validMoves containing only NormalCard moves
   */
  def filterNonSpecialCardMoves(validMoves: Moves): Moves = {
    Moves(validMoves.moves.filter(m => m.cards match {
      case List(NormalCard(_,_), _*) => true
      case List(WildCard(_,_,_), _*) => true
      case _ => false
    }))
  }

  /*
  Applies all possible combinations of threes to allMoves and returns them
  Assumes inbound moves do not contain any WildCards
   */
  def addThreesToMoves(allMoves: Moves, listOfThrees: List[Card]): Moves = {
    if(listOfThrees.isEmpty) allMoves
    else {
      val allPossibleCombinationsOfThrees = listOfThrees.
        toSet
        .subsets
        .toList
        .map(set => set.toList)

      // Apply 3s only on NormalMoves, cannot apply with special cards
      val listOfCardsInNormalMoves: List[List[Card]] = allMoves.moves.filter(move => move match {
        case Move(List(NormalCard(_,_), _*), _) => true
        case Move(List(WildCard(_,_,_), _*), _) => throw IllegalMoveSuppliedException("This method does not accept WildCard as part of allMoves")
        case _ => false
      }).map(move => move.cards)
      val listOfSpecialMoves = allMoves.moves.filter(move => move match {
        case Move(List(Joker), _) => true
        case Move(List(SpecialCard(_,_), _*), _) => true
        case _ => false
      })

      val totalMoves: List[Move] = (allPossibleCombinationsOfThrees cross listOfCardsInNormalMoves).map(list => Move(list)) ++
        allPossibleCombinationsOfThrees.filter(l => l.nonEmpty).map(listOfCard => Move(listOfCard)) ++
        listOfSpecialMoves

      // Making 3s assume values of the set they're a part of, or ACE for now
      // Cards get reassigned values later if they can be used to burn
      Moves(totalMoves
        .map(move => move.cards)
          .map(listOfCard =>
            if(listOfCard.forall(card => card.intValue != 3)) listOfCard
            else {
              if(listOfCard.forall(card => card match {
                case w: WildCard => true
                case e => false })) {
                listOfCard.map {
                  case w: WildCard => w.copy(assumedValue = NormalCard(ACE, Spade).intValue)
                  case e => e
                }
              }
              else listOfCard.map {
                case w: WildCard => w.copy(assumedValue = listOfCard.last.intValue)
                case c => c
              }
            }
          )
          .map(listOfCards => Move(listOfCards)))
    }
  }

  /*
  Returns a List[Card] containing all WildCards from supplied list
  Return List.empty if no wildcards
   */
  def getWildCardListFromIntermediateList(intermediateList: List[List[Card]]): List[Card] = {
    try {
      intermediateList.filter(list => list.head.intValue == 3).head
    } catch {
      case _: Exception => List.empty
    }
  }

  def getNumberOfWildCardsInMove(validMove: Move): Int =
    validMove.cards.foldLeft(0)((total, card) => card match {case c: WildCard => total + 1; case _ => total})

  /*
  Returns the new hand comprising of cards from currentHand that do not appear in movePlayed
   */
  def getNewHand(currentHand: Hand, movePlayed: Option[Move]): Hand = {
    movePlayed.getOrElse(None) match {
      case move: Move => Hand(
        currentHand
          .listOfCards
          .filter(c => c match {
            case w: WildCard =>
              if(move.cards.exists(mc => mc match {
                case mwc: WildCard => mwc.suit == w.suit
                case _ => false
              })) false
              else true
            case _ => !move.cards.contains(c)
          }))
      case None => currentHand
    }
  }

  /*
  Takes in a list of valid moves and assigns dangling wildcards optimal assumedValues
  A Dangling Wilcards is defined as a 3 or a set of 3s being played by themself, instead of with a NormalCard
  For example, upon entry into this function, a Move such as <3> or <3-3> would be assumed to be ACEs by default
  However, this isnt optimal, and it only suffices for checking validity of move
  Ideally, the <3> or <3-3> would assume value based on gameState
  This comes in one of three scenarios :-
  1. It maintains its assumedValue (highest possible faceValue of card)
  2. It's assumedValue changes to that of gameState.moveFaceValue, if doing so burns the game state
  3. It's assumedValue changes to that of gameState.moveFaceValue + 1, if gameState is not an ACE
   */
  def assignWildCardsOptimally(validMoves: Moves, gameState: Move): Moves = {
    Moves(validMoves.moves
      .map(move => if(GameUtilities.getNumberOfWildCardsInMove(move) == move.parity)
                      getMoveWithOptimalWildCardValue(move, gameState)
                    else move))
  }

  /*
  Assumption - the move is comprised entirely of 3s
  It is also a valid move given the gameState
  Example :- 3(A)-3(A) on top of 9-9
   */
  def getMoveWithOptimalWildCardValue(validMove: Move, gameState: Move): Move = {
    if(GameUtilities.getNumberOfWildCardsInMove(validMove) != validMove.parity) throw IllegalMoveSuppliedException("Function only accept moves comprised completely of WildCards")
    else if (gameState.parity == 0) validMove  /* Return highest possible assumed value if gameState is empty */
    else {
      validMove.highestCard match {
        case w: WildCard =>
          if(cardOrderValue(w.copy(assumedValue=gameState.moveFaceValue)) > cardOrderValue(gameState.highestCard)) {
            Move(validMove.cards.map(card => card match {
              case w: WildCard => w.copy(assumedValue=gameState.moveFaceValue)
              case c: Card => c
            }))
          }
          else validMove
        case _ => validMove
      }
    }
  }

  case class IllegalMoveSuppliedException(s: String) extends IllegalArgumentException(s)
}