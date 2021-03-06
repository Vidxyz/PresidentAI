package game

import game.FaceValue._
import game.Suits._
import player.Player
import utils.Constants
import utils.Constants._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

case object GameUtilities {

  private val wildcardMatcher = """3\([0-9]+\)""".r

  implicit class Crossable[X](xs: List[List[Card]]) {
    def cross(ys: List[List[Card]]): List[List[Card]] = for { x <- xs; y <- ys } yield x ++ y
  }

  def getCardFromCardStrings(value: String, suit: String): Card = {
    value.toLowerCase match {
      case "black_joker" => return BlackJoker
      case "red_joker" =>  return RedJoker
      case _ => /* continue */
    }

    val faceValue = value.toLowerCase  match {
      case "2" => TWO
      case wildcardMatcher(_*) => THREE
      case "4" => FOUR
      case "5" => FIVE
      case "6" => SIX
      case "7" => SEVEN
      case "8" => EIGHT
      case "9" => NINE
      case "10" => TEN
      case "j" => JACK
      case "q" => QUEEN
      case "k" => KING
      case "a" => ACE
      case s => throw IllegalMoveSuppliedException("Bad FaceValue: " + s)
    }

    val assumedValue: Int = faceValue match {
      case THREE => value.drop(1).drop(1).dropRight(1).toInt
      case _ => -1
    }

    val moveSuit = suit.toLowerCase match {
      case "diamond" => Diamond
      case "club" => Club
      case "heart" => Heart
      case "spade" => Spade
      case s => throw IllegalMoveSuppliedException("Bad Suit: " + s)
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
      if(seenSoFar.size == Constants.totalNumberOfCards) return playerHands

      val nextCardNum = random.nextInt(Constants.totalNumberOfCards)

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
  Sorts cards to give away as defined by cardGiveAwayPreference
  Expects only non-normal cards - throws exception otherwise
   */
  def sortCardsInPreferenceOrderOfGivingAwayBestCards(listOfNonNormalCards: List[Card]): List[Card] = {
    listOfNonNormalCards.foreach({ case c: NormalCard => throw IllegalCardSuppliedException("Error: Expected non-normal cards only"); case _ => })
    listOfNonNormalCards.sortWith(
      (card1, card2) => cardGiveAwayPreference.getOrElse(card1, -1) < cardGiveAwayPreference.getOrElse(card2, -1)
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
      if (currentSetIndex == intermediateSetsOfCards.size) movesSoFar
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

  /* Assumption : All cards in move are the same, except in the case of 3s being used as a WildCard */
  def isValidMove(move: Move, gameState: Move): Boolean = {
    // Base checks
    if(move.cards.isEmpty) return false
    if(gameState.cards.isEmpty) return true
    // Checks for Jokers
    if(gameState.highestCard == BlackJoker || gameState.highestCard == RedJoker) return false
    if(move.highestCard == BlackJoker || move.highestCard == RedJoker) return true

    // Checks for 2s
    // Need max(1, n-1) 2s to be played when state.size = n
    if(move.moveFaceValue == 2) {
      gameState.moveFaceValue match {
          // If gameState is a two, then you need to play same number of twos on top
          case 2 => return move.parity == gameState.parity && checkIfBetter(move, gameState)
          case _ => gameState.parity match {
                    case 1 => return move.parity == 1
                    case other => return move.parity == other - 1
                  }
      }
    }

    // Trivial Check
    if(move.parity != gameState.parity) false

    // This only happens when the Move in question doesn't involve 2s/JOKERs
    // AND is of the same parity
    // This implies one of three things
    // 1. Move is comprised of all NormalCard
    // 2. Move is comprised of NormalCard(s) + WildCard(s)
    // 3. Move is comprised of all WildCard(s)
    else checkIfBetter(move, gameState)
  }

  /*
  A legal move is defined as one of the following
  1. Comprised entirely of Jokers
  2. Comprised entirely of SpecialCards
  3. Comprised entirely of NormalCards
  4. Comprised entirely of WildCards
  5. Comprised of both WildCards and NormalCards
  6. For cases 3-5, all faceValues/assumedValues are the same
   */
  def isLegalMove(move: Move): Boolean = {
    if(move.cards.forall(card => card == BlackJoker)) true
    else if(move.cards.forall(card => card == RedJoker)) true
    else if(move.cards.forall(card => card match { case s:SpecialCard => true; case _ => false})) true
    else move.cards.forall(card => card match {
        case n:NormalCard => n.intValue == move.moveFaceValue
        case w:WildCard => w.assumedValue == move.moveFaceValue
        case _ => false
    })
  }

  // Returns true if move1 is "better" than move2 :- Higher value in numberToCardMap
  // Assumptions :-
  // 1. move1 and move2 are not EMPTY moves
  // 2. move1 and move2 have same parity - same size of cards - even in comparisons involving 2s
  //    - This is because we don't need to check if 2s are better than NormalCards, except for parity
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
      case List(BlackJoker,_*) => acc
      case List(RedJoker,_*) => acc
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
        if (move.moveFaceValue == -1) Move(List.empty) // Red_Joker or Black_Joker
        else if(move.moveFaceValue != gameState.moveFaceValue) move // Since it is a valid move, it has to be a higher card, or 2, replaces gameState
        else Move(List.empty) // Suit Burn, since it is a valid move and faceValues are the same as gameState
      case None => gameState
    }
  }

  /*
  Returns a list of validMoves containing only NormalCard/WildCard moves
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
        case Move(List(BlackJoker), _) => true
        case Move(List(RedJoker), _) => true
        case Move(List(SpecialCard(_,_), _*), _) => true
        case _ => false
      })

      val totalMoves: List[Move] = (allPossibleCombinationsOfThrees cross listOfCardsInNormalMoves).map(list => Move(list)) ++
        allPossibleCombinationsOfThrees.filter(l => l.nonEmpty).map(listOfCard => Move(listOfCard)) ++
        listOfSpecialMoves

      // Making 3s assume values of the set they're a part of, or ACE for now
      // Cards get reassigned values later if they can be used to burn
      // If not, they stick to the value of ACE - greedy algorithm
      Moves(totalMoves
        .map(move => move.cards)
          .map(listOfCard =>
            if(listOfCard.forall(card => card.intValue != 3)) listOfCard
            else {
              if(listOfCard.forall(card => card match {
                case w: WildCard => true
                case e => false })) {
                listOfCard.map {
                  case w: WildCard => w.copy(assumedValue = ACE_Spade.intValue)
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
      case move: Move => Hand(currentHand.listOfCards.filter(c => !move.cards.contains(c)))
      case None => currentHand
    }
  }

  /*
  Takes in a list of valid moves and assigns dangling wildcards optimal assumedValues
  A Dangling Wildcards is defined as a 3 or a set of 3s being played by themselves, instead of with a NormalCard
  For example, upon entry into this function, a Move such as <3> or <3-3> would be assumed to be ACEs by default
  However, this isn't optimal, and it only suffices for checking validity of move
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

  /* When a user chooses a move involving WildCards, they do not have an assumed value.
  * This method assigns values to moves comprising purely of WildCards by using getMoveWithOptimalWildCardValue
  * To do so, it first defaults such moves to a faceValue of ACE, and then re-assigns it if a burn is more appropriate
  * If the move involves WildCards AND NormalCards, the WildCards assume the faceValue of the LAST card in the move
  * This is because WildCards would inherently get sorted before any NormalCards, so there would be no question of assuming values
  * */
  def fixWildCardAssumedValueInMove(move: Move, gameState: Move): Move = {
    // Return move, if no wildcards are present in it
    if(!move.cards.exists(card => card match {case w:WildCard => true; case _ => false})) move
    else {
      if(move.cards.forall(card => card match {case w:WildCard => true; case _ => false})) {
        val modifiedMove = Move(move.cards.map({case w:WildCard => w.copy(assumedValue = ACE_Spade.intValue); case e => e}))
        getMoveWithOptimalWildCardValue(modifiedMove, gameState)
      }
      else
        Move(move.cards.map({case w:WildCard => w.copy(assumedValue = move.cards.last.intValue); case e => e}))
    }
  }

  /*
  Drops cards, and replaces them with cards, and returns a sorted hand
   */
  def dropAndReplaceCardsInHand(hand: Hand, cardsToDrop: List[Card], cardsToReplce: List[Card]): Hand = {
    Hand(GameUtilities.sortCards(hand.listOfCards.filter(card => !cardsToDrop.contains(card)) ++ cardsToReplce))
  }


  /*
  Exchanges hands with president-bum, vp-vb
  Neutral hand is untouched
  Assumes playerCompletionOrder.size == playerCompletionStatusOrder.size
  Assumes newPlayers.names == player names in completion order
  Returns a tuple comprising of newPlayers, and the cards that were received by the REAL player
   */
  def exchangeHands(newPlayers: mutable.Buffer[Player],
                    playerCompletionOrder: List[String],
                    playerCompletionStatuses: List[PlayerCompletionStatus],
                    userSelectedCardToGetRidOf: List[Card]): (mutable.Buffer[Player], List[Card]) = {
    val totalCardsToDrop = if(newPlayers.size >= 4) 2 else 1
    val droppedCards: mutable.Map[PlayerCompletionStatus, List[Card]] = collection.mutable.Map.empty
    val completionMap: Map[String, PlayerCompletionStatus] =  playerCompletionOrder.zip(playerCompletionStatuses).toMap

    newPlayers
      .map(p => (p, completionMap.getOrElse(p.name, Neutral)))
      .foreach({
        case (player, President) => if(player.isRealPlayer) droppedCards(President) = userSelectedCardToGetRidOf
                                    else droppedCards(President) = player.getWorstCards(totalCardsToDrop)
        case (player, VicePres) => if(player.isRealPlayer) droppedCards(VicePres) = userSelectedCardToGetRidOf
                                    else  droppedCards(VicePres) = player.getWorstCards(1)
        case (player, ViceBum) => droppedCards(ViceBum) = player.getBestCards(1)
        case (player, Bum) => droppedCards(Bum) = player.getBestCards(totalCardsToDrop)
        case (_, Neutral) =>
      })

    val realPlayerName = if(!newPlayers.exists(_.isRealPlayer)) "" else newPlayers.filter(_.isRealPlayer).head.name
    // This value is empty if neutral, or empty if only AI players in game
    val cardsReceivedByRealPlayer = droppedCards.getOrElse(nemesisMap.getOrElse(completionMap.getOrElse(realPlayerName, Neutral), Neutral), List.empty)

    (newPlayers
      .map(p => (p, completionMap.getOrElse(p.name, Neutral)))
      .map({
        case (player, President) => player.copy(hand = GameUtilities.dropAndReplaceCardsInHand(player.hand,
                                    droppedCards.getOrElse(President, List.empty), droppedCards.getOrElse(Bum, List.empty)))
        case (player, VicePres) => player.copy(hand = GameUtilities.dropAndReplaceCardsInHand(player.hand,
                                    droppedCards.getOrElse(VicePres, List.empty), droppedCards.getOrElse(ViceBum, List.empty)))
        case (player, ViceBum) => player.copy(hand = GameUtilities.dropAndReplaceCardsInHand(player.hand,
                                    droppedCards.getOrElse(ViceBum, List.empty), droppedCards.getOrElse(VicePres, List.empty)))
        case (player, Bum) => player.copy(hand = GameUtilities.dropAndReplaceCardsInHand(player.hand,
                                    droppedCards.getOrElse(Bum, List.empty), droppedCards.getOrElse(President, List.empty)))
        case (player, Neutral) => player
      })
      .map(player => if(player.name == Game.realPlayerName) player.copy(isRealPlayer = true) else player),
      cardsReceivedByRealPlayer)
  }

  /* Returns a tuple of normal cards and non-normal cards
     Returns NormalCards in order
     Returns non normal cards in preference order of giving away
  */
  def getNormalAndNonNormalListsOfCardsFromHand(hand: Hand): (List[Card], List[Card]) = {
    val normalCardsInHand = GameUtilities.sortCards(hand.listOfCards.filter({case n: NormalCard => true; case _ => false}))
    val nonNormalCardsInHand = GameUtilities.sortCardsInPreferenceOrderOfGivingAwayBestCards(hand.listOfCards.filter({case n: NormalCard => false; case _ => true})).reverse
    (normalCardsInHand, nonNormalCardsInHand)
  }

  case class IllegalMoveSuppliedException(s: String) extends IllegalArgumentException(s)
  case class IllegalCardSuppliedException(s: String) extends IllegalArgumentException(s)
}