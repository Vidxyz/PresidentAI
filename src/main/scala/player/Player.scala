package player

import game.GameUtilities._
import game.GameEngine.getNextMoveWrapper
import game._
import scala.io.StdIn._
import util.control.Breaks._


case class Player(name: String, hand: Hand, isRealPlayer: Boolean = false) {

  lazy val status: PlayerStatus = if (hand.listOfCards.isEmpty) Complete else Active
  implicit lazy val playerIndicators: PlayerIndicators = PlayerIndicators(hand)

  /*
  Returns the move chosen to play, given current hand and current state
   */
  def playNextMove(currentHand: Hand, currentState: Move): Option[Move] = {
    if(isRealPlayer) promptForNextMove(currentHand, currentState)
    else {
      val sortedHand = Hand(sortCards(currentHand.listOfCards))
      val intermediateLists: List[List[Card]] = getListsOfSimilarCards(sortedHand)
      val intermediateListsWithoutThrees: List[List[Card]] = intermediateLists.filter(list => list.head.intValue != 3)
      val listOfThreesInHand: List[Card] = getWildCardListFromIntermediateList(intermediateLists)
      val allMovesWithoutThrees: Moves = getAllMoves(intermediateListsWithoutThrees)
      val allMoves: Moves = addThreesToMoves(allMovesWithoutThrees, listOfThreesInHand)
      val validMoves: Moves = getValidMoves(allMoves, currentState)
      val validMovesWithWildCardsOptimallyAssigned: Moves = assignWildCardsOptimally(validMoves, currentState)
      val nextMove: Option[Move] = getNextMoveWrapper(validMovesWithWildCardsOptimallyAssigned, currentState)
      nextMove
    }
  }

  /*
  Prompts user for next move in the following format
  <3,Spade> <10,Club> <10,Heart>
  <Joker>
  <2,Diamond> <2,Club>
  <3,Club(8)> <3,Spade(8)>
  aka - cards are space separated, and 3s have to be explicitly assigned if played by themselves
  // TODO - penalize threes in reverse order? Maybe its more desirable to use threes with lower cards to get rid of them
            instead of using them with already higher cards
            Revise penalty to maximize usage of THREE with single cards? over doubles, triples...?
   */
  def promptForNextMove(currentHand: Hand, gameState: Move): Option[Move] = {
    val userMove: String = readLine("Enter your move : ")
    userMove.toLowerCase match {
      case "pass" => None
      case _ =>
        try {
          val move = parseUserLine(userMove)
          if(!GameUtilities.isValidMove(move.get, gameState) ||
            (currentHand.listOfCards.size - GameUtilities.getNewHand(currentHand, move).listOfCards.size != move.get.parity)) {
            println("Illegal move, please try again.")
            promptForNextMove(currentHand, gameState)
          }
          else move
        }
        catch {
          case e: Exception =>
            println("Invalid input, please try again.")
            promptForNextMove(currentHand, gameState)
        }
    }
  }

  def parseUserLine(userMove: String): Option[Move] = {
    Some(Move(GameUtilities.sortCards(userMove.split(" ")
      .map(cardString => {
        val pair = cardString.split(",")
          .map(section => section.filter(character => character != '<' && character != '>'))
          .toList
        val tuple = pair match {
          case List(a, b) => (a, b);
          case List(a) => (a, a) // In the case of <Joker>
        }
        GameUtilities.getCardFromCardStrings(tuple._1, tuple._2)
      }).toList)))
  }

}



case object PlayerIndicators {

  /*
  Spline interpolation via https://tools.timodenk.com/cubic-spline-interpolation
  Data points used -
  x  |  y
  --------
  1  | 100
  2  | 99
  6  | 50
  10 | 20
  20 | 10
  30 | 0

  Returns a 0-100 Double value of a highCardModifier
   */
  def applyCustomSpecialCardModifier(sizeOfHand: Int): Double = {
    sizeOfHand match {
      case x if 1 to 2 contains x =>
        (-1.364 * Math.pow(x, 3)) + (4.0921 * Math.pow(x, 2)) + (-3.7281 * x) + 101
      case x if 3 to 6 contains x =>
        (0.49041 * Math.pow(x, 3)) + (-7.0346 * Math.pow(x, 2)) + (18.525 * x) + 86.164
      case x if 7 to 10 contains x =>
        (-0.10911 * Math.pow(x, 3)) + (3.7567 * Math.pow(x, 2)) + (-46.222 * x) + 215.66
      case x if 11 to 20 contains x =>
        (-0.020145 * Math.pow(x, 3)) + (1.0878 * Math.pow(x, 2)) + (-19.534 * x) + 126.7
      case x if 21 to 30 contains x =>
        (.004029 * Math.pow(x, 3)) + (-0.36261 * Math.pow(x, 2)) + (9.4755 * x) - 66.697
      case _ => 0
    }
  }

  /*
  Returns 0-1 value giving the penalty modifier for wildcards as a function of hand size
   */
  def applyWildCardPenaltyModifer(sizeOfHand: Int): Double = {
    1 /(1 + scala.math.exp(-((0.5d * sizeOfHand) - 4)))
  }
}


case class PlayerIndicators(hand: Hand) {

  import PlayerIndicators._

  // Likelihood of playing a special card. Increases as the game moves on (hand nears empty)
  lazy val specialCardModifier: Double = applyCustomSpecialCardModifier(hand.listOfCards.size)/100
  lazy val wildCardPenaltyModifier: Double = applyWildCardPenaltyModifer(hand.listOfCards.size)
  lazy val highCardModifier: Double =  if(hand.delta == 0) 1d else 1d/hand.delta

  /*
  Gets the total number of cards of the type that is being played
  Returns
  Generally speaking, it is desirable to play all cards of the same type at once
  Example :- Playing quad4s when you have 4-4-4-4 in your hand, over triple4s, double4s, single4s
  Assumes that the card used in the validMove is present in Hand.
  If not, leads to an exception being thrown, and the nextMove defaulting to None
   */
  def getListSetSizeForCard(validMove: Move): Int = {
    // If the move is comprised entirely of wildcards, then parity of move itself
    if(GameUtilities.getNumberOfWildCardsInMove(validMove) == validMove.parity) validMove.parity
    else hand.listOfSimilarCards.filter(l => l.head.intValue == validMove.moveFaceValue).head.size
  }

}
