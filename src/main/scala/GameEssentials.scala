import FaceValue._
import GameUtilities.{getNextGameState, sortCards}

import scala.collection.mutable

sealed trait PlayerStatus
sealed trait Suit
sealed trait Value
sealed trait Card {
  def value: String = "Card"
  val intValue: Int
}

case object Active extends PlayerStatus
case object Complete extends PlayerStatus

object Suits {
  case object Diamond extends Suit
  case object Club extends Suit
  case object Heart extends Suit
  case object Spade extends Suit
}

object FaceValue {
  case object ACE extends Value
  case object TWO extends Value
  case object THREE extends Value
  case object FOUR extends Value
  case object FIVE extends Value
  case object SIX extends Value
  case object SEVEN extends Value
  case object EIGHT extends Value
  case object NINE extends Value
  case object TEN extends Value
  case object JACK extends Value
  case object QUEEN extends Value
  case object KING extends Value
}

case class IllegalFaceValueException(s: String) extends IllegalArgumentException(s)

case object Joker extends Card {
  override def toString: String = "<JOKER>"
  override def value: String = "JOKER"
  override val intValue: Int = -1
}

case class NormalCard(faceValue: Value, suit: Suit) extends Card {
  override def toString: String = "<" + faceValue.toString + "," + suit.toString + ">"
  override def value: String = faceValue.toString
  override val intValue: Int = faceValue match {
    case THREE => 3
    case FOUR => 4
    case FIVE => 5
    case SIX => 6
    case SEVEN => 7
    case EIGHT => 8
    case NINE => 9
    case TEN => 10
    case JACK => 11
    case QUEEN => 12
    case KING => 13
    case ACE => 14
    case _ => throw IllegalFaceValueException("Normal Card provided with illegal face value")
  }
}

case class SpecialCard(faceValue: Value = TWO, suit: Suit) extends Card {
  override def toString: String = "<" + faceValue.toString + "," + suit.toString + ">"
  override def value: String = faceValue.toString
  override val intValue: Int = faceValue match {
    case TWO => 2
    case _ => throw IllegalFaceValueException("Special Card provided with illegal face value")
  }
}

case class Hand(listOfCards: List[Card]) {

  /*
  Strength is defined as the numerical step-delta between
  We always want to minimize this?
   */
  lazy val strength: Int = GameUtilities
    .getListsOfSimilarCards(Hand(GameUtilities
      .sortCards(this.listOfCards)))
    .size

  def size: Int = this.listOfCards.size

  override def toString: String = {
    if(listOfCards.isEmpty) "EMPTY"
    else {
      var sizeSeen = 0
      var stringValue = ""
      while (sizeSeen < listOfCards.size) {
        stringValue = stringValue + listOfCards.slice(sizeSeen, sizeSeen + 7).toString + "\n"
        sizeSeen += 7
      }
      stringValue
    }
  }
}

/*
A move is classified as a sorted List[Card] sorted as per numberToCardMap
 */
case class Move(cards: List[Card]) {
  override def toString: String = if(cards.nonEmpty) "Move(" + cards + ")" else "EMPTY"

  def moveFaceValue: Int = {
    if (cards.isEmpty) 0
    else cards.head.intValue
  }
  def highestCard: Card = cards.last
  def parity: Int = cards.size
}
case class Moves(moves: List[Move])

case class Game(startState: Move) {

  /*
 Simulates a run of the game, given a list of Player and a starting state
  */
  def play(listOfPlayers: mutable.Buffer[Player]): Unit = {

    var currentState = this.startState

    var round = Round(currentState, "", listOfPlayers.size, 0,
      listOfPlayers.toList, Round.getNoPassList(listOfPlayers.size))

    // Keep the game going, until exactly one player is Active (Bum)
    while(listOfPlayers
      .map(player => player.status)
      .map(playerstatus => playerstatus == Active)
      .count(_ == true) > 1) {

      // Reset state if everyone has passed
      if(round.hasEveryoneExceptThePlayerWhoPlayedTheLastMovePassed) {
        println("END OF ROUND")
        //      println(round.roundPassStatus)
        currentState = Move(List.empty)

        // Since everyone else has passed, next one to play is the last one who played
        val nextPlayerIndex =  try {
          round.getIndexOf(round.lastMovePlayedBy)
        } catch {
          case e: Exception => round.currentPlayerTurn
        }

        // Update round with index of next player
        round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size,
          nextPlayerIndex, listOfPlayers.toList, Round.getNoPassList(listOfPlayers.size))
      }

      val currentPlayerObject = listOfPlayers(round.currentPlayerTurn)
      println(currentPlayerObject.name)
      println(Hand(sortCards(currentPlayerObject.hand.listOfCards)))

      val nextMove: Option[Move] =
      // If player has not skipped turn this round already, then they get to play
        if(!round.hasAlreadySkippedTurn(currentPlayerObject.name))
          currentPlayerObject.playNextMove(currentPlayerObject.hand, currentState)
        else {
          println("PASSED ALREADY")
          None
        }
      println("The next move is : " + nextMove)

      // If nextMove is not none, update lastMovePlayedBy
      if(nextMove.isDefined){
        round = Round(currentState, currentPlayerObject.name, listOfPlayers.size, round.currentPlayerTurn, listOfPlayers.toList, round.roundPassStatus)
      }
      // This means that the user is passing, nextMove is not defined. Update the roundPassStatus list
      else {
        println("PASS")
        val newPassList = (round.listOfPlayers zip round.roundPassStatus)
          .map { case (player, status) => if (player.name == listOfPlayers(round.currentPlayerTurn).name) true
          else status}
        round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size, round.currentPlayerTurn, listOfPlayers.toList, newPassList)
      }

      currentState = getNextGameState(currentState, nextMove)
      // Reset roundPassStatus list if currentState has become Empty
      // This can only happen when it is a suit-burn/2-burn/Joker/All-pass right now
      if(currentState.cards.isEmpty)
        round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size, round.currentPlayerTurn, listOfPlayers.toList, Round.getNoPassList(listOfPlayers.size))
      else
        round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size, round.currentPlayerTurn, listOfPlayers.toList, round.roundPassStatus)

      println("The current round state is : " + round.gameState)
      //    println("The pass status is : " + round.roundPassStatus)

      val newHandAfterPlaying = currentPlayerObject.getNewHand(currentPlayerObject.hand, nextMove)
      listOfPlayers.update(round.currentPlayerTurn, Player(currentPlayerObject.name, newHandAfterPlaying))

      // Check if playing last move led player to complete
      if(listOfPlayers(round.currentPlayerTurn).status == Complete) {
        println(listOfPlayers(round.currentPlayerTurn).name + " has finished!\n")
        listOfPlayers.remove(round.currentPlayerTurn)
        round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size, round.currentPlayerTurn - 1, listOfPlayers.toList, round.roundPassStatus)
      }

      /*
       Only change hands if currentState is NON-EMPTY
       If it is EMPTY, it means the currentPLayer gets to go again
       Empty state signifies a BURN has just taken place, the currentPlayer in question does not change
       The only exception here is when the player had finished their hand on a card that led to a BURN
      */
      if(currentState.cards.nonEmpty || round.playerEndedTheGameOnABurn)  {
        if (round.currentPlayerTurn + 1 == listOfPlayers.size)
          round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size, 0, listOfPlayers.toList, round.roundPassStatus)
        else
          round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size, round.currentPlayerTurn + 1, listOfPlayers.toList, round.roundPassStatus)
      }

      println("------------------------\n")
//      Thread.sleep(100)
    }
  }
}

