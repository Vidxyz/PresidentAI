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


