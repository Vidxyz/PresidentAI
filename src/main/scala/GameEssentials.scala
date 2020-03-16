import FaceValue._

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

case object Joker extends Card {
  override def toString: String = "<JOKER>"
  override def value: String = "JOKER"
  override val intValue: Int = -1
}

case class NormalCard(faceValue: Value, suit: Suit) extends Card {
  override def toString: String = "<" + faceValue.toString + "," + suit.toString + ">"
  override def value: String = faceValue.toString
  override val intValue: Int = faceValue match {
    case TWO => 2
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
  }
}

case class Hand(listOfCards: List[Card]) {

  override def toString: String = {
    if(listOfCards.isEmpty)
    else {
      var sizeSeen = 0      var stringValue = ""
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
  def moveFaceValue: Int = {
    if (cards.isEmpty) 0
    else cards.head.intValue
  }
  def highestCard: Card = cards.last
  def numberOfCards: Int = cards.size
  def begin: Boolean = cards.isEmpty
}
case class Moves(moves: List[Move])

case class Game(currentMove: Move)
