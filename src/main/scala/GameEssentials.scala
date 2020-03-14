sealed trait Suit
sealed trait Value
sealed trait Card

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
  override def toString: String = "JOKER"
}
case class NormalCard(faceValue: Value, suit: Suit) extends Card {
  override def toString: String = "<" + faceValue.toString + "," + suit.toString + ">"
}

case class Hand(listOfCards: List[Card]) {

  override def toString: String = {
    val size = listOfCards.size
    var sizeSeen = 0
    var stringValue = ""
    while(sizeSeen < size) {
      stringValue = stringValue + listOfCards.slice(sizeSeen, sizeSeen + 4).toString + "\n"
      sizeSeen += 4
    }
    stringValue
  }

}

case class Move(cards: List[Card])
case class Moves(moves: List[Move])

case class Game(currentMove: Move)
