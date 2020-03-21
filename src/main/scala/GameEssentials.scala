import FaceValue._

sealed trait PlayerStatus
sealed trait Suit
sealed trait Value
sealed trait Card {
  def value: String = "Card"
  val intValue: Int
  val isFaceCard: Boolean
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
  override val isFaceCard: Boolean = false
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
  override lazy val isFaceCard: Boolean = if(intValue > 10) true else false
}

case class SpecialCard(faceValue: Value = TWO, suit: Suit) extends Card {
  override def toString: String = "<" + faceValue.toString + "," + suit.toString + ">"
  override def value: String = faceValue.toString
  override val intValue: Int = faceValue match {
    case TWO => 2
    case _ => throw IllegalFaceValueException("Special Card provided with illegal face value")
  }
  override val isFaceCard: Boolean = false
}

case class Hand(listOfCards: List[Card]) {

  /*
  Has similar cards together. This will be used to penalize breaking of sets
  Example :-
  [
    [4,4,4]
    [5]
    [8,8]
    [Q,Q,Q]
    [K]
  ]
   */
  lazy val listOfSimilarCards: List[List[Card]] = GameUtilities.getListsOfSimilarCards(Hand(GameUtilities.sortCards(listOfCards)))

  /*
  Delta is defined as difference in intValue (faceValue) between the highest NormalCard and lowest NormalCard
   */
  lazy val delta: Int = {
    val sortedCards = GameUtilities.sortCards(listOfCards).filter(card => card match {
      case NormalCard(_,_) => true
      case _ => false
    })
    if(sortedCards.nonEmpty) sortedCards.last.intValue - sortedCards.head.intValue else 0
  }

  /*
  WeaknessFactor is defined as the maximum faceValue difference between intermediate sets of lists
  For example:- If hand has the following cards (represented as a listOfSimilarCards)
  [4,  4,  4,]
  [5]
  [8, 8]
  [J]
  [Q]
  [K, K, K]
  Then weakness factor is :- 3 (5-8 and 8-J)
  If the hand only had
  [4, 4, 4]
  [K, K, K]
  Then weakness factor is :- 9
  If the hand only had
  [4, 4, 4]
  Then weakness factor is :- 0
  We always want to minimize this
   */
  def weaknessFactor: Int = {
    if(listOfSimilarCards.size == 1) 0 else {
      var lastIntValueSeen  = listOfSimilarCards.tail.head.head.intValue
      listOfSimilarCards
        .tail.tail
        .foldLeft(listOfSimilarCards.tail.head.head.intValue - listOfSimilarCards.head.head.intValue)(
          (maxDifferenceSoFar, list) => {
            if (list.head.intValue - lastIntValueSeen > maxDifferenceSoFar) {
              val newDifference = list.head.intValue - lastIntValueSeen
              lastIntValueSeen = list.head.intValue
              newDifference
            }
            else {
              lastIntValueSeen = list.head.intValue
              maxDifferenceSoFar
            }
          })
    }
  }

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
  def isEmpty: Boolean = cards.isEmpty

  /*
  More thought needs to be put into this
   */
  def getMoveNormalCardModifier: Double = {
    cards.head match {
      case Joker => -1
      case SpecialCard(_,_) => -1
      case c: NormalCard => c.intValue
    }
  }
}

/*
A wrapper around list of moves, for logical reasons
 */
case class Moves(moves: List[Move])


