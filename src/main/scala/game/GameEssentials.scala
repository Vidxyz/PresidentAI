package game

import game.FaceValue.{TWO, _}

sealed trait PlayerStatus
sealed trait Suit
sealed trait Value
sealed trait Card {
  def value: String = "game.Card"
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
    case _ => throw IllegalFaceValueException("NormalCard provided with illegal face value")
  }
}

case class WildCard(faceValue: Value, suit: Suit, assumedValue: Int = 0) extends Card {
  override def toString: String = "<" + faceValue.toString + "," + suit.toString + "(" + assumedValue + ")>"
  override def value: String = faceValue.toString
  override val intValue: Int = faceValue match {
    case THREE => 3
    case _ => throw IllegalFaceValueException("WildCard provided with illegal face value")
  }
}

case class SpecialCard(faceValue: Value = TWO, suit: Suit) extends Card {
  override def toString: String = "<" + faceValue.toString + "," + suit.toString + ">"
  override def value: String = faceValue.toString
  override val intValue: Int = faceValue match {
    case TWO => 2
    case _ => throw IllegalFaceValueException("SpecialCard provided with illegal face value")
  }
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
    val intermediateListWithoutThrees = listOfSimilarCards.filter(listOfCard => listOfCard.head.intValue != 3)
    if(listOfSimilarCards.size == 1 || intermediateListWithoutThrees.size == 1) 0
    else {
      var lastIntValueSeen  = intermediateListWithoutThrees.tail.head.head.intValue
      intermediateListWithoutThrees
        .tail.tail
        .foldLeft(intermediateListWithoutThrees.tail.head.head.intValue - intermediateListWithoutThrees.head.head.intValue)(
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
A move is classified as a sorted List[game.Card] sorted as per numberToCardMap
 */
case class Move(cards: List[Card], likelihood: Double = 0) {
  override def toString: String = if(cards.nonEmpty) "game.Move(" + cards + ")" +
    (if (likelihood > 0) s"[$likelihood]" else "") else "EMPTY"

  def moveFaceValue: Int = {
    if (cards.isEmpty) 0
    else {
      highestCard match {
        case w: WildCard => w.assumedValue
        case n: NormalCard => n.intValue
        case s: SpecialCard => s.intValue
        case joker: Card => joker.intValue
      }
    }
  }

  /*
  Returns the highest card in the hand
  Defined as card with strongest suit
   */
  def highestCard: Card = {
    if(cards.size == 1) cards.head
    else {
      cards.foldLeft(cards.head)((highestCard, currentCard) =>
        if(GameUtilities.cardOrderValue(currentCard) >
          GameUtilities.cardOrderValue(highestCard)) currentCard
        else highestCard)
    }
  }
  def parity: Int = cards.size
  def numberOfNormalcards: Int = cards.count(card => card match {
    case w: WildCard => false;
    case _ => true
  })
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

  /*
  Override equals method to match on everything but likelihood
   */
  override def equals(obj: Any): Boolean = {
    obj match {
      case obj: Move => obj.canEqual(this) && this.cards == obj.cards
      case _ => false
    }
  }

  def canEqual(a: Any): Boolean = a.isInstanceOf[Move]
}

/*
A wrapper around list of moves, for logical reasons
 */
case class Moves(moves: List[Move])


