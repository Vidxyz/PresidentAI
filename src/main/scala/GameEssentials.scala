import Consants.numberToCardMap

import scala.annotation.tailrec

sealed trait Suit
sealed trait Value
sealed trait Card {
  def value: String = "Card"
}

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
  override def value: String = toString
}
case class NormalCard(faceValue: Value, suit: Suit) extends Card {
  override def toString: String = "<" + faceValue.toString + "," + suit.toString + ">"
  override def value: String = faceValue.toString
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

  /*
  Sort cards according to their (faceValue, suit)
  Sorting logic is as follows :-
  Diamonds < Clubs < Hearts < Spades
  3 < 4 < 5 < ..... < K < A < 2 < JOKER
  3_Diamonds < 3_Clubs < 3_Hears < 3_Spades
   */
  def sortCards: Hand = {
    Hand(this.listOfCards.sortWith(
      (card1, card2) =>
        numberToCardMap.find(_._2 == card1).map(_._1).getOrElse(-1) <
          numberToCardMap.find(_._2 == card2).map(_._1).getOrElse(-1)
    ))
  }

  def getListOfSets: List[Set[Card]] = {
    @tailrec
    def getListOfSetsHelper(lastCardSeen: Card, startIndex: Int,
                            endIndex: Int, listSoFar: List[Set[Card]]): List[Set[Card]] = {
      if (startIndex + 1 == this.listOfCards.size) listSoFar :+ Set() ++ this.listOfCards.slice(startIndex, endIndex)
      else {
        if(lastCardSeen.value == this.listOfCards(endIndex).value)
          getListOfSetsHelper(this.listOfCards(endIndex), startIndex, endIndex + 1, listSoFar)
        else
          getListOfSetsHelper(this.listOfCards(endIndex), endIndex, endIndex + 1,
            listSoFar :+ Set() ++ this.listOfCards.slice(startIndex, endIndex))
      }
    }
    getListOfSetsHelper(this.listOfCards.head, 0, 1, List.empty)
  }

  // Parse current listOfCards to make a set of valid moves
  def getAllMoves: Moves = {
    ???
  }

  def getValidMoves(allMoves: Moves): Moves = {
    ???
  }

}

case class Move(cards: List[Card])
case class Moves(moves: List[Move])

case class Game(currentMove: Move)
