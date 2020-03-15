import FaceValue._
import Suits._
import GameUtilities._

object Main extends App {

  val numberOfPlayers = 4
  val totalNormalCards = 54

  val sampleHand: Hand = GameUtilities.dealNewHand(numberOfPlayers, totalNormalCards)

  val errorHand: Hand = Hand(List(
    NormalCard(TEN, Diamond),
    NormalCard(JACK, Club),
    NormalCard(ACE, Diamond),
    NormalCard(KING, Heart),
    NormalCard(FOUR, Spade),
    NormalCard(TEN, Club),
    NormalCard(TEN, Heart),
    NormalCard(ACE, Heart),
    NormalCard(EIGHT, Heart),
    NormalCard(FOUR, Heart),
    NormalCard(SIX, Diamond),
    NormalCard(EIGHT, Diamond),
    Joker,
    Joker,
  ))

  val AI = Player("AI", errorHand)
//  val AI = Player("AI", sampleHand)

  print(AI.hand)
  println('\n')

  val newHand: Hand = sortCards(AI.hand.listOfCards)
  print(newHand)
  println("\n")

  val intermediatsets: List[List[Card]] = getListOfIntermediateSets(newHand.listOfCards)
  print(intermediatsets)
  println("\n")

  val allMoves: Moves = getAllMoves(intermediatsets)
  print(allMoves)
  println("\n")

  val validMoves: Moves = getValidMoves(allMoves, Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Spade))))
  print(validMoves)
  println("\n")

}




