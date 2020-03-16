import FaceValue._
import Suits._
import GameUtilities._

object Main extends App {

  val numberOfPlayers = 4
  val totalNormalCards = 54

  val sampleHand: Hand = GameUtilities.dealNewHand(numberOfPlayers, totalNormalCards)

  val stackedHand: Hand = Hand(List(
    NormalCard(TEN, Diamond),
    NormalCard(JACK, Club),
    NormalCard(ACE, Diamond),
    NormalCard(KING, Heart),
    NormalCard(FOUR, Spade),
    NormalCard(TEN, Club),
    NormalCard(TEN, Heart),
    NormalCard(ACE, Heart),
    NormalCard(TWO, Diamond),
    NormalCard(TWO, Heart),
    NormalCard(TWO, Spade),
    NormalCard(TWO, Club),
    Joker,
    Joker,
  ))

  val currentState = Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Spade), NormalCard(SIX, Heart), NormalCard(SIX, Club)))

  val AI = Player("AI", stackedHand)
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

  val validMoves: Moves = getValidMoves(allMoves, currentState)
  print(validMoves)
  println("\n")

}




