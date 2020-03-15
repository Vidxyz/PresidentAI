import FaceValue._
import Suits._
import GameUtilities._

object Main extends App {

  val numberOfPlayers = 4
  val totalNormalCards = 54

  val sampleHand: Hand = GameUtilities.dealNewHand(numberOfPlayers, totalNormalCards)

  val AI = Player("AI", sampleHand)

  print(AI.hand)
  println('\n')

  val newHand: Hand = sortCards(AI.hand.listOfCards)
  print(newHand)
  println("\n")

  val intermediatsets = getListOfIntermediateSets(newHand.listOfCards)
  print(intermediatsets)
  println("\n")

  val allMoves = getAllMoves(intermediatsets)
  print(allMoves)
  println("\n")

  val validMoves = getValidMoves(allMoves, Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Spade))))
  print(validMoves)
  println("\n")

}




