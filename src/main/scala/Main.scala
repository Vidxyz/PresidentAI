import FaceValue.{ACE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, TEN, JACK, QUEEN, KING}
import Suits.{Diamond, Spade, Heart, Club}

object Main extends App {

  val numberOfPlayers = 4
  val totalNormalCards = 54

  val sampleHand: Hand = GameUtilities.dealNewHand(numberOfPlayers, totalNormalCards)

  val AI = Player("AI", sampleHand)

  print(AI.hand)

  println('\n')

  val newHand = AI.hand.sortCards

  print(newHand)

  println("\n")
  val intermediatsets = newHand.getListOfIntermediateSets

  println("\n")
  print(intermediatsets)

//
  val allMoves = newHand.getAllMoves(intermediatsets)
  println("\n")
//
  print(allMoves)

  println("\n")

  val validMoves = newHand.getValidMoves(allMoves, Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Spade))))

  print(validMoves)

}




