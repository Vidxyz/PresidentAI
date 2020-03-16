import FaceValue._
import Suits._
import GameUtilities._

object Main extends App {

  val numberOfPlayers = 4
  val totalNormalCards = 54

  val stackedHand: Hand = Hand(List(
    NormalCard(TEN, Diamond),
    NormalCard(SIX, Heart),
    NormalCard(SIX, Spade),
    NormalCard(KING, Heart),
    NormalCard(FOUR, Spade),
    NormalCard(TEN, Club),
    NormalCard(TEN, Heart),
    NormalCard(EIGHT, Heart),
    NormalCard(SEVEN, Diamond),
    NormalCard(SEVEN, Heart),
    NormalCard(TWO, Heart),
    NormalCard(TWO, Spade),
    Joker,
    Joker,
  ))

  val currentState = Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club)))
//  val currentState = Move(List.empty)
//  val currentState = Move(List(NormalCard(TWO, Heart)))
//  val currentState = Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Club), NormalCard(ACE, Club)))

//  var AI = Player("AI", stackedHand)
  var AI = Player("AI", GameUtilities.dealNewHand(numberOfPlayers, totalNormalCards), Active)
  var computer = Player("Computer", GameUtilities.dealNewHand(numberOfPlayers, totalNormalCards), Active)

  val sortedHand: Hand = Hand(sortCards(AI.hand.listOfCards))
  print(sortedHand)
  println("\n")

  val validMoves: Moves = getValidMoves(getAllMoves(getListsOfSimilarCards(sortedHand)), currentState)
  print(validMoves)
  print("\n")

  val nextMove = AI.playNextMove(AI.hand, currentState)
    println("The next move is : " + nextMove)
    println("\n")

  val nextGameState = getNextGameState(currentState, nextMove)
  println("The next game state is : " + nextGameState)
  println("\n")

  val newHandAfterPlaying = AI.getNewHand(AI.hand, nextMove)
  AI = Player(AI.name, newHandAfterPlaying, AI.status)
  println(Hand(sortCards(AI.hand.listOfCards)))
}




