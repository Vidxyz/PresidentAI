import FaceValue._
import Suits._

object Main extends App {

  val numberOfPlayers = 6
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
    SpecialCard(TWO, Heart),
    SpecialCard(TWO, Spade),
    Joker,
    Joker,
  ))

  val errorHand = Hand(List(
    NormalCard(FOUR, Diamond),
    NormalCard(FIVE, Heart),
    NormalCard(FIVE, Spade),
    NormalCard(TEN, Club),
    NormalCard(JACK, Heart),
    NormalCard(KING, Spade),
    NormalCard(ACE, Club)))

  var currentState = Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club)))
//  val currentState = Move(List.empty)
//  val currentState = Move(List(NormalCard(TWO, Heart)))
//  val currentState = Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Club), NormalCard(ACE, Club)))
//  var AI = Player("AI", errorHand, Active)
//  var AI = Player("AI", stackedHand, Active)
//  val listOfPlayers = GameUtilities.createPlayers(listOfNames)
//  var AI = Player("AI", GameUtilities.dealNewHand(numberOfPlayers, totalNormalCards))
//  var computer = Player("Computer", GameUtilities.dealNewHand(numberOfPlayers, totalNormalCards))

  val listOfNames = List("Player1", "Player2", "Player3", "Player4")
  // Comment out seed for true randomness
  val listOfPlayers = GameUtilities.generatePlayersAndDealHands(listOfNames, seed=77).toBuffer

  val game = Game(Move(List.empty))
//  val game = Game(currentState)

  println("The starting state is : " + game.startState)
  println("\n")
  game.play(listOfPlayers)

}




