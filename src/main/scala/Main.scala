import game.FaceValue._
import game.Suits._
import game.{Game, GameUtilities, Hand, Joker, Move, NormalCard, SpecialCard}

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
//  val currentState = game.Move(List.empty)
//  val currentState = game.Move(List(game.NormalCard(TWO, Heart)))
//  val currentState = game.Move(List(game.NormalCard(ACE, Diamond), game.NormalCard(ACE, Club), game.NormalCard(ACE, Club)))
//  var AI = player.Player("AI", errorHand, game.Active)
//  var AI = player.Player("AI", stackedHand, game.Active)
//  val listOfPlayers = game.GameUtilities.createPlayers(listOfNames)
//  var AI = player.Player("AI", game.GameUtilities.dealNewHand(numberOfPlayers, totalNormalCards))
//  var computer = player.Player("Computer", game.GameUtilities.dealNewHand(numberOfPlayers, totalNormalCards))

  val listOfNames = List("Player1", "Player2", "Player3", "Player4")
  val listOfNames2 = List("Player1", "Player2")
  // Comment out seed for true randomness
//  val listOfPlayers = game.GameUtilities.generatePlayersAndDealHands(listOfNames, seed=5).toBuffer
  // 77 and 13 are good seeds
//  val listOfPlayers = GameUtilities.generatePlayersAndDealHands(listOfNames, seed=13).toBuffer
  val listOfPlayers = GameUtilities.generatePlayersAndDealHands(listOfNames).toBuffer
  val listOfPlayers2 = GameUtilities.generatePlayersAndDealHands(listOfNames2, seed=77).toBuffer

  val game = Game(Move(List.empty))
//  val game = game.Game(currentState)

  println("The starting state is : " + game.startState)
  println("\n")
//  game.play(listOfPlayers)
  game.play(listOfPlayers)

}




