import FaceValue._
import Suits._
import GameUtilities._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

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
    NormalCard(TWO, Heart),
    NormalCard(TWO, Spade),
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


  var AI = new Player("AI", GameUtilities.dealNewHand(numberOfPlayers, totalNormalCards))
  var computer = Player("Computer", GameUtilities.dealNewHand(numberOfPlayers, totalNormalCards))

  val listOfNames = List("AI", "Computer", "Player3", "JohnDoe")
  val listOfPlayers = GameUtilities.generatePlayersAndDealHands(listOfNames).toBuffer
  val totalNumberOfPlayers = listOfPlayers.size

  println("The starting state is : " + currentState)
  println("\n")

  // Player who's next turn it is to play
  // 0 <= currentPlayerNumber < totalNumberOfPlayers
  var currentPlayerNumber = 0

  while(listOfPlayers
                    .map(player => player.status)
                    .map(playerstatus => playerstatus == Active)
                    .forall(bool => bool)) {

    val currentPlayerObject = listOfPlayers(currentPlayerNumber)
    println("-----------------------------------------------------------------------------------------")
    println(currentPlayerObject.name)
    println("-----------------------------------------------------------------------------------------")
    println(Hand(sortCards(currentPlayerObject.hand.listOfCards)))
    val nextMove: Option[Move] = currentPlayerObject.playNextMove(currentPlayerObject.hand, currentState)
    println("The next move is : " + nextMove)
    currentState = getNextGameState(currentState, currentPlayerObject.playNextMove(currentPlayerObject.hand, currentState))
    println("The current state is : " + currentState)
    val newHandAfterPlaying = currentPlayerObject.getNewHand(currentPlayerObject.hand, nextMove)
    listOfPlayers.update(currentPlayerNumber, Player.apply(currentPlayerObject.name, newHandAfterPlaying))
    println(listOfPlayers(currentPlayerNumber).status)
    println("-----------------------------------------------------------------------------------------")
    println("\n")

    // Need to know which player to switch move to
    // Incrementally, or depending on game state
    if(currentState.cards.nonEmpty) {
      if (currentPlayerNumber + 1 == totalNumberOfPlayers) currentPlayerNumber = 0
      else currentPlayerNumber += 1
    }

    Thread.sleep(100)
  }


//  val sortedHand: Hand = Hand(sortCards(AI.hand.listOfCards))
//  print("Sorted hand : " + sortedHand)
//  println("\n")
//
//  val intermediate = getListsOfSimilarCards(sortedHand)
//  print("Intermediate : " + intermediate)
//  println("\n")
//
//  val validMoves: Moves = getValidMoves(getAllMoves(getListsOfSimilarCards(sortedHand)), currentState)
//  print(validMoves)
//  print("\n")
//
//  val nextMove = AI.playNextMove(AI.hand, currentState)
//    println("The next move is : " + nextMove)
//    println("\n")
//
//  val nextGameState = getNextGameState(currentState, nextMove)
//  println("The next game state is : " + nextGameState)
//  println("\n")
//
//  val newHandAfterPlaying = AI.getNewHand(AI.hand, nextMove)
//  AI = Player(AI.name, newHandAfterPlaying, AI.status)
//  println(Hand(sortCards(AI.hand.listOfCards)))

}




