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
    NormalCard(ACE, Club))

  )

  var currentState = Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club)))
//  val currentState = Move(List.empty)
//  val currentState = Move(List(NormalCard(TWO, Heart)))
//  val currentState = Move(List(NormalCard(ACE, Diamond), NormalCard(ACE, Club), NormalCard(ACE, Club)))

  var switch = true
//  var AI = Player("AI", errorHand, Active)
//  var AI = Player("AI", stackedHand, Active)
    val listOfNames = List("AI", "Computer")
//  val listOfPlayers = GameUtilities.createPlayers(listOfNames)


  var AI = Player("AI", GameUtilities.dealNewHand(numberOfPlayers, totalNormalCards), Active)
  var computer = Player("Computer", GameUtilities.dealNewHand(numberOfPlayers, totalNormalCards), Active)

  val listOfPlayers = ArrayBuffer(AI, computer)
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
    val nextMove = currentPlayerObject.playNextMove(currentPlayerObject.hand, currentState)
    println("The next move is : " + nextMove)
    currentState = getNextGameState(currentState, currentPlayerObject.playNextMove(currentPlayerObject.hand, currentState))
    println("The current state is : " + currentState)
    val newHandAfterPlaying = currentPlayerObject.getNewHand(currentPlayerObject.hand, nextMove)
    listOfPlayers.update(currentPlayerNumber, Player(currentPlayerObject.name, newHandAfterPlaying, currentPlayerObject.status))
    println("-----------------------------------------------------------------------------------------")
    println("\n")

    if (currentPlayerNumber + 1 == totalNumberOfPlayers) currentPlayerNumber = 0
    else currentPlayerNumber += 1


//    if (switch) {
//      println("-----------------------------------------------------------------------------------------")
//      println(AI.name)
//      println("-----------------------------------------------------------------------------------------")
//      println(Hand(sortCards(AI.hand.listOfCards)))
//      val nextMove = AI.playNextMove(AI.hand, currentState)
//      println("The next move is : " + nextMove)
//      currentState = getNextGameState(currentState, AI.playNextMove(AI.hand, currentState))
//      println("The current state is : " + currentState)
//      val newHandAfterPlaying = AI.getNewHand(AI.hand, nextMove)
//      AI = Player(AI.name, newHandAfterPlaying, AI.status)
//      println("-----------------------------------------------------------------------------------------")
//      println("\n")
//      switch = false
//    }
//    else {
//      println("-----------------------------------------------------------------------------------------")
//      println(computer.name)
//      println("-----------------------------------------------------------------------------------------")
//      println(Hand(sortCards(computer.hand.listOfCards)))
//      val nextMove = computer.playNextMove(computer.hand, currentState)
//      println("The next move is : " + nextMove)
//      currentState = getNextGameState(currentState, computer.playNextMove(computer.hand, currentState))
//      println("The current state is : " + currentState)
//      val newHandAfterPlaying = computer.getNewHand(computer.hand, nextMove)
//      computer = Player(computer.name, newHandAfterPlaying, computer.status)
//      println("-----------------------------------------------------------------------------------------")
//      println("\n")
//      switch = true
//    }

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




