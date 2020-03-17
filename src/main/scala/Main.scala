import FaceValue._
import Suits._
import GameUtilities._


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


  var AI =Player("AI", GameUtilities.dealNewHand(numberOfPlayers, totalNormalCards))
  var computer = Player("Computer", GameUtilities.dealNewHand(numberOfPlayers, totalNormalCards))

  val listOfNames = List("Player1", "Player2", "Player3", "Player4")
  val listOfPlayers = GameUtilities.generatePlayersAndDealHands(listOfNames).toBuffer

  println("The starting state is : " + currentState)
  println("\n")

  // Player who's next turn it is to play
  // 0 <= currentPlayerNumber < totalNumberOfPlayers
  var currentPlayerNumber = 0
  var lastMovePlayedBy = ""

  while(listOfPlayers
                    .map(player => player.status)
                    .map(playerstatus => playerstatus == Active)
                    .count(_ == true) > 1) {

    val currentPlayerObject = listOfPlayers(currentPlayerNumber)
//    val currentPlayerObject = listOfPlayers(currentPlayerNumber)

    // To avoid and infinite loop of None moves, we restore currentState to empty if it is our turn and we played the last move too
    // This implementation always discards the LAST CARD played by a player on their LAST turn (clears the state)
    // TODO - Bugfix here
    if(currentPlayerObject.name == lastMovePlayedBy ||
      (!listOfPlayers.map(player => player.name).contains(lastMovePlayedBy) && lastMovePlayedBy != "")) {
      println("Clearing state due to passing by other players")
      currentState = Move(List.empty)
    }

    println("-------------------------")
    println(currentPlayerObject.name)
    println("------------------------")
    println(Hand(sortCards(currentPlayerObject.hand.listOfCards)))

    val nextMove: Option[Move] = currentPlayerObject.playNextMove(currentPlayerObject.hand, currentState)
    println("The next move is : " + nextMove)

    if(nextMove.isDefined) lastMovePlayedBy = listOfPlayers(currentPlayerNumber).name

    currentState = getNextGameState(currentState, currentPlayerObject.playNextMove(currentPlayerObject.hand, currentState))
    println("The current state is : " + currentState)

    val newHandAfterPlaying = currentPlayerObject.getNewHand(currentPlayerObject.hand, nextMove)
    listOfPlayers.update(currentPlayerNumber, Player(currentPlayerObject.name, newHandAfterPlaying))

    if(listOfPlayers(currentPlayerNumber).status == Complete) {
      println(listOfPlayers(currentPlayerNumber).name + " has finished!")
      listOfPlayers.remove(currentPlayerNumber)
      currentPlayerNumber -= 1
    }

    println("-------------------------")
    println("\n")

    // Need to know which player to switch move to
    // Incrementally, or depending on game state
    // Also need to skip over those that are complete
    // Only change hands if currentState is NON-EMPTY
    // Empty state signifies a BURN has just taken place, the currentPlayer in question does not change

    // If everyone passes, then the person who played last gets to play first
    // If everyone passes, and the person who played last is out, then the next person in line gets to start
    // Need to maintain this ordering somehow
    if(currentState.cards.nonEmpty ||
      (!listOfPlayers.map(player => player.name).contains(lastMovePlayedBy) && lastMovePlayedBy != ""))  {
      if (currentPlayerNumber + 1 == listOfPlayers.size) currentPlayerNumber = 0
      else currentPlayerNumber += 1
    }

//    Thread.sleep(10)
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




