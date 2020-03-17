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


  var AI = Player("AI", GameUtilities.dealNewHand(numberOfPlayers, totalNormalCards))
  var computer = Player("Computer", GameUtilities.dealNewHand(numberOfPlayers, totalNormalCards))

  val listOfNames = List("Player1", "Player2", "Player3", "Player4")
  val listOfPlayers = GameUtilities.generatePlayersAndDealHands(listOfNames).toBuffer

  println("The starting state is : " + currentState)
  println("\n")

  // Player who's next turn it is to play
  // 0 <= currentPlayerNumber < totalNumberOfPlayers
  var maxAllowedSkips = listOfPlayers.size - 1
  var numberOfSkips = 0

  var round = Round(currentState, "", listOfPlayers.size, 0,
    listOfPlayers.toList, Round.getNoPassList(listOfPlayers.size))

  while(listOfPlayers
                    .map(player => player.status)
                    .map(playerstatus => playerstatus == Active)
                    .count(_ == true) > 1) {

    // To avoid and infinite loop of None moves, we restore currentState to empty if it is our turn and we played the last move too
    if(/*round.checkIfLastMovePlayedBy(currentPlayerObject.name) || */round.hasEveryonePassed) {
      println("Clearing state due to passing by other players. RoundPassStatus list is")
      println(round.roundPassStatus)
      currentState = Move(List.empty)

      // This fails when player exited on a burn, as their index cannot be found to be last person to have played
      val nextPlayerIndex =  try {
        round.getIndexOf(round.lastMovePlayedBy)
      } catch {
        case e: Exception => round.currentPlayerTurn
      }
      round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size,
        nextPlayerIndex, listOfPlayers.toList, Round.getNoPassList(listOfPlayers.size))
    }

    val currentPlayerObject = listOfPlayers(round.currentPlayerTurn)

    println(currentPlayerObject.name)
    println("------------------------")
    println("Hand")
    println(Hand(sortCards(currentPlayerObject.hand.listOfCards)))

    val nextMove: Option[Move] =
    // If player has not skipped turn this round already, then they get to play
    if(!round.hasAlreadySkippedTurn(currentPlayerObject.name)) currentPlayerObject.playNextMove(currentPlayerObject.hand, currentState)
    else {
      println("SKIPPING TURN BECAUSE ALREADY PASSED IN THIS ROUND")
      None
    }
    println("The next move is : " + nextMove)

    if(nextMove.isDefined){
      round = Round(currentState, currentPlayerObject.name, listOfPlayers.size, round.currentPlayerTurn, listOfPlayers.toList, round.roundPassStatus)
    }
    // This means that the user is passing, nextMove is not defined
    else {
      println("SKIPPING TURN")
      val newPassList = (round.listOfPlayers zip round.roundPassStatus)
                                    .map {
                                      case (player, status) =>
                                        if (player.name == listOfPlayers(round.currentPlayerTurn).name) true
                                        else status
                                    }
      round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size, round.currentPlayerTurn, listOfPlayers.toList, newPassList)
    }

    currentState = getNextGameState(currentState, nextMove)

    // Reset roundPassStatus list if currentState has become Empty
    // This can only happen when it is a suit-burn/2/Joker right now
    if(currentState.cards.isEmpty)
      round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size, round.currentPlayerTurn, listOfPlayers.toList, Round.getNoPassList(listOfPlayers.size))
    else
      round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size, round.currentPlayerTurn, listOfPlayers.toList, round.roundPassStatus)

    println("The current round state is : " + round.gameState)
//    println("The pass status is : " + round.roundPassStatus)

    val newHandAfterPlaying = currentPlayerObject.getNewHand(currentPlayerObject.hand, nextMove)
    listOfPlayers.update(round.currentPlayerTurn, Player(currentPlayerObject.name, newHandAfterPlaying))

    if(listOfPlayers(round.currentPlayerTurn).status == Complete) {
      println(listOfPlayers(round.currentPlayerTurn).name + " has finished!")
      listOfPlayers.remove(round.currentPlayerTurn)
      round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size, round.currentPlayerTurn - 1, listOfPlayers.toList, round.roundPassStatus)
    }


    println("\n")

    // Need to know which player to switch move to
    // Incrementally, or depending on game state
    // Also need to skip over those that are complete
    // Only change hands if currentState is NON-EMPTY
    // Empty state signifies a BURN has just taken place, the currentPlayer in question does not change
    // -----
    // If everyone passes, then the person who played last gets to play first
    // If everyone passes, and the person who played last is out, then the next person in line gets to start
    // Need to maintain this ordering somehow
    if(currentState.cards.nonEmpty || round.playerFinishedTheirTurnOnABurn)  {
      if (round.currentPlayerTurn + 1 == listOfPlayers.size)
        round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size, 0, listOfPlayers.toList, round.roundPassStatus)
      else
        round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size, round.currentPlayerTurn + 1, listOfPlayers.toList, round.roundPassStatus)
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




