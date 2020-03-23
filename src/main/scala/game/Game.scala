package game

import player.Player
import game.GameUtilities.{sortCards, getNextGameState}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Game(startState: Move) {

  /*
  Keeps a completion order of the form (playerName, roundEnded)
   */
  var playerCompletionOrder: ListBuffer[String] = new ListBuffer[String]

  /*
 Simulates a run of the game, given a list of player.Player and a starting state
  */
  def play(listOfPlayers: mutable.Buffer[Player]): Unit = {

    var currentState = this.startState

    var round = Round(currentState, "", listOfPlayers.size, 0,
      listOfPlayers.toList, Round.getNoPassList(listOfPlayers.size))

    // Keep the game going, until exactly one player is game.Active (Bum)
    while(listOfPlayers
      .map(player => player.status)
      .map(playerstatus => playerstatus == Active)
      .count(_ == true) > 1) {

      // Reset state if everyone has passed
      if(round.hasEveryoneExceptThePlayerWhoPlayedTheLastMovePassed) {
        println("END OF ROUND")
        //      println(round.roundPassStatus)
        currentState = Move(List.empty)

        // Since everyone else has passed, next one to play is the last one who played
        val nextPlayerIndex =  try {
          round.getIndexOf(round.lastMovePlayedBy)
        } catch {
          case e: Exception => round.currentPlayerTurn
        }

        // Update round with index of next player
        round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size,
          nextPlayerIndex, listOfPlayers.toList, Round.getNoPassList(listOfPlayers.size))
      }

      val currentPlayerObject = listOfPlayers(round.currentPlayerTurn)
      println(currentPlayerObject.name)
      println(Hand(sortCards(currentPlayerObject.hand.listOfCards)))

      val nextMove: Option[Move] =
      // If player has not skipped turn this round already, then they get to play
        if(!round.hasAlreadySkippedTurn(currentPlayerObject.name))
          currentPlayerObject.playNextMove(currentPlayerObject.hand, currentState)
        else {
          println("PASSED ALREADY")
          None
        }
      println("The next move is : " + nextMove)

      // If nextMove is not none, update lastMovePlayedBy
      if(nextMove.isDefined){
        round = Round(currentState, currentPlayerObject.name, listOfPlayers.size, round.currentPlayerTurn, listOfPlayers.toList, round.roundPassStatus)
      }
      // This means that the user is passing, nextMove is not defined. Update the roundPassStatus list
      else {
        println("PASS")
        val newPassList = (round.listOfPlayers zip round.roundPassStatus)
          .map { case (player, status) => if (player.name == listOfPlayers(round.currentPlayerTurn).name) true
          else status}
        round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size, round.currentPlayerTurn, listOfPlayers.toList, newPassList)
      }

      currentState = getNextGameState(currentState, nextMove)
      // Reset roundPassStatus list if currentState has become Empty
      // This can only happen when it is a suit-burn/2-burn/game.Joker/All-pass right now
      if(currentState.cards.isEmpty)
        round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size, round.currentPlayerTurn, listOfPlayers.toList, Round.getNoPassList(listOfPlayers.size))
      else
        round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size, round.currentPlayerTurn, listOfPlayers.toList, round.roundPassStatus)

      println("The current round state is : " + round.gameState)
      //    println("The pass status is : " + round.roundPassStatus)

      val newHandAfterPlaying = currentPlayerObject.getNewHand(currentPlayerObject.hand, nextMove)
      listOfPlayers.update(round.currentPlayerTurn, Player(currentPlayerObject.name, newHandAfterPlaying))

      // Check if playing last move led player to complete
      if(listOfPlayers(round.currentPlayerTurn).status == Complete) {
        println(listOfPlayers(round.currentPlayerTurn).name + " has finished!\n")
        listOfPlayers.remove(round.currentPlayerTurn)
        round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size, round.currentPlayerTurn - 1, listOfPlayers.toList, round.roundPassStatus)
        playerCompletionOrder += currentPlayerObject.name
      }

      /*
       Only change hands if currentState is NON-EMPTY
       If it is EMPTY, it means the currentPLayer gets to go again
       Empty state signifies a BURN has just taken place, the currentPlayer in question does not change
       The only exception here is when the player had finished their hand on a card that led to a BURN
      */
      if(currentState.cards.nonEmpty || round.playerEndedTheGameOnABurn)  {
        if (round.currentPlayerTurn + 1 == listOfPlayers.size)
          round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size, 0, listOfPlayers.toList, round.roundPassStatus)
        else
          round = Round(currentState, round.lastMovePlayedBy, listOfPlayers.size, round.currentPlayerTurn + 1, listOfPlayers.toList, round.roundPassStatus)
      }

      println("------------------------\n")
//            Thread.sleep(10)
    }

    printStats()

  }

  def printStats(): Unit = {
    println("GAME OVER")
    println("---------")
    println(playerCompletionOrder)
  }
}
