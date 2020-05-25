package game

import neural_network.{Subject, Transcriber}
import player.Player
import game.GameUtilities.{getNextGameState, sortCards}
import ui.MainLayout

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class IllegalNumberOfPlayersException(s: String) extends IllegalArgumentException(s)

case object Game {

  val realPlayerName = "YOU"
  val sleepTimeBetweenGames = 5000
  val newCardReceivedTime = 1500
  val sleepTime = 750

  val totalPlayerSizeMap: Map[Int, List[PlayerCompletionStatus]] = Map(
    2 -> List(President, Bum),
    3 -> List(President, Neutral, Bum),
    4 -> List(President, VicePres, ViceBum, Bum),
    5 -> List(President, VicePres, Neutral, ViceBum, Bum),
    6 -> List(President, VicePres, Neutral, Neutral, ViceBum, Bum)
  )

  def getPlayerCompletionMessage(status: PlayerCompletionStatus, totalNumberOfPlayers: Int): String = {
    val numberOfCards = if(totalNumberOfPlayers >= 4) 2 else 1
    status match {
      case President =>
        if(numberOfCards == 1) "Congratulations! You are the new President!\nAs a reward, you will get the Bum's best card.\nAdditionally, you can choose 1 card to give away.\nYou also get to start the next game!"
        else "Congratulations! You are the new President!\nAs a reward, you get the Bum's 2 best cards.\nAdditionally, you can also choose 2 cards to give away.\nYou also get to start the next game!"
      case VicePres => "Congratulations, You are the new VicePresident!\nAs a reward, you will get the Bum's best card.\nAdditionally, you can choose 1 card to give away."
      case Neutral =>  "Good Job, You are Neutral!\nYou will not be involved in any exchange of cards."
      case ViceBum => "Sorry, you are the ViceBum.\nYou will be forced to give up your best card in exchange for the VicePresident's worst."
      case Bum =>
        if(numberOfCards == 1) "Sorry, you are the Bum.\nYou will be forced to give up your best card in exchange for the President's worst card."
        else "Sorry, you are the Bum.\nYou will be forced to give up your 2 best cards in exchange for the President's 2 worst cards."

    }
  }

  def apply(startState: Move, playerNames: List[String], mainLayout: MainLayout): Game = {
    if(playerNames.size < 2 || playerNames.size > 6) throw IllegalNumberOfPlayersException("Need 2-6 players to play the game")
    else {
      val players = GameUtilities.generatePlayersAndDealHands(playerNames)
        .map(player => if(player.name == Game.realPlayerName) player.copy(isRealPlayer = true) else player)
      new Game(startState, players.toBuffer, mainLayout)
    }
  }
}

/**
 * Used for creating datasets
 * @param currentHand - real player's current hand
 * @param gameState - last move played in current round
 * @param movePlayed - move chosen to play by the real player
 */
case class GameData(currentHand: Hand = Hand(List.empty), gameState: Move = Move(List.empty), movePlayed: Option[Move] = Some(Move(List.empty)))

case class Game(startState: Move, var players: mutable.Buffer[Player], mainLayout: MainLayout, var isActive: Boolean = true) extends Subject[Game, GameData]{
  import Game._

  val playerCompletionStatusOrder: List[PlayerCompletionStatus] = totalPlayerSizeMap.getOrElse(players.size, List.empty)

  var previousRoundPlayerCompletionStatuses: List[PlayerCompletionStatus] = players.map(_ => Neutral).toList
  var previousRoundPlayerCompletionOrder: List[String] = players.map(_.name).toList

  var playerCompletionOrder: ListBuffer[String] = new ListBuffer[String]
  var startingPlayerIndex = 0

  // Establish observer pattern relationship
  val transcriber: Transcriber = new Transcriber
  addObserver(transcriber)

  /*
 Simulates multiple runs of the game, until user quits. First game begins with P1 starting
 todo - minimize state changes?
  */
  def play(): Unit = {

    while(true) {
      playerCompletionOrder.clear()
      // This game ends when there's either a bum (graceful exit)
      // Or the game was restarted/hands re-dealt (isActive = false)
      runSingleGame(startingPlayerIndex)

      // This check exists so that BUM does'nt get updated when the user has requested a re-deal/new game
      // This is true if the game has ended gracefully -> aka there is a BUM
      if(isActive) {
        // Involves bookkeeping, UI cleanup
        performGracefulExit()
      }
      else return
    }

  }

  def performGracefulExit() = {
    mainLayout.updateLastRemainingPlayer(players.indexWhere(player => player.status == Active))
    // Update next iteration of game's startingPlayerIndex => President gets to start the next game
    startingPlayerIndex = players.map(_.name).indexOf(playerCompletionOrder.head)
    // Update the round completion status of the previous round = Graceful exit means all positions are filled
    previousRoundPlayerCompletionStatuses = playerCompletionStatusOrder
    previousRoundPlayerCompletionOrder = playerCompletionOrder.toList

    // Print stats to console and show UI dialog
    printStats()

    // Re-deal fresh set of hands and update UI
    players = GameUtilities.generatePlayersAndDealHands(players.map(_.name).toList)
      .map(player => if(player.name == Game.realPlayerName) player.copy(isRealPlayer = true) else player).toBuffer
    updateUI(players)
    // This blocks passage until user dismisses dialog
    mainLayout.showUserPromptForGameCompletionStatus(previousRoundPlayerCompletionOrder, previousRoundPlayerCompletionStatuses)
    // Perform card exchange
    val (newPlayers, realPlayerCardsReceived) = GameUtilities.exchangeHands(players, previousRoundPlayerCompletionOrder, previousRoundPlayerCompletionStatuses, mainLayout.selectedCardsToGetRidOf)
    players = newPlayers
    mainLayout.selectedCardsToGetRidOf = List.empty
    // Perform UI updates
    updateUI(players)
    mainLayout.highlightNewlyReceivedCard(realPlayerCardsReceived)
  }

  def updateUI(players: mutable.Buffer[Player]) = {
    mainLayout.updatePlayerObjects(players.toList)
    mainLayout.updateRoundObject(null)
    mainLayout.resetPlayerCompletionStatus
    mainLayout.resetUserPassStatus
    mainLayout.revalidate()
    mainLayout.repaint()
  }

  def runSingleGame(startingPlayerIndex: Int) = {
    var currentState = this.startState

    var round = Round(currentState, "", startingPlayerIndex, players.toList, Round.getPassStatusFalseForAll(players.toList))

    // Keep the game going, until exactly one player is game.Active (Bum)
    while(players
      .map(player => player.status)
      .map(playerstatus => playerstatus == Active)
      .count(_ == true) > 1 && isActive) {

      mainLayout.updateRoundObject(round)

      // Reset state if everyone has passed
      if(round.hasEveryoneExceptThePlayerWhoPlayedTheLastMovePassed) {
        println("END OF ROUND - CLEARING ROUND CARDS")
        currentState = Move(List.empty)

        // Since everyone else has passed, next one to play is the last one who played
        val nextPlayerIndex =  try {
          // This throws exception if lastMovePlayedBy is by someone who has COMPLETED since
          round.getIndexOf(round.lastMovePlayedBy)
        } catch {
          // If last move is played by someone who doesnt exist anymore
          // Then next person to play is the one player in starting order
          case e: Exception => round.getIndexOfNextPlayer
        }

        // Update round with index of next player and reset pass list and clean round.movesPlayed
        round = Round(currentState, round.lastMovePlayedBy, nextPlayerIndex,
          players.toList, Round.getPassStatusFalseForAll(players.toList))
        mainLayout.updateRoundObject(round)
        mainLayout.resetUserPassStatus
      }

      val currentPlayerObject = players(round.currentPlayerTurn)
      println(currentPlayerObject.name)
      println(Hand(sortCards(currentPlayerObject.hand.listOfCards)))

      mainLayout.updateActivePlayerAvatar
      Thread.sleep(sleepTime)

      try {
        val nextMove: Option[Move] =
        // If player has not skipped turn this round already, then they get to play
          if (!round.hasAlreadySkippedTurn(currentPlayerObject.name)) {
            if (currentPlayerObject.isRealPlayer) mainLayout.getUserInputMove()

            /** This is blocking, waits for user input */
            else currentPlayerObject.playNextMove(currentPlayerObject.hand, currentState)
          }
          else {
            println("PASSED ALREADY")
            mainLayout.updateUserHasPassedOnRound(round.currentPlayerTurn)
            None
          }
        println("The next move is : " + nextMove)

        /**
         * Here, we have info regarding
         *1. The player's hand
         *2. The player's chosen move
         *3. The current gameState that led to player choosing this move
         * We simply transcribe this data into our dataset, iff the player in question is a real player
         */
        if (currentPlayerObject.isRealPlayer) {
          notifyObservers(GameData(currentPlayerObject.hand, currentState, nextMove))
        }

        currentState = getNextGameState(currentState, nextMove)
        // If nextMove is not none, update lastMovePlayedBy since the player is gonna play the move
        if (nextMove.isDefined) {
          round = Round(currentState, currentPlayerObject.name, round.currentPlayerTurn, players.toList, round.roundPassStatus, round.movesPlayed :+ nextMove.get)
        }
        // This means that the user is passing, nextMove is not defined. Update the roundPassStatus list
        else {
          println("PASS")
          val newRoundPassStatus = round.roundPassStatus + (currentPlayerObject.name -> true)
          round = Round(currentState, round.lastMovePlayedBy, round.currentPlayerTurn, players.toList, newRoundPassStatus, round.movesPlayed)
          // Make call to update UI to draw PASS here
          mainLayout.updateUserHasPassedOnRound(round.currentPlayerTurn)
        }
        mainLayout.updateRoundObject(round)

        // Add a timing break here so that visual changes are reflected
        Thread.sleep(sleepTime)

        // Reset roundPassStatus list if currentState has become Empty, and reset roundMovesPlayed as well
        // This can only happen when it is a suit-burn/2-burn/game.Joker/All-pass right now
        if (currentState.isEmpty) {
          round = Round(currentState, round.lastMovePlayedBy, round.currentPlayerTurn, players.toList, Round.getPassStatusFalseForAll(players.toList))
          mainLayout.resetUserPassStatus
        }
        else
          round = Round(currentState, round.lastMovePlayedBy, round.currentPlayerTurn, players.toList, round.roundPassStatus, round.movesPlayed)

        mainLayout.updateRoundObject(round)

        println("The current round state is : " + round.gameState)
        println("The roundMovesPlayed is " + round.movesPlayed)

        val newHandAfterPlaying = GameUtilities.getNewHand(currentPlayerObject.hand, nextMove)
        players.update(round.currentPlayerTurn, currentPlayerObject.copy(hand = newHandAfterPlaying))

        // Check if playing last move led player to complete
        if (players(round.currentPlayerTurn).status == Complete) {
          println(players(round.currentPlayerTurn).name + " has finished!\n")
          round = Round(currentState, round.lastMovePlayedBy, round.currentPlayerTurn,
            players.toList, round.updatedRoundPassStatus(currentPlayerObject.name), round.movesPlayed)
          playerCompletionOrder += currentPlayerObject.name
          mainLayout.updatePlayerCompletion(round.currentPlayerTurn)
        }


        // Updating player object so that the hand is refreshed
        mainLayout.updatePlayerObjects(players.toList)
        // Updating round object so that the gameState is accurately reflected
        mainLayout.updateRoundObject(round)

        /*
         Only change player turn if currentState is NON-EMPTY
         If it is EMPTY, it means the currentPLayer gets to go again
         Empty state signifies a BURN has just taken place, the currentPlayer in question does not change
         The only exception here is when the player had finished their hand on a card that led to a BURN
        */
        if (currentState.cards.nonEmpty || round.playerEndedTheGameOnABurn) {
          round = Round(currentState, round.lastMovePlayedBy, round.getNextActivePlayerInSequence(round.currentPlayerTurn),
            players.toList, round.roundPassStatus, round.movesPlayed)
        }

        println("------------------------\n")
        Thread.sleep(sleepTime)
      }
      catch {
        case e: InactiveGameException => /** Continue without any updates, as game is inactive. Loop invariant will ensure loop termination */
      }

    }
    // Add in the last player in the completion list
    playerCompletionOrder += players(round.currentPlayerTurn).name
  }

  def printStats(): Unit = {
    println("---------")
    println("GAME OVER")
    println("---------")
    println(playerCompletionOrder)
    mainLayout.printStats(playerCompletionOrder.toList)
  }
}

case class InactiveGameException(s: String) extends IllegalStateException(s)
