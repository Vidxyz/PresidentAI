package game

import player.Player
import game.GameUtilities.{getNextGameState, sortCards}
import ui.MainLayout

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class IllegalNumberOfPlayersException(s: String) extends IllegalArgumentException(s)

case object Game {

  val realPlayerName = "YOU"
  val sleepTimeBetweenGames = 5000
  val sleepTime = 200

  val totalPlayerSizeMap: Map[Int, List[PlayerCompletionStatus]] = Map(
    2 -> List(President, Bum),
    3 -> List(President, Neutral, Bum),
    4 -> List(President, VicePres, ViceBum, Bum),
    5 -> List(President, VicePres, Neutral, ViceBum, Bum),
    6 -> List(President, VicePres, Neutral, Neutral, ViceBum, Bum)
  )

  // Todo - ensure correct number of cards are shown on text - sometimes you lose/gain 2 cards, and other times, just the 1
  val playerCompletionMessages: Map[PlayerCompletionStatus, String] = Map(
    President -> "Congratulations! You are the new President! As a reward, two of your worst cards will be replaced with the Bum's best cards. \n You also get to start the next game!",
    VicePres -> "Congratulations, You are the new VicePresident! As a rewards, your worst card will be replaced with the ViceBum's bext card.",
    Neutral -> "Congratulations, You are Neutral. You will not be involved in any exchange of cards.",
    ViceBum -> "Sorry, you are the ViceBum. You will be forced to give up your best card in exchange for the VicePresident's worst.",
    Bum -> "Sorry, you are the Bum. You will be forced to give up two of your best cards in exchange for 2 of the President's worst cards."
  )


  def getPlayerCompletionMessage(status: PlayerCompletionStatus, totalNumberOfPlayers: Int): String = {
    val numberOfCards = if(totalNumberOfPlayers >= 4) 2 else 1
    status match {
      case President => {
        if(numberOfCards == 1) "Congratulations! You are the new President! As a reward, your worst card will be replaced with the Bum's best card. \n You also get to start the next game!"
        else "Congratulations! You are the new President! As a reward, two of your worst cards will be replaced with the Bum's best cards. \n You also get to start the next game!"
      }
      case VicePres => "Congratulations, You are the new VicePresident! As a rewards, your worst card will be replaced with the ViceBum's bext card."
      case Neutral =>  "Congratulations, You are Neutral. You will not be involved in any exchange of cards."
      case ViceBum => "Sorry, you are the ViceBum. You will be forced to give up your best card in exchange for the VicePresident's worst."
      case Bum => {
        if(numberOfCards == 1) "Sorry, you are the Bum. You will be forced to give up your best card in exchange for the President's worst card."
        else "Sorry, you are the Bum. You will be forced to give up your 2 best cards in exchange for the President's 2 worst cards."

      }
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

case class Game(startState: Move, var players: mutable.Buffer[Player], mainLayout: MainLayout, var isActive: Boolean = true) {
  import Game._
  /*
  Keeps a completion order of the form (playerName, roundEnded)
   */
  val playerCompletionStatusOrder: List[PlayerCompletionStatus] = totalPlayerSizeMap.getOrElse(players.size, List.empty)
  var playerCompletionOrder: ListBuffer[String] = new ListBuffer[String]
  var startingPlayerIndex = 0

  /*
 Simulates multiple runs of the game, until user quits. First game begins with P1 starting
  */
  def play(): Unit = {

    while(true) {
      playerCompletionOrder.clear()
      runSingleGame(startingPlayerIndex)

      // This check exists so that BUM doesnt get updated when the user has requested a re-deal/new game
      // This is true if the game has ended gracefully -> aka there is a BUM
      if(isActive) {
        mainLayout.updateLastRemainingPlayer(players.indexWhere(player => player.status == Active))
        // Update next iteration of game's startingPlayerIndex => President gets to start the next game
        startingPlayerIndex = players.map(_.name).indexOf(playerCompletionOrder.head)
        // Print stats to console and show UI dialog
        printStats()
        // Re-deal fresh set of hands and update UI
        players = GameUtilities.generatePlayersAndDealHands(players.map(_.name).toList)
          .map(player => if(player.name == Game.realPlayerName) player.copy(isRealPlayer = true) else player).toBuffer
        updateUI(players)
        // This blocks passage until user dismisses dialog
        mainLayout.showUserPromptForGameCompletionStatus(playerCompletionOrder.toList, playerCompletionStatusOrder)
        players = exchangeHands(players, playerCompletionOrder.toList)
        updateUI(players)
      }
      else return
    }

  }

  /*
  Exchanges hands with president-bum, vp-vb
  Neutral hand is untouched
  Assumes playerCompletionOrder.size == playerCompletionStatusOrder.size
  Assumes newPlayers.names == player names in completion order
  // todo - test this code and write tests
   */
  def exchangeHands(newPlayers: mutable.Buffer[Player], playerCompletionOrder: List[String]): mutable.Buffer[Player] = {
    val totalCardsToDrop = if(newPlayers.size >= 4) 2 else 1
    val droppedCards: mutable.Map[PlayerCompletionStatus, List[Card]] = collection.mutable.Map.empty
    val completionMap: Map[String, PlayerCompletionStatus] =  playerCompletionOrder.zip(playerCompletionStatusOrder).toMap

    newPlayers
      .map(p => (p, completionMap.getOrElse(p.name, Neutral)))
      .foreach({
        case (player, President) => droppedCards(President) = player.getWorstCards(totalCardsToDrop)
        case (player, VicePres) => droppedCards(VicePres) = player.getWorstCards(1)
        case (player, ViceBum) => droppedCards(ViceBum) = player.getBestCards(1)
        case (player, Bum) => droppedCards(Bum) = player.getBestCards(totalCardsToDrop)
        case (player, Neutral) =>
      })

    newPlayers
      .map(p => (p, completionMap.getOrElse(p.name, Neutral)))
      .map({
        case (player, President) => player.copy(hand = GameUtilities.dropAndReplaceCardsInHand(player.hand,
                    droppedCards.getOrElse(President, List.empty), droppedCards.getOrElse(Bum, List.empty)))
        case (player, VicePres) => player.copy(hand = GameUtilities.dropAndReplaceCardsInHand(player.hand,
          droppedCards.getOrElse(VicePres, List.empty), droppedCards.getOrElse(ViceBum, List.empty)))
        case (player, ViceBum) => player.copy(hand = GameUtilities.dropAndReplaceCardsInHand(player.hand,
          droppedCards.getOrElse(ViceBum, List.empty), droppedCards.getOrElse(VicePres, List.empty)))
        case (player, Bum) => player.copy(hand = GameUtilities.dropAndReplaceCardsInHand(player.hand,
          droppedCards.getOrElse(Bum, List.empty), droppedCards.getOrElse(President, List.empty)))
        case (player, Neutral) => player
        })
      .map(player => if(player.name == Game.realPlayerName) player.copy(isRealPlayer = true) else player)

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

      val nextMove: Option[Move] =
      // If player has not skipped turn this round already, then they get to play
        if(!round.hasAlreadySkippedTurn(currentPlayerObject.name)) {
          if(currentPlayerObject.isRealPlayer) mainLayout.getUserInputMove()
          else currentPlayerObject.playNextMove(currentPlayerObject.hand, currentState)
        }
        else {
          println("PASSED ALREADY")
          mainLayout.updateUserHasPassedOnRound(round.currentPlayerTurn)
          None
        }
      println("The next move is : " + nextMove)

      currentState = getNextGameState(currentState, nextMove)
      // If nextMove is not none, update lastMovePlayedBy since the player is gonna play the move
      if(nextMove.isDefined){
        round = Round(currentState, currentPlayerObject.name, round.currentPlayerTurn, players.toList, round.roundPassStatus, round.movesPlayed  :+ nextMove.get)
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
      if(currentState.isEmpty) {
        round = Round(currentState, round.lastMovePlayedBy, round.currentPlayerTurn, players.toList, Round.getPassStatusFalseForAll(players.toList))
        mainLayout.resetUserPassStatus
      }
      else
        round = Round(currentState, round.lastMovePlayedBy, round.currentPlayerTurn, players.toList, round.roundPassStatus, round.movesPlayed)

      mainLayout.updateRoundObject(round)

      println("The current round state is : " + round.gameState)
      println("The roundMovesPlayed is " + round.movesPlayed)
      //    println("The pass status is : " + round.roundPassStatus)

      val newHandAfterPlaying = GameUtilities.getNewHand(currentPlayerObject.hand, nextMove)
      players.update(round.currentPlayerTurn, currentPlayerObject.copy(hand = newHandAfterPlaying))

      // Check if playing last move led player to complete
      if(players(round.currentPlayerTurn).status == Complete) {
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
      if(currentState.cards.nonEmpty || round.playerEndedTheGameOnABurn)  {
        round = Round(currentState, round.lastMovePlayedBy, round.getNextActivePlayerInSequence(round.currentPlayerTurn),
          players.toList, round.roundPassStatus, round.movesPlayed)
      }

      println("------------------------\n")
      Thread.sleep(sleepTime)

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
