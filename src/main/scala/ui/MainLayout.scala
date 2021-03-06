package ui

import game.{Card, Game, GameUtilities, Move, PlayerCompletionStatus, President, Round, VicePres}
import javax.swing.SwingUtilities
import player.Player
import ui.layouts.{BottomLayout, MiddleLayout, TopLayout, UserPromptDialogLayout}
import ui.panels.GameOverPanel

import scala.swing.{Dialog, GridBagPanel, SimpleSwingApplication, Swing}
import scala.swing.Dialog._

class MainLayout(app: SimpleSwingApplication) extends GridBagPanel {

  var gameThread: Thread = _
  val playerNames = List(Game.realPlayerName, "Bob", "Mike", "Joe", "Kevin", "Andre")
  var selectedPlayerNames = List(Game.realPlayerName, "Bob", "Mike", "Joe", "Kevin", "Andre")

  var game: Game = Game(Move(List.empty), selectedPlayerNames, this)

  var selectedCardsToGetRidOf: List[Card] = List.empty

  val topPanel = new TopLayout(app,
    if(game.players.size >= 3) game.players(2) else null,
    if(game.players.size >= 4) game.players(3) else null,
    if(game.players.size >= 5) game.players(4) else null, null)

  val middlePanel = new MiddleLayout(app, game.players(1),
    if(game.players.size == 6) game.players.last else null, null)

  val bottomPanel = new BottomLayout(app, this, game.players.head, null)



  println("The starting state is : " + game.startState)
  println("\n")
  beginGame

  val c: Constraints = new Constraints()

  c.fill = GridBagPanel.Fill.Vertical
  c.weightx = 1
  c.gridx = 0
  c.gridy = 0
  layout(topPanel) = c

  c.fill = GridBagPanel.Fill.Vertical
  c.weightx = 1
  c.gridx = 0
  c.gridy = 1
  layout(middlePanel) = c

  c.fill = GridBagPanel.Fill.Vertical
  c.weightx = 1
  c.gridx = 0
  c.gridy = 2
  layout(bottomPanel) = c


  def getUserInputMove(): Option[Move] = {
    bottomPanel.getUserInputMove()
  }

  // Note - Top and Middle Panels only update hands, not names. Potential source of bug later
  def updatePlayerObjects(players: List[Player]) = {
    topPanel.updatePlayerHands(players)
    middlePanel.updatePlayerHands(players)
    bottomPanel.updateRealPlayerObject(players.head)
    revalidate()
    repaint()
  }

  def updateActivePlayerAvatar = {
    bottomPanel.updateActivePlayerAvatar()
    middlePanel.updateActivePlayerAvatar()
    topPanel.updateActivePlayerAvatar()
    revalidate()
    repaint()
  }

  def updateRoundObject(round: Round) = {
    topPanel.updateRoundObject(round)
    middlePanel.updateRoundObject(round)
    bottomPanel.updateRoundObject(round)
    revalidate()
    repaint()
  }

  def updatePlayerCompletion(indexOfCompletedPlayer: Int) = {
    topPanel.updatePlayerCompletion(indexOfCompletedPlayer)
    middlePanel.updatePlayerCompletion(indexOfCompletedPlayer)
    bottomPanel.updatePlayerCompletion(indexOfCompletedPlayer)
    revalidate()
    repaint()
  }

  def updateUserHasPassedOnRound(indexOfPassedPlayer: Int) = {
    topPanel.updateUserHasPassedOnRound(indexOfPassedPlayer)
    middlePanel.updateUserHasPassedOnRound(indexOfPassedPlayer)
    bottomPanel.updateUserHasPassedOnRound(indexOfPassedPlayer)
  }

  def resetUserPassStatus = {
    topPanel.resetUserPassStatus
    middlePanel.resetUserPassStatus
    bottomPanel.resetUserPassStatus
    revalidate()
    repaint()
  }

  def resetPlayerCompletionStatus = {
    topPanel.resetPlayerCompletionStatus
    middlePanel.resetPlayerCompletionStatus
    bottomPanel.resetPlayerCompletionStatus
    revalidate()
    repaint()
  }

  def promptDialogForNewGame = {
    game.isActive = false

    val choices = List("2", "3", "4", "5", "6")
    val userChoice = showInput(this, "Select total number of players in game", "Game Settings", Message.Plain, Swing.EmptyIcon, choices, choices.head)
    val selectedChoice = userChoice.getOrElse(choices.head).toInt
    selectedPlayerNames = playerNames.take(selectedChoice)

    gameThread.join()
    game = Game(Move(List.empty), selectedPlayerNames, this)

    updatePlayerObjects(game.players.toList)
    updateRoundObject(null)
    resetPlayerCompletionStatus
    resetUserPassStatus
    revalidate()
    repaint()

    beginGame

  }

  // Difference between new game and re-dealing hands is that state is saved
  // Saved state for redealing hands include startIndex (may or may not be 0, based on if one game has already competed)
  // While the game initially starts with index 0, once one game ends, the winner of that game starts next
  // startIndex could thus be not 0 in these situations
  def reDealHandsForThisGame = {
    game.isActive = false

    gameThread.join()
    game.players = GameUtilities.generatePlayersAndDealHands(selectedPlayerNames)
                  .map(player => if(player.name == Game.realPlayerName) player.copy(isRealPlayer = true) else player).toBuffer
    updateUI()

    showUserPromptForGameCompletionStatus(game.previousRoundPlayerCompletionOrder, game.previousRoundPlayerCompletionStatuses)
    val (newPlayers, realPlayerCardsReceived) = GameUtilities.exchangeHands(game.players, game.previousRoundPlayerCompletionOrder, game.previousRoundPlayerCompletionStatuses, selectedCardsToGetRidOf)
    game.players = newPlayers
    selectedCardsToGetRidOf = List.empty
    game.isActive = true

    updateUI()
    updateActivePlayerAvatar
    highlightNewlyReceivedCard(realPlayerCardsReceived)
    revalidate()
    repaint()

    beginGame
  }

  def updateUI() = {
    val freshRound = Round(game.startState, "", game.startingPlayerIndex, game.players.toList, Round.getPassStatusFalseForAll(game.players.toList))
    updatePlayerObjects(game.players.toList)
    updateRoundObject(freshRound)
    resetPlayerCompletionStatus
    resetUserPassStatus
    revalidate()
    repaint()
  }

  def beginGame = {
    gameThread = new Thread {
      override def run {
        println("Starting a new game")
        game.play()
        println("Game thread is over!")
      }
    }
    gameThread.start()
  }

  def updateLastRemainingPlayer(indexOfLastRemainingPlayer: Int) = {
    topPanel.updateLastRemainingPlayer(indexOfLastRemainingPlayer)
    middlePanel.updateLastRemainingPlayer(indexOfLastRemainingPlayer)
    bottomPanel.updateLastRemainingPlayer(indexOfLastRemainingPlayer)
  }

  def isGameActive: Boolean = game.isActive

  def printStats(playerCompletionOrder: List[String]) = {
    val dialog = new Dialog()
    dialog.contents = new GameOverPanel(playerCompletionOrder)
    dialog.resizable = false
    dialog.centerOnScreen()
    dialog.open()
    Thread.sleep(Game.sleepTimeBetweenGames)
    dialog.close()
  }

  def showUserPromptForGameCompletionStatus(playerCompletionOrder: List[String], playerCompletionStatusOrder: List[PlayerCompletionStatus]): Unit = {
    val playerPosition = playerCompletionOrder.zip(playerCompletionStatusOrder)
      .filter(tuple => tuple._1 == game.players.filter(_.isRealPlayer).head.name)
      .map(_._2).head
    val completionMessage = Game.getPlayerCompletionMessage(playerPosition, playerCompletionOrder.size)
    playerPosition match {
      case President | VicePres => showUserDialogToPromptCardsToGetRidOf(playerPosition, completionMessage, playerCompletionOrder.size)
      case _ =>  showMessage(this, completionMessage, playerPosition.toString)
    }
  }

  def showUserDialogToPromptCardsToGetRidOf(playerPosition: PlayerCompletionStatus, completionMessage: String, numberOfPlayers: Int) = {
    val dialog = new Dialog()
    val currentHand = game.players.filter(_.isRealPlayer).head.hand
    println(currentHand)
    val userPromptDialogLayout = new UserPromptDialogLayout(app, currentHand, dialog, playerPosition, completionMessage, numberOfPlayers)
    dialog.contents = userPromptDialogLayout
    dialog.resizable = false
    dialog.centerOnScreen()
    dialog.modal = true
    dialog.open()
    // This is to busy wait until user closes the dialog, which is done via mouse click listener
    while(dialog.peer.isVisible) {
      Thread.sleep(100)
    }
    selectedCardsToGetRidOf = userPromptDialogLayout.selectedCards
    dialog.dispose()
  }

  // todo - BUG, when re-dealt, things are not being highlighted
  def highlightNewlyReceivedCard(received: List[Card]) = {
    bottomPanel.highlightNewlyReceivedCard(received)
    Thread.sleep(Game.newCardReceivedTime)
    // Hacky way of getting to reset card selection
    bottomPanel.highlightNewlyReceivedCard(List.empty)
  }

}
