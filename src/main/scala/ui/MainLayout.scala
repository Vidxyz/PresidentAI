package ui

import game.{Game, GameUtilities, Move, Round}
import player.Player
import ui.layouts.{BottomLayout, MiddleLayout, TopLayout}

import scala.swing.{GridBagPanel, SimpleSwingApplication, Swing}
import scala.swing.Dialog._

class MainLayout(app: SimpleSwingApplication) extends GridBagPanel {

  val playerNames = List("Real", "Bob", "Mike", "Joe", "Kevin", "Andrew")
  val players = GameUtilities.generatePlayersAndDealHands(playerNames)
    .map(player => if(player.name == "Real") player.copy(isRealPlayer = true) else player)

  val topPanel = new TopLayout(app,
    if(players.size >= 3) players(2).hand.listOfCards else List.empty,
    if(players.size >= 4) players(3).hand.listOfCards else List.empty,
    if(players.size >= 5) players(4).hand.listOfCards else List.empty, null)
  val middlePanel = new MiddleLayout(app, players(1).hand.listOfCards,
    if(players.size == 6) players.last.hand.listOfCards else List.empty, null)
  val bottomPanel = new BottomLayout(app, this, players.head, null)


  var game: Game = Game(Move(List.empty), players.toBuffer, this)
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

  def promptDialogForNewGame = {
    game.isActive = false

    val choices = List("2", "3", "4", "5", "6")
    val userChoice = showInput(this, "Select total number of players in game", "Game Settings", Message.Plain, Swing.EmptyIcon, choices, choices.head)
    val selectedChoice = userChoice.getOrElse(choices.head).toInt
    val newPlayers = playerNames.take(selectedChoice)
    val newListOfPlayers = GameUtilities.generatePlayersAndDealHands(newPlayers).map(player => if(player.name == "Real") player.copy(isRealPlayer = true) else player)

    game = Game(Move(List.empty), newListOfPlayers.toBuffer, this)

    updatePlayerObjects(newListOfPlayers)
    updateRoundObject(null)
    revalidate()
    repaint()

    beginGame

  }

  def reDealHandsForThisGame = {

  }

  def beginGame = {
    val thread = new Thread {
      override def run {
        println("Starting a new game")
        game.play()
      }
    }
    thread.start()
  }

}
