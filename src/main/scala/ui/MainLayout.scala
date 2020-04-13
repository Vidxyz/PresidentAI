package ui

import game.{Move, Round}
import player.Player
import ui.layouts.{BottomLayout, MiddleLayout, TopLayout}
import scala.swing.{GridBagPanel, SimpleSwingApplication}

class MainLayout(app: SimpleSwingApplication, players: List[Player]) extends GridBagPanel {

  val topPanel = new TopLayout(app, if(players.size >= 3) players(2).hand.listOfCards else List.empty,
    if(players.size >= 4) players(3).hand.listOfCards else List.empty,
    if(players.size >= 5) players(4).hand.listOfCards else List.empty, null)
  val middlePanel = new MiddleLayout(app, players(1).hand.listOfCards, if(players.size == 6) players.last.hand.listOfCards else List.empty, null)
  val bottomPanel = new BottomLayout(app, players.head, null)

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
    topPanel.updatePlayers(players)
    middlePanel.updatePlayers(players)
    bottomPanel.updateRealPlayerObject(players.head)
    revalidate()
    repaint()
  }

  def updateActivePlayerAvatar() = {
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

}
