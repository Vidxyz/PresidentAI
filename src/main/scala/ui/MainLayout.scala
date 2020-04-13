package ui

import game.{Move, Round}
import player.Player
import ui.layouts.{BottomLayout, MiddleLayout, TopLayout}
import scala.swing.{GridBagPanel, SimpleSwingApplication}

class MainLayout(app: SimpleSwingApplication, players: List[Player]) extends GridBagPanel {

  val topPanel = new TopLayout(app, players.size >= 3, players.size >= 4, players.size >= 5, null)
  val middlePanel = new MiddleLayout(app, true, players.size == 6, null)
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

  def updateRealPlayerObject(realPlayer: Player) = {
    bottomPanel.updateRealPlayerObject(realPlayer)
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

  def displayUserHasPassedOnRound(indexOfPassedPlayer: Int) = {
    topPanel.displayUserHasPassedOnRound(indexOfPassedPlayer)
    middlePanel.displayUserHasPassedOnRound(indexOfPassedPlayer)
    bottomPanel.displayUserHasPassedOnRound(indexOfPassedPlayer)
  }

}
