package ui.layouts

import game.{Card, Round}
import player.Player
import ui.panels.{ComputerPlayerAvatarPanel, CurrentRoundPanel}

import scala.swing.{GridBagPanel, SimpleSwingApplication}

class MiddleLayout(app: SimpleSwingApplication, player2Hand: List[Card], player6Hand: List[Card], var round: Round) extends GridBagPanel {

  val player2AvatarPanel = new ComputerPlayerAvatarPanel(app, player2Hand)
  val currentRoundPanel = new CurrentRoundPanel(app, round)
  val player6AvatarPanel = new ComputerPlayerAvatarPanel(app, player6Hand)

  val c: Constraints = new Constraints()

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 0
  c.gridy = 0
  layout(player2AvatarPanel) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 1
  c.gridy = 0
  layout(currentRoundPanel) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 2
  c.gridy = 0
  layout(player6AvatarPanel) = c

  def updateRoundObject(newRound: Round) = {
    this.round = newRound
    currentRoundPanel.updateRoundObject(newRound)
    revalidate()
    repaint()
  }

  def updateActivePlayerAvatar() = {
    round.currentPlayerTurn match {
      case 1 =>
        player2AvatarPanel.setPlayerAvatarStatus(true)
        player6AvatarPanel.setPlayerAvatarStatus(false)
      case 5 =>
        player2AvatarPanel.setPlayerAvatarStatus(false)
        player6AvatarPanel.setPlayerAvatarStatus(true)
      case _ =>
        player2AvatarPanel.setPlayerAvatarStatus(false)
        player6AvatarPanel.setPlayerAvatarStatus(false)
    }
    revalidate()
    repaint()
  }

  def updatePlayerCompletion(indexOfCompletedPlayer: Int) = {
    indexOfCompletedPlayer match {
      case 1 => player2AvatarPanel.setPlayerAvatarToComplete()
      case 5 => player6AvatarPanel.setPlayerAvatarToComplete()
      case _ =>
    }
    revalidate()
    repaint()
  }

  def displayUserHasPassedOnRound(indexOfPassedPlayer: Int) = {
    indexOfPassedPlayer match {
      case 1 => player2AvatarPanel.displayUserHasPassedOnRound()
      case 5 => player6AvatarPanel.displayUserHasPassedOnRound()
      case _ =>
    }
  }

  def updatePlayers(players: List[Player]) = {
    if(players.size >= 2) player2AvatarPanel.updatePlayerObject(players(1))
    if(players.size >= 6) player6AvatarPanel.updatePlayerObject(players(5))
  }

}
