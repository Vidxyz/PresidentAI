package ui.layouts

import game.Round
import player.Player
import ui.panels.{ComputerPlayerAvatarPanel, CurrentRoundPanel}

import scala.swing.{GridBagPanel, SimpleSwingApplication}

class MiddleLayout(app: SimpleSwingApplication, player1: Player, player2: Player, round: Round) extends GridBagPanel {

  val player1AvatarPanel = new ComputerPlayerAvatarPanel(app)
  val currentRoundPanel = new CurrentRoundPanel(app, round)
  val player2AvatarPanel = new ComputerPlayerAvatarPanel(app)

  val c: Constraints = new Constraints()

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 0
  c.gridy = 0
  layout(player1AvatarPanel) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 1
  c.gridy = 0
  layout(currentRoundPanel) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 2
  c.gridy = 0
  layout(player2AvatarPanel) = c

}
