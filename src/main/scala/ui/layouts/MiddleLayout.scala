package ui.layouts

import game.Round
import ui.panels.{ComputerPlayerAvatarPanel, CurrentRoundPanel}
import scala.swing.{GridBagPanel, SimpleSwingApplication}

class MiddleLayout(app: SimpleSwingApplication, isPlayer2InGame: Boolean, isPlayer6InGame: Boolean, round: Round) extends GridBagPanel {

  val player2AvatarPanel = new ComputerPlayerAvatarPanel(app, isPlayer2InGame)
  val currentRoundPanel = new CurrentRoundPanel(app, round)
  val player6AvatarPanel = new ComputerPlayerAvatarPanel(app, isPlayer6InGame)

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

  def updateRoundObject(round: Round) = {
    currentRoundPanel.updateRoundObject(round)
    revalidate()
    repaint()
  }

}
