package ui.layouts

import player.Player
import ui.panels.{ComputerPlayerAvatarPanel, EmptyFillerPanel}

import scala.swing.{GridBagPanel, SimpleSwingApplication}

class TopLayout(app: SimpleSwingApplication, players: List[Player]) extends GridBagPanel {


  val computer1 = new ComputerPlayerAvatarPanel(app)
  val filler1 = new EmptyFillerPanel
  val computer2 = new ComputerPlayerAvatarPanel(app)
  val filler2 = new EmptyFillerPanel
  val computer3 = new ComputerPlayerAvatarPanel(app)

  val c: Constraints = new Constraints()

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 0
  c.gridy = 0
  layout(computer1) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 1
  c.gridy = 0
  layout(filler1) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 2
  c.gridy = 0
  layout(computer2) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 3
  c.gridy = 0
  layout(filler2) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 4
  c.gridy = 0
  layout(computer3) = c


}
