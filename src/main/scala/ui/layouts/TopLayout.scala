package ui.layouts

import ui.panels.{ComputerPlayerAvatarPanel, EmptyFillerPanel}
import scala.swing.{GridBagPanel, SimpleSwingApplication}

class TopLayout(app: SimpleSwingApplication, isPlayer3InGame: Boolean,
                isPlayer4InGame: Boolean, isPlayer5InGame: Boolean) extends GridBagPanel {


  val player3AvatarPanel = new ComputerPlayerAvatarPanel(app, isPlayer3InGame)
  val filler1 = new EmptyFillerPanel
  val player4AvatarPanel = new ComputerPlayerAvatarPanel(app, isPlayer4InGame)
  val filler2 = new EmptyFillerPanel
  val player5AvatarPanel = new ComputerPlayerAvatarPanel(app, isPlayer5InGame)

  val c: Constraints = new Constraints()

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 0
  c.gridy = 0
  layout(player3AvatarPanel) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 1
  c.gridy = 0
  layout(filler1) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 2
  c.gridy = 0
  layout(player4AvatarPanel) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 3
  c.gridy = 0
  layout(filler2) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 4
  c.gridy = 0
  layout(player5AvatarPanel) = c


}
