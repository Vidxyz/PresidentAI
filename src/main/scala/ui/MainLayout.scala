package ui

import player.Player
import ui.layouts.{BottomLayout, MiddleLayout, TopLayout}

import scala.swing.{GridBagPanel, SimpleSwingApplication}

class MainLayout(app: SimpleSwingApplication, players: List[Player]) extends GridBagPanel {

  val topPanel = new TopLayout(app, players)
  val middlePanel = new MiddleLayout(app, players(1), players(1), null)
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

}
