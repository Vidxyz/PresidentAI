package ui.layouts

import game.Round
import ui.panels.{ComputerPlayerAvatarPanel, EmptyFillerPanel}

import scala.swing.{GridBagPanel, SimpleSwingApplication}

class TopLayout(app: SimpleSwingApplication, isPlayer3InGame: Boolean,
                isPlayer4InGame: Boolean, isPlayer5InGame: Boolean, var round: Round) extends GridBagPanel {


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


  def updateActivePlayerAvatar() = {
    round.currentPlayerTurn match {
      case 2 =>
        player3AvatarPanel.setPlayerAvatarStatus(true)
        player4AvatarPanel.setPlayerAvatarStatus(false)
        player5AvatarPanel.setPlayerAvatarStatus(false)
      case 3 =>
        player3AvatarPanel.setPlayerAvatarStatus(false)
        player4AvatarPanel.setPlayerAvatarStatus(true)
        player5AvatarPanel.setPlayerAvatarStatus(false)
      case 4 =>
        player3AvatarPanel.setPlayerAvatarStatus(false)
        player4AvatarPanel.setPlayerAvatarStatus(false)
        player5AvatarPanel.setPlayerAvatarStatus(true)
      case _ =>
        player3AvatarPanel.setPlayerAvatarStatus(false)
        player4AvatarPanel.setPlayerAvatarStatus(false)
        player5AvatarPanel.setPlayerAvatarStatus(false)
    }
    revalidate()
    repaint()
  }

  def updateRoundObject(newRound: Round): Unit = {
    this.round = newRound
  }

}
