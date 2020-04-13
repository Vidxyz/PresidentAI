package ui.layouts

import game.{Card, Round}
import player.Player
import ui.panels.{ComputerPlayerAvatarPanel, EmptyFillerPanel}

import scala.swing.{GridBagPanel, SimpleSwingApplication}

class TopLayout(app: SimpleSwingApplication, player3Hand: List[Card],
                player4Hand: List[Card], player5Hand: List[Card], var round: Round) extends GridBagPanel {


  val player3AvatarPanel = new ComputerPlayerAvatarPanel(app, player3Hand)
  val filler1 = new EmptyFillerPanel
  val player4AvatarPanel = new ComputerPlayerAvatarPanel(app, player4Hand)
  val filler2 = new EmptyFillerPanel
  val player5AvatarPanel = new ComputerPlayerAvatarPanel(app, player5Hand)

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

  def updatePlayerCompletion(indexOfCompletedPlayer: Int) = {
    indexOfCompletedPlayer match {
      case 2 => player3AvatarPanel.setPlayerAvatarToComplete()
      case 3 => player4AvatarPanel.setPlayerAvatarToComplete()
      case 4 => player5AvatarPanel.setPlayerAvatarToComplete()
      case _ =>
    }
    revalidate()
    repaint()
  }

  def updateRoundObject(newRound: Round): Unit = {
    this.round = newRound
  }

  def displayUserHasPassedOnRound(indexOfPassedPlayer: Int) = {
    indexOfPassedPlayer match {
      case 2 => player3AvatarPanel.displayUserHasPassedOnRound()
      case 3 => player4AvatarPanel.displayUserHasPassedOnRound()
      case 5 => player5AvatarPanel.displayUserHasPassedOnRound()
      case _ =>
    }
  }

  def updatePlayers(players: List[Player]) = {
    if(players.size >= 3) player3AvatarPanel.updatePlayerObject(players(2))
    if(players.size >= 4) player4AvatarPanel.updatePlayerObject(players(3))
    if(players.size >= 5) player5AvatarPanel.updatePlayerObject(players(4))
  }

}
