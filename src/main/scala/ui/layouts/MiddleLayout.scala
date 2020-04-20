package ui.layouts

import game.{Card, Round}
import player.Player
import ui.panels.{ComputerPlayerAvatarPanel, CurrentRoundPanel, EmptyFillerPanel}

import scala.swing.{GridBagPanel, SimpleSwingApplication}

class MiddleLayout(app: SimpleSwingApplication, player2: Player, player6: Player, var round: Round) extends GridBagPanel {

  val filler1 = new EmptyFillerPanel
  val player2AvatarPanel = new ComputerPlayerAvatarPanel(app, if(player2 == null) "" else player2.name,
    if(player2 == null) List.empty else player2.hand.listOfCards)
  val currentRoundPanel = new CurrentRoundPanel(app, round)
  val filler2 = new EmptyFillerPanel
  val player6AvatarPanel = new ComputerPlayerAvatarPanel(app, if(player6 == null) "" else  player6.name,
    if(player6 == null) List.empty else player6.hand.listOfCards)

  val c: Constraints = new Constraints()


  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridwidth = 1
  c.gridheight = 1
  c.gridx = 0
  c.gridy = 0
  layout(filler1) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridwidth = 1
  c.gridheight = 1
  c.gridx = 0
  c.gridy = 1
  layout(player2AvatarPanel) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridwidth = 1
  c.gridheight = 2
  c.gridx = 1
  c.gridy = 0
  layout(currentRoundPanel) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridwidth = 1
  c.gridheight = 1
  c.gridx = 2
  c.gridy = 0
  layout(filler2) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridwidth = 1
  c.gridheight = 1
  c.gridx = 2
  c.gridy = 1
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
      case 1 => player2AvatarPanel.setPlayerAvatarToComplete
      case 5 => player6AvatarPanel.setPlayerAvatarToComplete
      case _ =>
    }
    revalidate()
    repaint()
  }

  def updateUserHasPassedOnRound(indexOfPassedPlayer: Int) = {
    indexOfPassedPlayer match {
      case 1 => player2AvatarPanel.updateUserHasPassedOnRound
      case 5 => player6AvatarPanel.updateUserHasPassedOnRound
      case _ =>
    }
    revalidate()
    repaint()
  }

  def resetUserPassStatus = {
    player2AvatarPanel.resetUserPassStatus
    player6AvatarPanel.resetUserPassStatus
    revalidate()
    repaint()
  }

  def updatePlayerHands(players: List[Player]) = {
    if(players.size >= 2) player2AvatarPanel.updatePlayerHand(players(1).hand.listOfCards) else player2AvatarPanel.updatePlayerHand(List.empty)
    if(players.size >= 6) player6AvatarPanel.updatePlayerHand(players(5).hand.listOfCards) else player6AvatarPanel.updatePlayerHand(List.empty)
  }

  def updateLastRemainingPlayer(indexOfLastPlayer: Int) = {
    indexOfLastPlayer match {
      case 1 => player2AvatarPanel.setPlayerAvatarToBum
      case 5 => player6AvatarPanel.setPlayerAvatarToBum
      case _ =>
    }
    revalidate()
    repaint()
  }

  def resetPlayerCompletionStatus: Unit = {
    player2AvatarPanel.resetUserCompletionStatus
    player6AvatarPanel.resetUserCompletionStatus
    revalidate()
    repaint()
  }

}
