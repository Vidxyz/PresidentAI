package ui.layouts

import game.{Card, Round}
import player.Player
import ui.panels.{ComputerPlayerAvatarPanel, EmptyFillerPanel}

import scala.swing.{GridBagPanel, SimpleSwingApplication}

class TopLayout(app: SimpleSwingApplication, player3: Player,
                player4: Player, player5: Player, var round: Round) extends GridBagPanel {


  val player3AvatarPanel = new ComputerPlayerAvatarPanel(app, if(player3 == null) "" else player3.name,
    if(player3 == null) List.empty else player3.hand.listOfCards)
  val filler1 = new EmptyFillerPanel
  val filler12 = new EmptyFillerPanel
  val player4AvatarPanel = new ComputerPlayerAvatarPanel(app, if(player4 == null) "" else player4.name,
    if(player4 == null) List.empty else player4.hand.listOfCards)
  val filler2 = new EmptyFillerPanel
  val filler22 = new EmptyFillerPanel
  val player5AvatarPanel = new ComputerPlayerAvatarPanel(app, if(player5 == null) "" else player5.name,
    if(player5 == null) List.empty else player5.hand.listOfCards)

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
  layout(filler12) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 3
  c.gridy = 0
  layout(player4AvatarPanel) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 4
  c.gridy = 0
  layout(filler2) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 5
  c.gridy = 0
  layout(filler22) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 6
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
      case 2 => player3AvatarPanel.setPlayerAvatarToComplete
      case 3 => player4AvatarPanel.setPlayerAvatarToComplete
      case 4 => player5AvatarPanel.setPlayerAvatarToComplete
      case _ =>
    }
    revalidate()
    repaint()
  }

  def updateRoundObject(newRound: Round): Unit = {
    this.round = newRound
  }

  def updateUserHasPassedOnRound(indexOfPassedPlayer: Int) = {
    indexOfPassedPlayer match {
      case 2 => player3AvatarPanel.updateUserHasPassedOnRound
      case 3 => player4AvatarPanel.updateUserHasPassedOnRound
      case 4 => player5AvatarPanel.updateUserHasPassedOnRound
      case _ =>
    }
    revalidate()
    repaint()
  }

  def resetUserPassStatus = {
    player3AvatarPanel.resetUserPassStatus
    player4AvatarPanel.resetUserPassStatus
    player5AvatarPanel.resetUserPassStatus
    revalidate()
    repaint()
  }

  def updatePlayerHands(players: List[Player]) = {
    if(players.size >= 3) player3AvatarPanel.updatePlayerHand(players(2).hand.listOfCards) else player3AvatarPanel.updatePlayerHand(List.empty)
    if(players.size >= 4) player4AvatarPanel.updatePlayerHand(players(3).hand.listOfCards) else player4AvatarPanel.updatePlayerHand(List.empty)
    if(players.size >= 5) player5AvatarPanel.updatePlayerHand(players(4).hand.listOfCards) else player5AvatarPanel.updatePlayerHand(List.empty)
  }

  def updateLastRemainingPlayer(indexOfLastPlayer: Int) = {
    indexOfLastPlayer match {
      case 2 => player3AvatarPanel.setPlayerAvatarToBum
      case 3 => player4AvatarPanel.setPlayerAvatarToBum
      case 4 => player5AvatarPanel.setPlayerAvatarToBum
      case _ =>
    }
    revalidate()
    repaint()
  }

  def resetPlayerCompletionStatus = {
    player3AvatarPanel.resetUserCompletionStatus
    player4AvatarPanel.resetUserCompletionStatus
    player5AvatarPanel.resetUserCompletionStatus
    revalidate()
    repaint()
  }

}
