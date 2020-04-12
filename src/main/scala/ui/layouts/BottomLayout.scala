package ui.layouts

import game.{Move, Round}
import player.Player
import ui.models.{CardTile, HandCard}
import ui.panels.{PlayerCardTilePanel, PlayerHandPanel, PlayerMoveOptionsPanel}

import scala.swing.{GridBagPanel, SimpleSwingApplication}

/*
This holds the PlayerHandPanel, PlayerCardListPanel, PlayerMoveOptionsPanel
 */
class BottomLayout(app: SimpleSwingApplication, var realPlayer: Player, var round: Round) extends GridBagPanel {

  var playerCardListPanel = new PlayerCardTilePanel(app, realPlayer, this)
  var playerHandPanel = new PlayerHandPanel(app, realPlayer, this)
  val playerMoveOptionsPanel = new PlayerMoveOptionsPanel(this)

  var selectedMove: Option[Move] = None
  var isMoveSelected = false

  val c: Constraints = new Constraints()

  setUpPlayerCardListPanel
  setUpPlayerHandPanel
  setUpPlayerMoveOptionsPanel


  def setUpPlayerCardListPanel = {
    c.fill = GridBagPanel.Fill.Horizontal
    c.weightx = 1
    c.gridx = 0
    c.gridy = 0
    layout(playerCardListPanel) = c
  }

  def setUpPlayerHandPanel = {
    c.fill = GridBagPanel.Fill.Horizontal
    c.weightx = 1
    c.gridx = 1
    c.gridy = 0
    layout(playerHandPanel) = c
  }

  def setUpPlayerMoveOptionsPanel = {
    c.fill = GridBagPanel.Fill.Horizontal
    c.weightx = 1
    c.gridx = 2
    c.gridy = 0
    layout(playerMoveOptionsPanel) = c
  }

  def updateCardSelectStatusViaHandCard(handCardUiList: List[HandCard]) = {
    playerCardListPanel.updateCardSelectStatus(handCardUiList)
  }

  def updateCardSelectStatusViaCardTile(cardTileUiList: List[CardTile]) = {
    playerHandPanel.updateCardSelectStatus(cardTileUiList)
  }

  def updateRoundObject(newRound: Round) = {
    this.round = newRound
  }

  def updateActivePlayerAvatar() = {
    // Do nothing for now
  }

  def updatePlayerCompletion(indexOfCompletedPlayer: Int) = {
    // Do nothing for now
  }

  def highlightPossibleCards() = {
    val p = realPlayer.copy(isRealPlayer = false)
    val nextMove = p.playNextMove(p.hand, round.gameState)
    if(nextMove.isDefined) {
      playerHandPanel.setCardsAsSelected(nextMove)
      playerCardListPanel.setCardsAsSelected(nextMove)
    }
  }

  def updateInternalMoveAsUserPass() = {
    isMoveSelected = true
    selectedMove = None
  }

  def updateInternalMoveUsingSelectedCards() = {
    isMoveSelected = true
    selectedMove = Some(Move(playerCardListPanel.cardTileList.filter(tile => tile.isSelected).map(tile => tile.card)))
  }

  def getUserInputMove(): Option[Move] = {
    while(!isMoveSelected) {
      Thread.sleep(100)
    }
    isMoveSelected = false
    selectedMove
  }

  def updateRealPlayerObject(newPlayer: Player) = {
    this.realPlayer = newPlayer
    playerCardListPanel.updateRealPlayerObject(newPlayer)
    playerHandPanel.updateRealPlayerObject(newPlayer)
    playerHandPanel.revalidate()
    playerHandPanel.repaint()
    playerCardListPanel.revalidate()
    playerCardListPanel.repaint()
    revalidate()
    repaint()
  }

}
