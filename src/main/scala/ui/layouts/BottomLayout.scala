package ui.layouts

import game.{Move, Round}
import player.Player
import ui.models.{CardTile, HandCard}
import ui.panels.{PlayerCardTilePanel, PlayerHandPanel, PlayerMoveOptionsPanel}

import scala.swing.{GridBagPanel, SimpleSwingApplication}

/*
This holds the PlayerHandPanel, PlayerCardListPanel, PlayerMoveOptionsPanel
 */
class BottomLayout(app: SimpleSwingApplication, realPlayer: Player, round: Round) extends GridBagPanel {

  val playerCardListPanel = new PlayerCardTilePanel(app, realPlayer, this)
  val playerHandPanel = new PlayerHandPanel(app, realPlayer, this)
  val playerMoveOptionsPanel = new PlayerMoveOptionsPanel(this)

  val c: Constraints = new Constraints()

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 0
  c.gridy = 0
  layout(playerCardListPanel) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 1
  c.gridy = 0
  layout(playerHandPanel) = c

  c.fill = GridBagPanel.Fill.Horizontal
  c.weightx = 1
  c.gridx = 2
  c.gridy = 0
  layout(playerMoveOptionsPanel) = c

  def updateCardSelectStatusViaHandCard(handCardUiList: List[HandCard]) = {
    playerCardListPanel.updateCardSelectStatus(handCardUiList)
  }

  def updateCardSelectStatusViaCardTile(cardTileUiList: List[CardTile]) = {
    playerHandPanel.updateCardSelectStatus(cardTileUiList)
  }

  def highlightPossibleCards() = {
    val p = realPlayer.copy(isRealPlayer = false)
    val nextMove = p.playNextMove(p.hand, Move(List.empty))
    playerHandPanel.setCardsAsSelected(nextMove)
    playerCardListPanel.setCardsAsSelected(nextMove)
  }

}
