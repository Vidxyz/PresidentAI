package ui.layouts

import game.{GameUtilities, Move, Round}
import player.Player
import ui.models.{CardTile, HandCard}
import ui.panels.{PlayerCardTilePanel, PlayerHandPanel, PlayerMoveOptionsPanel}

import scala.swing.{GridBagPanel, SimpleSwingApplication}

/*
This holds the PlayerHandPanel, PlayerCardListPanel, PlayerMoveOptionsPanel
 */
class BottomLayout(app: SimpleSwingApplication, var realPlayer: Player, var round: Round) extends GridBagPanel {

  var playerCardTilePanel = new PlayerCardTilePanel(app, realPlayer, this)
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
    layout(playerCardTilePanel) = c
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
    playerCardTilePanel.updateCardSelectStatus(handCardUiList)
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
      playerCardTilePanel.setCardsAsSelected(nextMove)
    }
  }

  def updateInternalMoveAsUserPass() = {
    isMoveSelected = true
    selectedMove = None
  }

  //  cannot accept this when no cards are selected, or when invalid move is selected
  def updateInternalMoveUsingSelectedCards: Unit = {
    import scala.swing.Dialog._
    if(!playerCardTilePanel.cardTileList.exists(tile => tile.isSelected)) {
      showMessage(this, "Please select a card(s) to play", "No Cards Selected")
      return
    }

    val unsanitizedMove = Move(playerCardTilePanel.cardTileList.filter(tile => tile.isSelected).map(tile => tile.card))
    selectedMove = Some(GameUtilities.fixWildCardAssumedValueInMove(unsanitizedMove, round.gameState))

    if(GameUtilities.isLegalMove(selectedMove.get) && GameUtilities.isValidMove(selectedMove.get, round.gameState)) isMoveSelected = true
    else {
      showMessage(this, "Please select cards to make a valid move, or pass", "Invalid Move")
      resetSelectionOnCards
    }
  }

  def resetSelectionOnCards: Unit = {
    playerCardTilePanel.resetSelectionOnCards
    playerHandPanel.resetSelectionOnCards
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
    playerCardTilePanel.updateRealPlayerObject(newPlayer)
    playerHandPanel.updateRealPlayerObject(newPlayer)
    playerHandPanel.revalidate()
    playerHandPanel.repaint()
    playerCardTilePanel.revalidate()
    playerCardTilePanel.repaint()
    revalidate()
    repaint()
  }

  def displayUserHasPassedOnRound(indexOfPassedPlayer: Int) = {
    // Do nothing
  }

}